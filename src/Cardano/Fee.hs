{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides the API of Coin Selection algorithm and Fee Calculation
-- This module contains the implementation of adjusting coin selection for a
-- fee.  The sender pays for the fee and additional inputs are picked randomly.
-- For more information refer to:
-- https://iohk.io/blog/self-organisation-in-coin-selection/

module Cardano.Fee
    (
      -- * Types
      Fee (..)
    , FeePolicy (..)

      -- * Fee Calculation
    , computeFee
    , distributeFee

      -- * Fee Adjustment
    , FeeOptions (..)
    , ErrAdjustForFee(..)
    , adjustForFee

      -- * Dust Processing
    , coalesceDust

    ) where

import Prelude hiding
    ( round )

import Cardano.CoinSelection
    ( CoinSelection (..), changeBalance, inputBalance, outputBalance )
import Cardano.Types
    ( Coin (..)
    , FeePolicy (..)
    , TxIn
    , TxOut (..)
    , UTxO (..)
    , balance'
    , invariant
    , isValidCoin
    , pickRandom
    )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE )
import Control.Monad.Trans.State
    ( StateT (..), evalStateT )
import Crypto.Random.Types
    ( MonadRandom )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Ord
    ( Down (..), comparing )
import Data.Quantity
    ( Quantity (..) )
import Data.Ratio
    ( (%) )
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Rounding
    ( RoundingDirection (..), round )
import Quiet
    ( Quiet (Quiet) )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

{-------------------------------------------------------------------------------
                                    Types
-------------------------------------------------------------------------------}

-- | A 'Fee', isomorph to 'Coin' but ease type-signatures and readability.
newtype Fee = Fee
    { getFee :: Word64 }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet Fee)

{-------------------------------------------------------------------------------
                                Fee Calculation
-------------------------------------------------------------------------------}

-- | Compute fee for a given payload. Fee follows a simple linear
-- equation:
--
-- @
--     f = a + size * b
-- @
--
-- where @a@ & @b@ are values fixed by the protocol.
computeFee
    :: FeePolicy
    -> Quantity "byte" Int
    -> Fee
computeFee policy (Quantity sz) =
    Fee $ ceiling (a + b*fromIntegral sz)
  where
    LinearFee (Quantity a) (Quantity b) (Quantity _c) = policy

{-------------------------------------------------------------------------------
                                Fee Adjustment
-------------------------------------------------------------------------------}

data FeeOptions = FeeOptions
    { estimateFee
      :: CoinSelection -> Fee
      -- ^ Estimate fees based on number of inputs and values of the outputs
      -- Some pointers / order of magnitude from the current configuration:
      --     a: 155381 # absolute minimal fees per transaction
      --     b: 43.946 # additional minimal fees per byte of transaction size
    , dustThreshold
      :: Coin
      -- ^ Defines the maximum size of a dust coin.
      --
      -- Change values that are less than or equal to this threshold will be
      -- evicted from created transactions.
      --
    } deriving (Generic)

newtype ErrAdjustForFee
    = ErrCannotCoverFee Word64
    -- ^ UTxO exhausted during fee covering
    -- We record what amount missed to cover the fee
    deriving (Show, Eq)

-- | Given the coin selection result from a policy run, adjust the outputs for
-- fees, potentially returning additional inputs that we need to cover all
-- fees.
--
-- We lose the relationship between the transaction outputs and their
-- corresponding inputs/change outputs here. This is a decision we may wish to
-- revisit later. For now however note that since
--
--  (a) coin selection tries to establish a particular ratio between
--      payment outputs and change outputs (currently it aims for an average of
--      1:1)
--
--  (b) coin selection currently only generates a single change output per
--      payment output, distributing the fee proportionally across all change
--      outputs is roughly equivalent to distributing it proportionally over
--      the payment outputs (roughly, not exactly, because the 1:1 proportion
--      is best effort only, and may in some cases be wildly different).
--
-- Note that for (a) we don't need the ratio to be 1:1, the above reasoning
-- will remain true for any proportion 1:n. For (b) however, if coin selection
-- starts creating multiple outputs, and this number may vary, then losing the
-- connection between outputs and change outputs will mean that that some
-- outputs may pay a larger percentage of the fee (depending on how many change
-- outputs the algorithm happened to choose).
--
adjustForFee
    :: MonadRandom m
    => FeeOptions
    -> UTxO
    -> CoinSelection
    -> ExceptT ErrAdjustForFee m CoinSelection
adjustForFee unsafeOpt utxo coinSel = do
    let opt = invariant
            "adjustForFee: fee must be non-null" unsafeOpt (not . nullFee)
    senderPaysFee opt utxo coinSel
  where
    nullFee opt = estimateFee opt coinSel == Fee 0

-- | The sender pays fee in this scenario, so fees are removed from the change
-- outputs, and new inputs are selected if necessary.
senderPaysFee
    :: MonadRandom m
    => FeeOptions
    -> UTxO
    -> CoinSelection
    -> ExceptT ErrAdjustForFee m CoinSelection
senderPaysFee opt utxo sel = evalStateT (go sel) utxo where
    go
        :: MonadRandom m
        => CoinSelection
        -> StateT UTxO (ExceptT ErrAdjustForFee m) CoinSelection
    go coinSel@(CoinSelection inps outs chgs) = do
        -- 1/
        -- We compute fees using all inputs, outputs and changes since all of
        -- them have an influence on the fee calculation.
        let upperBound = estimateFee opt coinSel
        -- 2/
        -- Substract fee from change outputs, proportionally to their value.
        let coinSel' = CoinSelection
                { inputs = inps
                , outputs = outs
                , change = rebalanceChangeOutputs opt upperBound chgs
                }
        let remFee = remainingFee opt coinSel'
        -- 3.1/
        -- Should the change cover the fee, we're (almost) good. By removing
        -- change outputs, we make them smaller and may reduce the size of the
        -- transaction, and the fee. Thus, we end up paying slightly more than
        -- the upper bound. We could do some binary search and try to
        -- re-distribute excess across changes until fee becomes bigger.
        if remFee == Fee 0
        then pure coinSel'
        else do
            -- 3.2/
            -- Otherwise, we need an extra entries from the available utxo to
            -- cover what's left. Note that this entry may increase our change
            -- because we may not consume it entirely. So we will just split
            -- the extra change across all changes possibly increasing the
            -- number of change outputs (if there was none, or if increasing a
            -- change value causes an overflow).
            --
            -- Because selecting a new input increases the fee, we need to
            -- re-run the algorithm with this new elements and using the initial
            -- change plus the extra change brought up by this entry and see if
            -- we can now correctly cover fee.
            inps' <- coverRemainingFee remFee
            let extraChange = splitChange (Coin $ balance' inps') chgs
            go $ CoinSelection (inps <> inps') outs extraChange

-- | A short / simple version of the 'random' fee policy to cover for fee in
-- case where existing change were not enough.
coverRemainingFee
    :: MonadRandom m
    => Fee
    -> StateT UTxO (ExceptT ErrAdjustForFee m) [(TxIn, TxOut)]
coverRemainingFee (Fee fee) = go [] where
    go acc
        | balance' acc >= fee =
            return acc
        | otherwise = do
            -- We ignore the size of the fee, and just pick randomly
            StateT (lift . pickRandom) >>= \case
                Just entry ->
                    go (entry : acc)
                Nothing -> do
                    lift $ throwE $ ErrCannotCoverFee (fee - balance' acc)

-- | Reduce the given change outputs by the total fee, returning the remainig
-- change outputs if any are left, or the remaining fee otherwise
--
-- We divvy up the fee over all change outputs proportionally, to try and keep
-- any output:change ratio as unchanged as possible
rebalanceChangeOutputs :: FeeOptions -> Fee -> [Coin] -> [Coin]
rebalanceChangeOutputs opt totalFee chgs =
    case coalesceDust (Coin 0) chgs of
        [] -> []
        x : xs ->
            coalesceDust (dustThreshold opt)
            $ map reduceSingleChange
            $ F.toList
            $ distributeFee totalFee
            $ x :| xs

-- | Reduce single change output by a given fee amount. If fees are too big for
-- a single coin, returns a `Coin 0`.
reduceSingleChange :: (Fee, Coin) -> Coin
reduceSingleChange (Fee fee, Coin chng)
    | chng >= fee =
          Coin (chng - fee)
    | otherwise =
          Coin 0

-- | Distribute the given fee over the given list of coins, so that each coin
--   is allocated a __fraction__ of the fee in proportion to its relative size.
--
-- == Pre-condition
--
-- Every coin in the given list must be __non-zero__ in value.
--
-- == Examples
--
-- >>> distributeFee (Fee 2) [(Coin 1), (Coin 1)]
-- [(Fee 1, Coin 1), (Fee 1, Coin 1)]
--
-- >>> distributeFee (Fee 4) [(Coin 1), (Coin 1)]
-- [(Fee 2, Coin 1), (Fee 2, Coin 1)]
--
-- >>> distributeFee (Fee 7) [(Coin 1), (Coin 2), (Coin 4)]
-- [(Fee 1, Coin 1), (Fee 2, Coin 2), (Fee 4, Coin 4)]
--
-- >>> distributeFee (Fee 14) [(Coin 1), (Coin 2), (Coin 4)]
-- [(Fee 2, Coin 1), (Fee 4, Coin 2), (Fee 8, Coin 4)]
--
distributeFee :: Fee -> NonEmpty Coin -> NonEmpty (Fee, Coin)
distributeFee (Fee feeTotal) coinsUnsafe =
    NE.zip feesRounded coins
  where
    -- A list of coins that are non-zero in value.
    coins :: NonEmpty Coin
    coins =
        invariant "distributeFee: all coins must be non-zero in value."
        coinsUnsafe (Coin 0 `F.notElem`)

    -- A list of rounded fee portions, where each fee portion deviates from the
    -- ideal unrounded portion by as small an amount as possible.
    feesRounded :: NonEmpty Fee
    feesRounded
        -- 1. Start with the list of ideal unrounded fee portions for each coin:
        = feesUnrounded
        -- 2. Attach an index to each fee portion, so that we can remember the
        --    original order:
        & NE.zip indices
        -- 3. Sort the fees into descending order of their fractional parts:
        & NE.sortBy (comparing (Down . fractionalPart . snd))
        -- 4. Apply pre-computed roundings to each fee portion:
        --    * portions with the greatest fractional parts are rounded up;
        --    * portions with the smallest fractional parts are rounded down.
        & NE.zipWith (\roundDir (i, f) -> (i, round roundDir f)) feeRoundings
        -- 5. Restore the original order:
        & NE.sortBy (comparing fst)
        -- 6. Strip away the indices:
        & fmap (Fee . fromIntegral @Integer . snd)
      where
        indices :: NonEmpty Int
        indices = 0 :| [1 ..]

    -- A list of rounding directions, one per fee portion.
    --
    -- Since the ideal fee portion for each coin is a rational value, we must
    -- therefore round each rational value either /up/ or /down/ to produce a
    -- final integer result.
    --
    -- However, we can't take the simple approach of either rounding /all/ fee
    -- portions down or rounding /all/ fee portions up, as this could cause the
    -- sum of fee portions to either undershoot or overshoot the original fee.
    --
    -- So in order to hit the fee exactly, we must round /some/ of the portions
    -- up, and /some/ of the portions down.
    --
    -- Fortunately, we can calculate exactly how many fee portions must be
    -- rounded up, by first rounding /all/ portions down, and then computing
    -- the /shortfall/ between the sum of the rounded-down portions and the
    -- original fee.
    --
    -- We return a list where all values of 'RoundUp' occur in a contiguous
    -- section at the start of the list, of the following form:
    --
    --     [RoundUp, RoundUp, ..., RoundDown, RoundDown, ...]
    --
    feeRoundings :: NonEmpty RoundingDirection
    feeRoundings =
        applyN feeShortfall (NE.cons RoundUp) (NE.repeat RoundDown)
      where
         -- The part of the total fee that we'd lose if we were to take the
         -- simple approach of rounding all ideal fee portions /down/.
        feeShortfall
            = fromIntegral feeTotal
            - fromIntegral @Integer (F.sum $ round RoundDown <$> feesUnrounded)

    -- A list of ideal unrounded fee portions, with one fee portion per coin.
    --
    -- A coin's ideal fee portion is the rational portion of the total fee that
    -- corresponds to that coin's relative size when compared to other coins.
    feesUnrounded :: NonEmpty Rational
    feesUnrounded = calculateIdealFee <$> coins
      where
        calculateIdealFee (Coin c)
            = fromIntegral c
            * fromIntegral feeTotal
            % fromIntegral (getCoin totalCoinValue)

    -- The total value of all coins.
    totalCoinValue :: Coin
    totalCoinValue = Coin $ F.sum $ getCoin <$> coins

-- | From the given list of coins, remove dust coins with a value less than or
--   equal to the given threshold value, redistributing their total value over
--   the coins that remain.
--
-- This function satisfies the following properties:
--
-- >>> sum coins = sum (coalesceDust threshold coins)
-- >>> all (/= Coin 0) (coalesceDust threshold coins)
--
coalesceDust :: Coin -> [Coin] -> [Coin]
coalesceDust threshold coins =
    splitChange valueToDistribute coinsToKeep
  where
    (coinsToKeep, coinsToRemove) = L.partition (> threshold) coins
    valueToDistribute = Coin $ sum $ getCoin <$> coinsToRemove

-- | Computes how much is left to pay given a particular selection
remainingFee
    :: HasCallStack
    => FeeOptions
    -> CoinSelection
    -> Fee
remainingFee opts s = do
    if fee >= diff
    then Fee (fee - diff)
    else do
        -- NOTE
        -- The only case where we may end up with an unbalanced transaction is
        -- when we have a dangling change output (i.e. adding it costs too much
        -- and we can't afford it, but not having it result in too many coins
        -- left for fees).
        let Fee feeDangling =
                estimateFee opts $ s { change = [Coin (diff - fee)] }
        if (feeDangling >= diff)
            then Fee (feeDangling - fee)
            else error $ unwords
                [ "Generated an unbalanced tx! Too much left for fees"
                , ": fee (raw) =", show fee
                , ": fee (dangling) =", show feeDangling
                , ", diff =", show diff
                , "\nselection =", pretty s
                ]
  where
    Fee fee = estimateFee opts s
    diff = inputBalance s - (outputBalance s + changeBalance s)

-- Equally split the extra change obtained when picking new inputs across all
-- other change. Note that, it may create an extra change output if:
--
--   (a) There's no change at all initially
--   (b) Adding change to an exiting one would cause an overflow
--
-- It makes no attempt to divvy the new output proportionally over the change
-- outputs. This means that if we happen to pick a very large UTxO entry,
-- adding this evenly rather than proportionally might skew the payment:change
-- ratio a lot. Could consider defining this in terms of divvy instead.
splitChange :: Coin -> [Coin] -> [Coin]
splitChange = go
  where
    go remaining as | remaining == Coin 0 = as
    go remaining [] = [remaining]
        -- we only create new change if for whatever reason there is none
        -- already or if is some overflow happens when we try to add.
    go remaining [a] =
        let
            newChange = Coin $ (getCoin remaining) + (getCoin a)
        in
            if isValidCoin newChange
            then [newChange]
            else [a, remaining]
    go rest@(Coin remaining) ls@(a : as) =
        let
            piece = remaining `div` fromIntegral (length ls)
            newRemaining = Coin (remaining - piece)
            newChange = Coin (piece + getCoin a)
        in
            if isValidCoin newChange
            then newChange : go newRemaining as
            else a : go rest as

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Extract the fractional part of a rational number.
--
-- Examples:
--
-- >>> fractionalPart (3 % 2)
-- 1 % 2
--
-- >>> fractionalPart (11 % 10)
-- 1 % 10
--
fractionalPart :: Rational -> Rational
fractionalPart = snd . properFraction @_ @Integer

-- | Apply the same function multiple times to a value.
--
applyN :: Int -> (a -> a) -> a -> a
applyN n f = F.foldr (.) id (replicate n f)
