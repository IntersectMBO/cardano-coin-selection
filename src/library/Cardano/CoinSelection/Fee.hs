{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides functionality for __adjusting__ coin selections in order to pay for
-- transaction __fees__.
--
module Cardano.CoinSelection.Fee
    (
      -- * Fundamental Types
      Fee (..)
    , FeeEstimator (..)

      -- * Fee Adjustment
    , adjustForFee
    , FeeOptions (..)
    , FeeBalancingPolicy (..)
    , FeeAdjustmentError (..)

      -- * Dust Processing
    , DustThreshold (..)
    , coalesceDust

      -- # Internal Functions
    , calculateFee
    , distributeFee
    , reduceChangeOutputs
    , splitCoin

    ) where

import Prelude hiding
    ( round )

import Cardano.CoinSelection
    ( CoinMap (..)
    , CoinMapEntry (..)
    , CoinSelection (..)
    , coinMapFromList
    , coinMapRandomEntry
    , sumChange
    , sumInputs
    , sumOutputs
    )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, throwE )
import Control.Monad.Trans.State
    ( StateT (..), evalStateT )
import Crypto.Random.Types
    ( MonadRandom )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Maybe
    ( fromMaybe )
import Data.Ord
    ( Down (..), comparing )
import Data.Ratio
    ( (%) )
import GHC.Generics
    ( Generic )
import Internal.Coin
    ( Coin )
import Internal.Invariant
    ( invariant )
import Internal.Rounding
    ( RoundingDirection (..), round )
import Quiet
    ( Quiet (Quiet) )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Internal.Coin as C

--------------------------------------------------------------------------------
-- Fundamental Types
--------------------------------------------------------------------------------

-- | Represents a non-negative fee to be paid on a transaction.
--
newtype Fee = Fee { unFee :: Coin }
    deriving newtype (Monoid, Semigroup)
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet Fee)

-- | Defines the /maximum/ size of a __dust coin__.
--
-- Functions that accept a 'DustThreshold' argument will generally not include
-- values that are /less than or equal to/ this threshold in the 'change' sets
-- of generated selections, /coalescing/ such coins together into larger
-- coins that /exceed/ the threshold.
--
-- Specifying a dust threshold of __/n/__ causes all coins that are less than
-- or equal to __/n/__ to be treated as dust and coalesced together.
--
-- Specifying a dust threshold of __0__ completely /disables/ dust elimination
-- with the exception of zero-valued coins, which will always be eliminated.
--
-- See 'coalesceDust'.
--
newtype DustThreshold = DustThreshold { unDustThreshold :: Coin }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet DustThreshold)

-- | Provides a function capable of __estimating__ the transaction fee required
--   for a given coin selection, according to the rules of a particular
--   blockchain.
--
-- The fee estimate should be a function of the __current__ memberships of the
-- 'inputs', 'outputs', and 'change' sets.
--
-- Depending on the rules of the blockchain under consideration, the fee
-- estimate may take either (or both) of the following factors into account:
--
--   - the number of entries in each set;
--   - the coin value of each set member.
--
-- A fee estimate may differ from the final fee required for a selection, as
-- fees are generally paid for by /adjusting/ a given selection to make a /new/
-- selection. See 'adjustForFee' for more details of this process.
--
newtype FeeEstimator i o = FeeEstimator
    { estimateFee :: CoinSelection i o -> Fee
    } deriving Generic

--------------------------------------------------------------------------------
-- Fee Adjustment
--------------------------------------------------------------------------------

-- | Provides options for fee adjustment.
--
data FeeOptions i o = FeeOptions
    { feeEstimator
        :: FeeEstimator i o
        -- ^ Estimate fees based on selected inputs and requested outputs.

    , dustThreshold
        :: DustThreshold
        -- ^ The threshold to use for dust elimination. Specifying a threshold
        -- of zero will disable dust elimination. See 'DustThreshold' for more
        -- details.

    , feeBalancingPolicy
        :: FeeBalancingPolicy
        -- ^ What do to when we encounter a dangling change output.
        -- See 'FeeBalancingPolicy'
    } deriving Generic

-- | A choice of fee balancing policies for use when adjusting a coin selection.
--
-- == Background
--
-- A coin selection __'s'__ is said to have a /perfectly-balanced/ fee when it
-- satisfies the following property:
--
-- >>> sumInputs s = sumOutputs s + sumChange s + estimateFee s
--
-- Conversely, a selection is said to have an /unbalanced/ fee when it
-- satisfies the following property:
--
-- >>> sumInputs s > sumOutputs s + sumChange s + estimateFee s
--
-- In other words, if a coin selection has an /unbalanced/ fee, the /effective/
-- fee is greater than the minimum fee /actually required/ by the blockchain.
--
-- == Balanced Fees vs Minimal Fees
--
-- Some blockchains /require /that fees are always /perfectly-balanced/.
--
-- However, for blockchains that allow /unbalanced/ fees, it is sometimes
-- possible to /save money/ by generating a coin selection with an unbalanced
-- fee. This may seem counterintuitive at first, but consider an individual
-- change ouput __/c/__ of value __/v/__. If the /marginal fee/ __/f/__
-- associated with __/c/__ is greater than its value __/v/__, then we will
-- /save money/ by __not__ including __/c/__ within 'change'.
--
-- There are two policy choices available for handling change values with
-- marginal fees greater than their value:
--
--   - For blockchains that __allow__ transactions with /unbalanced/ fees,
--     specifying the 'RequireMinimalFee' policy will allow money to be saved by
--     /excluding/ change outputs that have a marginal fee greater than
--     their value.
--
--   - For blockchains that do __not__ allow transactions with /unbalanced/
--     fees, specifying the 'RequireBalancedFee' policy will always generate
--     selections with fees that are perfectly-balanced, even if the resulting
--     fees are higher than could be achieved by allowing unbalanced fees.
--
data FeeBalancingPolicy
    = RequireBalancedFee
        -- ^ Generate selections with fees that are perfectly balanced, with the
        -- trade-off of allowing slightly higher fees.
    | RequireMinimalFee
        -- ^ Generate selections with the lowest fees possible, with the
        -- trade-off of allowing slightly imbalanced fees.
    deriving (Generic, Show, Eq)

-- | Represents the set of possible failures that can occur when adjusting a
--   'CoinSelection' with the 'adjustForFee' function.
--
data FeeAdjustmentError i o
    = CannotCoverFee Fee
    -- ^ Indicates that the given map of additional inputs was exhausted while
    --   attempting to select extra inputs to cover the required fee.
    --
    -- Records the shortfall (__/f/__ − __/s/__) between the required fee
    -- __/f/__ and the total value __/s/__ of currently-selected inputs.

    | CoinSelectionUnderfunded (CoinSelection i o)
    -- ^ Indicates that the given coin selection is __underfunded__: the total
    -- value of 'inputs' is less than the total value of 'outputs', as
    -- calculated by the 'CoinSelection.coinMapValue' function.
    deriving (Show, Eq)

-- | Adjusts the given 'CoinSelection' in order to pay for a __transaction__
--   __fee__, required in order to publish the selection as a transaction on
--   a blockchain.
--
-- == Background
--
-- Implementations of 'Cardano.CoinSelection.CoinSelectionAlgorithm' generally
-- produce coin selections that are /exactly balanced/, satisfying the
-- following equality:
--
-- >>> sumInputs s = sumOutputs s + sumChange s
--
-- In order to pay for a transaction fee, the above equality must be
-- transformed into an /inequality/:
--
-- >>> sumInputs s > sumOutputs s + sumChange s
--
-- The difference between these two sides represents value to be paid /by the/
-- /originator/ of the transaction, in the form of a fee:
--
-- >>> sumInputs s = sumOutputs s + sumChange s + fee
--
-- == The Adjustment Process
--
-- In order to generate a fee that is acceptable to the network, this function
-- adjusts the 'change' and 'inputs' of the given 'CoinSelection', consulting
-- the 'FeeEstimator' as a guide for how much the current selection would cost
-- to publish as a transaction on the network.
--
-- == Methods of Adjustment
--
-- There are two methods of adjustment possible:
--
--  1. The __'change'__ set can be /reduced/, either by:
--
--      a. completely removing a change value from the set; or by
--
--      b. reducing a change value to a lower value.
--
--  2. The __'inputs'__ set can be /augmented/, by selecting additional inputs
--     from the specified 'CoinMap' argument.
--
-- == Dealing with Dust Values
--
-- If, at any point, a change value is generated that is less than or equal
-- to the 'DustThreshold', this function will eliminate that change value
-- from the 'change' set, redistributing the eliminated value over the remaining
-- change values, ensuring that the total value of all 'change' is preserved.
--
-- See 'coalesceDust' for more details.
--
-- == Termination
--
-- Since adjusting a selection can affect the fee estimate produced by
-- 'estimateFee', the process of adjustment is an /iterative/ process.
--
-- The termination post-condition depends on the choice of
-- 'FeeBalancingPolicy':
--
--   - If 'RequireBalancedFee' is specified, this function terminates
--     only when it has generated a 'CoinSelection' __'s'__ that satisfies the
--     following property:
--
--         >>> sumInputs s = sumOutputs s + sumChange s + estimateFee s
--
--   - If 'RequireMinimalFee' policy is specified, the above /equality/
--     is relaxed to the following /inequality/:
--
--         >>> sumInputs s ≥ sumOutputs s + sumChange s + estimateFee s
--
-- See 'FeeBalancingPolicy' for more details.
--
adjustForFee
    :: (Ord i, MonadRandom m)
    => FeeOptions i o
    -> CoinMap i
    -> CoinSelection i o
    -> ExceptT (FeeAdjustmentError i o) m (CoinSelection i o)
adjustForFee unsafeOpt utxo coinSel = do
    let opt = invariant
            "adjustForFee: fee must be non-null" unsafeOpt (not . nullFee)
    senderPaysFee opt utxo coinSel
  where
    nullFee opt = estimateFee (feeEstimator opt) coinSel == Fee C.zero

--------------------------------------------------------------------------------
-- Internal Functions
--------------------------------------------------------------------------------

-- Calculates the current fee associated with a given 'CoinSelection'.
--
-- If the result is less than zero, returns 'Nothing'.
--
calculateFee :: CoinSelection i o -> Maybe Fee
calculateFee s = Fee <$> sumInputs s `C.sub` (sumOutputs s `C.add` sumChange s)

-- The sender pays fee in this scenario, so fees are removed from the change
-- outputs, and new inputs are selected if necessary.
--
senderPaysFee
    :: forall i o m . (Ord i, MonadRandom m)
    => FeeOptions i o
    -> CoinMap i
    -> CoinSelection i o
    -> ExceptT (FeeAdjustmentError i o) m (CoinSelection i o)
senderPaysFee opts utxo sel =
    evalStateT (go sel) utxo
  where
    go
        :: CoinSelection i o
        -> StateT
            (CoinMap i)
            (ExceptT (FeeAdjustmentError i o) m)
            (CoinSelection i o)
    go coinSel@(CoinSelection inps outs chgs) = do
        -- Substract fee from change outputs, proportionally to their value.
        (coinSel', remFee) <- lift $ except $ reduceChangeOutputs opts coinSel

        -- Should the change cover the fee, we're (almost) good. By removing
        -- change outputs, we make them smaller and may reduce the size of the
        -- transaction, and the fee. Thus, we end up paying slightly more than
        -- the upper bound. We could do some binary search and try to
        -- re-distribute excess across changes until fee becomes bigger.
        if remFee == Fee C.zero
        then pure coinSel'
        else do
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
            let extraChange = splitCoin (sumEntries inps') chgs
            go $ CoinSelection (inps <> coinMapFromList inps') outs extraChange

-- A short and simple version of the 'random' fee policy to cover for the fee
-- in the case where existing set of change is not enough.
--
coverRemainingFee
    :: MonadRandom m
    => Fee
    -> StateT (CoinMap i) (ExceptT (FeeAdjustmentError i o) m) [CoinMapEntry i]
coverRemainingFee (Fee fee) = go [] where
    go acc
        | sumEntries acc >= fee =
            return acc
        | otherwise = do
            -- We ignore the size of the fee, and just pick randomly
            StateT (lift . coinMapRandomEntry) >>= \case
                Just entry ->
                    go (entry : acc)
                Nothing ->
                    lift $ throwE $ CannotCoverFee $ Fee $
                        fee `C.distance` (sumEntries acc)

-- Pays for the given fee by subtracting it from the given list of change
-- outputs, so that each change output is reduced by a portion of the fee
-- that's in proportion to its relative size.
--
-- == Basic Examples
--
-- >>> reduceChangeOutputs (DustThreshold 0) (Fee 4) (Coin <$> [2, 2, 2, 2])
-- [Coin 1, Coin 1, Coin 1, Coin 1]
--
-- >>> reduceChangeOutputs (DustThreshold 0) (Fee 15) (Coin <$> [2, 4, 8, 16])
-- [Coin 1, Coin 2, Coin 4, Coin 8]
--
-- == Handling Dust
--
-- Any dust outputs in the resulting list are coalesced according to the given
-- dust threshold: (See 'coalesceDust'.)
--
-- >>> reduceChangeOutputs (DustThreshold 1) (Fee 4) (Coin <$> [2, 2, 2, 2])
-- [Coin 4]
--
-- == Handling Insufficient Change
--
-- If there's not enough change to pay for the fee, or if there's only just
-- enough to pay for it exactly, this function returns the /empty list/:
--
-- >>> reduceChangeOutputs (DustThreshold 0) (Fee 15) (Coin <$> [10])
-- []
--
-- >>> reduceChangeOutputs (DustThreshold 0) (Fee 15) (Coin <$> [1, 2, 4, 8])
-- []
--
reduceChangeOutputs
    :: FeeOptions i o
    -> CoinSelection i o
    -> Either (FeeAdjustmentError i o) (CoinSelection i o, Fee)
reduceChangeOutputs opts s = do
    -- The original requested fee amount
    let Fee φ_original = estimateFee (feeEstimator opts) s
    -- The initial amount left for fee (i.e. inputs - outputs)
    let mδ_original = sumInputs s `C.sub` (sumOutputs s `C.add` sumChange s)
    case mδ_original of
        -- selection is now balanced, nothing to do.
        Just δ_original | φ_original == δ_original -> do
            pure (s, Fee C.zero)

        -- some fee left to pay, but we've depleted all change outputs
        Just δ_original | φ_original > δ_original && null (change s) -> do
            let remainder = φ_original `C.distance` δ_original
            pure (s, Fee remainder)

        -- some fee left to pay, and we've haven't depleted all change yet
        Just δ_original | φ_original > δ_original && not (null (change s)) -> do
            let remainder = φ_original `C.distance` δ_original
            let chgs' = distributeFee (Fee remainder) (NE.fromList (change s))
                      & fmap payFee
                      & coalesceDust (dustThreshold opts)
            reduceChangeOutputs opts (s { change = chgs' })

        -- The current selection has a higher fee than necessary. This typically
        -- occurs if, after reducing an output to pay for the predicted fee, the
        -- required fee turns out to be less than originally predicted.
        -- The outcome depends on whether or not the node allows transactions
        -- to be unbalanced.
        Just δ_original | δ_original > φ_original -> do
            let extraChg = δ_original `C.distance` φ_original
            let sDangling = s { change = splitCoin extraChg (change s) }
            let Fee φ_dangling = estimateFee (feeEstimator opts) sDangling
            -- We have `δ_dangling = φ_original` by construction of sDangling.
            --
            -- Proof:
            --
            -- δ_dangling = Σi_dangling - (Σo_dangling + Σc_dangling)
            --            = Σi_original - (Σo_original + Σc_original + extraChg)
            --            = Σi_original - (Σo_original + Σc_original) - extraChg
            --            = δ_original - extraChg
            --            = δ_original - (δ_original - φ_original)
            --            = φ_original
            let δ_dangling = φ_original
            case φ_dangling `C.sub` δ_dangling of
                -- we've left too much, but adding a change output would be more
                -- expensive than not having it. Here we have two choices:
                --
                -- a) If the node allows unbalanced transaction, we can stop
                --    here and do nothing. We'll leave slightly more than what's
                --    needed for fees, but having an extra change output isn't
                --    worth it anyway.
                --
                -- b) If we __must__ balance the transaction, then we can choose
                --    to pay the extra cost by adding the change output and
                --    continue trying to balance the transaction (likely, by
                --    selecting another input).
                Just remainder | φ_dangling >= δ_original ->
                    case feeBalancingPolicy opts of
                        RequireMinimalFee ->
                            pure (s, Fee C.zero)
                        RequireBalancedFee ->
                            pure (sDangling, Fee remainder)

                -- If however, adding the dangling change doesn't cost more than
                -- not having it, we might as well add it to get the money and
                -- continue balancing!
                _otherwise ->
                    reduceChangeOutputs opts sDangling

        -- The only way to end-up here is if the user has provided an invalid
        -- selection where outputs are trying to spend more than inputs. This is
        -- simply forbidden.
        _Nothing ->
            Left (CoinSelectionUnderfunded s)

-- Distribute the given fee over the given list of coins, so that each coin
-- is allocated a __fraction__ of the fee in proportion to its relative size.
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
        coinsUnsafe (C.zero `F.notElem`)

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
        & fmap snd
        -- 7. Transform results into fees:
        & fmap (Fee . fromMaybe C.zero . C.coinFromIntegral @Integer)
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
            = C.coinToIntegral feeTotal
            - fromIntegral @Integer (F.sum $ round RoundDown <$> feesUnrounded)

    -- A list of ideal unrounded fee portions, with one fee portion per coin.
    --
    -- A coin's ideal fee portion is the rational portion of the total fee that
    -- corresponds to that coin's relative size when compared to other coins.
    feesUnrounded :: NonEmpty Rational
    feesUnrounded = calculateIdealFee <$> coins
      where
        calculateIdealFee c
            = C.coinToIntegral c
            * C.coinToIntegral feeTotal
            % C.coinToIntegral totalCoinValue

    -- The total value of all coins.
    totalCoinValue :: Coin
    totalCoinValue = F.fold coins

-- | From the given list of coins, remove dust coins with a value less than or
--   equal to the given threshold value, redistributing their total value over
--   the coins that remain.
--
-- This function satisfies the following properties:
--
-- >>> sum coins = sum (coalesceDust threshold coins)
-- >>> all (/= Coin 0) (coalesceDust threshold coins)
--
coalesceDust :: DustThreshold -> NonEmpty Coin -> [Coin]
coalesceDust (DustThreshold threshold) coins =
    splitCoin valueToDistribute coinsToKeep
  where
    (coinsToKeep, coinsToRemove) = NE.partition (> threshold) coins
    valueToDistribute = F.fold coinsToRemove

-- Splits up the given coin of value __@v@__, distributing its value over the
-- given coin list of length __@n@__, so that each coin value is increased by
-- an integral amount within unity of __@v/n@__, producing a new list of coin
-- values where the overall total is preserved.
--
-- == Basic Examples
--
-- When it's possible to divide a coin evenly, each coin value is increased by
-- the same integer amount:
--
-- >>> splitCoin (Coin 40) (Coin <$> [1, 1, 1, 1])
-- [Coin 11, Coin 11, Coin 11, Coin 11]
--
-- >>> splitCoin (Coin 40) (Coin <$> [1, 2, 3, 4])
-- [Coin 11, Coin 12, Coin 13, Coin 14]
--
-- == Handling Non-Uniform Increases
--
-- When it's not possible to divide a coin evenly, each integral coin value in
-- the resulting list is always within unity of the ideal unrounded result:
--
-- >>> splitCoin (Coin 2) (Coin <$> [1, 1, 1, 1])
-- [Coin 1, Coin 1, Coin 2, Coin 2]
--
-- >>> splitCoin (Coin 10) (Coin <$> [1, 1, 1, 1])
-- [Coin 3, Coin 3, Coin 4, Coin 4]
--
-- == Handling Empty Lists
--
-- If the given list is empty, this function returns a list with the original
-- given coin as its sole element:
--
-- >>> splitCoin (Coin 10) []
-- [Coin 10]
--
-- == Properties
--
-- The total value is always preserved:
--
-- >>> sum (splitCoin x ys) == x + sum ys
--
splitCoin :: Coin -> [Coin] -> [Coin]
splitCoin coinToSplit coinsToIncrease =
    case (mIncrement, mShortfall) of
        (Just increment, Just shortfall) ->
            zipWith C.add coinsToIncrease increments
          where
            increments = zipWith C.add majorIncrements minorIncrements
            majorIncrements = repeat increment
            minorIncrements = replicate (C.coinToIntegral shortfall) C.one
                <> repeat C.zero
        _ | coinToSplit > C.zero ->
            [coinToSplit]
        _ ->
            []
  where
    mCoinCount = length coinsToIncrease
    mIncrement = coinToSplit `C.div` mCoinCount
    mShortfall = coinToSplit `C.mod` mCoinCount

-- Extract the fractional part of a rational number.
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

-- Apply the same function multiple times to a value.
--
applyN :: Int -> (a -> a) -> a -> a
applyN n f = F.foldr (.) id (replicate n f)

-- Find the sum of a list of entries.
--
sumEntries :: [CoinMapEntry i] -> Coin
sumEntries = F.fold . fmap entryValue

-- | Reduce a coin value by a given fee amount. If fees are too big for
-- a single coin, returns a `Coin 0`.
payFee :: (Fee, Coin) -> Coin
payFee (Fee f, c) = fromMaybe C.zero (c `C.sub` f)
