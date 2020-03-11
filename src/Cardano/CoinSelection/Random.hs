{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains an implementation of the __random-improve__ coin
-- selection algorithm.
--
module Cardano.CoinSelection.Random
    ( randomImprove
    ) where

import Prelude

import Cardano.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionAlgorithm (..)
    , CoinSelectionOptions (..)
    , ErrCoinSelection (..)
    )
import Cardano.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Types
    ( Coin (..)
    , TxIn
    , TxOut (..)
    , UTxO (..)
    , balance'
    , distance
    , invariant
    , pickRandom
    )
import Control.Arrow
    ( left )
import Control.Monad
    ( foldM )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), except )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), runMaybeT )
import Crypto.Random.Types
    ( MonadRandom )
import Data.Functor
    ( ($>) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Ord
    ( Down (..) )
import Data.Word
    ( Word64 )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

-- | Random-Improve Algorithm
--
-- 1. Randomly select outputs from the UTxO until the payment value is covered.
--    (In the rare case that this fails because the maximum number of
--    transaction inputs has been exceeded, fall back on the largest-first
--    algorithm for this step.)
--
-- 2. The algorithm first makes a random selection for each output from the
--    UTxO, processing the biggest output first and proceeding in a descending
--    order.  If the selection is not successful largest-first fallback kicks
--    in.  If the selection is successful for each output then the improvement
--    is tried for each selection, once again starting from the selection made
--    for the biggest output. The improvement is tried for the next biggest
--    output's selection. An output is considered an improvement when:
--
--    (a)  It doesn’t exceed a specified upper limit.
--    (b)  Adding the new output gets us closer to the ideal change value.
--    (c)  It doesn’t exceed a maximum number of transaction inputs.
--
-- This algorithm follows three principles:
--
-- @
-- **Self organisation principle 1**
-- Random selection has a high probability of picking dust outputs precisely
-- when there is a lot of dust in the UTxO.
-- @
--
-- @
-- **Self organisation principle 2**
-- If for each payment request for value `x` we create a change output roughly
-- of the same value `x`, then we will end up with a lot of change outputs in
-- our UTxO of size `x` precisely when we have a lot of payment requests of
-- size `x`
-- @
--
-- @
-- **Self organisation principle 3**
-- Searching the UTxO for additional entries to improve our change output is
-- only useful if the UTxO contains entries that are sufficiently small enough.
-- But precisely when the UTxO contains many small entries, it is less likely
-- that a randomly chosen UTxO entry will push the total above the upper bound
-- we set.
-- @
randomImprove :: MonadRandom m => CoinSelectionAlgorithm m e
randomImprove = CoinSelectionAlgorithm payForOutputs

payForOutputs
    :: MonadRandom m
    => CoinSelectionOptions e
    -> NonEmpty TxOut
    -> UTxO
    -> ExceptT (ErrCoinSelection e) m (CoinSelection, UTxO)
payForOutputs options outputsRequested utxo = do
    mRandomSelections <- lift $ runMaybeT $ foldM makeRandomSelection
        (inputCountMax, utxo, []) outputsDescending
    case mRandomSelections of
        Just (inputCountRemaining, utxoRemaining, randomSelections) -> do
            (_, finalSelection, utxoRemaining') <- lift $ foldM
                improveSelection
                    (inputCountRemaining, mempty, utxoRemaining)
                    (reverse randomSelections)
            validateSelection finalSelection $>
                (finalSelection, utxoRemaining')
        Nothing ->
            -- In the case that we fail to generate a selection, delegate to
            -- the "Largest-First" algorithm as a backup.
            selectCoins largestFirst options outputsRequested utxo
  where
    inputCountMax =
        fromIntegral $ maximumInputCount options outputCount
    outputCount =
        fromIntegral $ NE.length outputsRequested
    outputsDescending =
        L.sortOn (Down . coin) $ NE.toList outputsRequested
    validateSelection =
        except . left ErrInvalidSelection . validate options

-- | Randomly select entries from the given UTxO set, until the total value of
--   selected entries is greater than or equal to the given output value.
--
-- Once a random selection has been made that meets the above criterion, this
-- function returns that selection as is, making no attempt to improve upon
-- the selection in any way.
--
makeRandomSelection
    :: MonadRandom m
    => (Word64, UTxO, [([CoinSelectionInput], TxOut)])
    -> TxOut
    -> MaybeT m (Word64, UTxO, [([CoinSelectionInput], TxOut)])
makeRandomSelection
    (inputCountRemaining, utxoRemaining, existingSelections) txout = do
        (utxoSelected, utxoRemaining') <- coverRandomly ([], utxoRemaining)
        return
            ( inputCountRemaining - fromIntegral (L.length utxoSelected)
            , utxoRemaining'
            , (utxoSelected, txout) : existingSelections
            )
  where
    coverRandomly
        :: MonadRandom m
        => ([CoinSelectionInput], UTxO)
        -> MaybeT m ([CoinSelectionInput], UTxO)
    coverRandomly (selected, remaining)
        | L.length selected > fromIntegral inputCountRemaining =
            MaybeT $ return Nothing
        | balance' selected >= targetMin (mkTargetRange txout) =
            MaybeT $ return $ Just (selected, remaining)
        | otherwise =
            pickRandomT remaining >>= \(picked, remaining') ->
                coverRandomly (picked : selected, remaining')

-- | Perform an improvement to random selection on a given output.
improveSelection
    :: MonadRandom m
    => (Word64, CoinSelection, UTxO)
    -> ([CoinSelectionInput], TxOut)
    -> m (Word64, CoinSelection, UTxO)
improveSelection (maxN0, selection, utxo0) (inps0, txout) = do
    (maxN, inps, utxo) <- improve (maxN0, inps0, utxo0)
    return
        ( maxN
        , selection <> CoinSelection
            { inputs = inps
            , outputs = [txout]
            , change = mkChange txout inps
            }
        , utxo
        )
  where
    target = mkTargetRange txout

    improve
        :: MonadRandom m
        => (Word64, [CoinSelectionInput], UTxO)
        -> m (Word64, [CoinSelectionInput], UTxO)
    improve (maxN, inps, utxo)
        | maxN >= 1 && balance' inps < targetAim target = do
            runMaybeT (pickRandomT utxo) >>= \case
                Nothing ->
                    return (maxN, inps, utxo)
                Just (io, utxo') | isImprovement io inps -> do
                    let inps' = io : inps
                    let maxN' = maxN - 1
                    improve (maxN', inps', utxo')
                Just _ ->
                    return (maxN, inps, utxo)
        | otherwise =
            return (maxN, inps, utxo)

    isImprovement :: CoinSelectionInput -> [CoinSelectionInput] -> Bool
    isImprovement io selected =
        let
            condA = -- (a) It doesn’t exceed a specified upper limit.
                balance' (io : selected) < targetMax target

            condB = -- (b) Addition gets us closer to the ideal change
                distance (targetAim target) (balance' (io : selected))
                <
                distance (targetAim target) (balance' selected)

            -- (c) Doesn't exceed maximum number of inputs
            -- Guaranteed by the precondition on 'improve'.
        in
            condA && condB

{-------------------------------------------------------------------------------
                                 Internals
-------------------------------------------------------------------------------}

-- | Represents an entry from a 'UTxO' set that has been selected for inclusion
--   in the set of 'inputs' of a 'CoinSelection'.
type CoinSelectionInput = (TxIn, TxOut)

-- | Represents a target range of /total input values/ for a given output.
--
-- In this context, /total input value/ refers to the total value of a set of
-- inputs selected to pay for a given output.
--
data TargetRange = TargetRange
    { targetMin :: Word64
        -- ^ The minimum value, corresponding to exactly the requested target
        -- amount, and a change amount of zero.
    , targetAim :: Word64
        -- ^ The ideal value, corresponding to exactly twice the requested
        -- target amount, and a change amount equal to the requested amount.
    , targetMax :: Word64
        -- ^ The maximum value, corresponding to exactly three times the
        -- requested amount, and a change amount equal to twice the requested
        -- amount.
    }

-- | Compute the target range of /total input values/ for a given output.
--
-- See 'TargetRange'.
--
mkTargetRange :: TxOut -> TargetRange
mkTargetRange (TxOut _ (Coin c)) = TargetRange
    { targetMin = c
    , targetAim = 2 * c
    , targetMax = 3 * c
    }

-- | Re-wrap 'pickRandom' in a 'MaybeT' monad
pickRandomT :: MonadRandom m => UTxO -> MaybeT m (CoinSelectionInput, UTxO)
pickRandomT =
    MaybeT . fmap (\(m,u) -> (,u) <$> m) . pickRandom

-- | Compute corresponding change outputs from a target output and a selection
-- of inputs.
--
-- > pre-condition: the output must be smaller (or eq) than the sum of inputs
mkChange :: TxOut -> [CoinSelectionInput] -> [Coin]
mkChange (TxOut _ (Coin out)) inps =
    let
        selected = invariant
            "mkChange: output is smaller than selected inputs!"
            (balance' inps)
            (>= out)
        Coin maxCoinValue = maxBound
    in
        case selected - out of
            c | c > maxCoinValue ->
                let h = (c `div` 2) in [Coin h, Coin (c - h)]
            c | c == 0 ->
                []
            c ->
                [ Coin c ]
