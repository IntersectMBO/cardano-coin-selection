{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains an implementation of the __Random-Improve__ coin
-- selection algorithm.
--
module Cardano.CoinSelection.RandomImprove
    ( randomImprove
    ) where

import Prelude

import Cardano.CoinSelection
    ( CoinMap (..)
    , CoinMapEntry (..)
    , CoinSelection (..)
    , CoinSelectionAlgorithm (..)
    , CoinSelectionError (..)
    , CoinSelectionLimit (..)
    , CoinSelectionParameters (..)
    , CoinSelectionResult (..)
    , coinMapFromList
    , coinMapRandomEntry
    , coinMapToList
    , coinMapValue
    )
import Control.Monad
    ( foldM )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), runMaybeT )
import Crypto.Random.Types
    ( MonadRandom )
import Data.Ord
    ( Down (..) )
import Internal.Coin
    ( Coin )

import qualified Data.List as L
import qualified Internal.Coin as C

-- | An implementation of the __Random-Improve__ coin selection algorithm.
--
-- = Overview
--
-- The __Random-Improve__ coin selection algorithm works in __two phases__, by
-- /first/ selecting UTxO entries /at random/ to pay for each of the given
-- outputs, and /then/ attempting to /improve/ upon each of the selections.
--
-- === Phase 1: Random Selection
--
-- __In this phase, the algorithm randomly selects a minimal set of UTxO__
-- __entries to pay for each of the given outputs.__
--
-- During this phase, the algorithm:
--
--   *  processes outputs in /descending order of coin value/.
--
--   *  maintains a /remaining UTxO set/, initially equal to the given
--      /UTxO set/ parameter.
--
-- For each output of value __/v/__, the algorithm /randomly/ selects entries
-- from the /remaining UTxO set/, until the total value of selected entries is
-- greater than or equal to __/v/__. The selected entries are then associated
-- with that output, and removed from the /remaining UTxO set/.
--
-- This phase ends when every output has been associated with a selection of
-- UTxO entries.
--
-- However, if the remaining UTxO set is completely exhausted before all
-- outputs can be processed, the algorithm terminates with an error.
--
-- === Phase 2: Improvement
--
-- __In this phase, the algorithm attempts to improve upon each of the UTxO__
-- __selections made in the previous phase, by conservatively expanding the__
-- __selection made for each output.__
--
-- During this phase, the algorithm:
--
--   *  processes outputs in /ascending order of coin value/.
--
--   *  continues to maintain the /remaining UTxO set/ produced by the previous
--      phase.
--
--   *  maintains an /accumulated coin selection/, which is initially /empty/.
--
-- For each output of value __/v/__, the algorithm:
--
--  1.  __Calculates a /target range/__ for the total value of inputs used to
--      pay for that output, defined by the triplet:
--
--      (/minimum/, /ideal/, /maximum/) = (/v/, /2v/, /3v/)
--
--  2.  __Attempts to /improve/ upon the /existing UTxO selection/__ for that
--      output, by repeatedly selecting additional entries at random from the
--      /remaining UTxO set/, stopping when the selection can be improved upon
--      no further.
--
--      A selection with value /v1/ is considered to be an /improvement/ over a
--      selection with value /v0/ if __all__ of the following conditions are
--      satisfied:
--
--       * __Condition 1__: we have moved closer to the /ideal/ value:
--
--             abs (/ideal/ − /v1/) < abs (/ideal/ − /v0/)
--
--       * __Condition 2__: we have not exceeded the /maximum/ value:
--
--             /v1/ ≤ /maximum/
--
--       * __Condition 3__: when counting cumulatively across all outputs
--       considered so far, we have not selected more than the /maximum/ number
--       of UTxO entries specified by 'calculateInputLimit'.
--
--  3.  __Creates a /change value/__ for the output, equal to the total value
--      of the /final UTxO selection/ for that output minus the value /v/ of
--      that output.
--
--  4.  __Updates the /accumulated coin selection/__:
--
--       * Adds the /output/ to 'outputs'.
--       * Adds the /improved UTxO selection/ to 'inputs'.
--       * Adds the /change value/ to 'change'.
--
-- This phase ends when every output has been processed, __or__ when the
-- /remaining UTxO set/ has been exhausted, whichever occurs sooner.
--
-- = Termination
--
-- When both phases are complete, the algorithm terminates.
--
-- The /accumulated coin selection/ and /remaining UTxO set/ are returned to
-- the caller.
--
-- === Failure Modes
--
-- The algorithm terminates with an __error__ if:
--
--  1.  The /total value/ of the initial UTxO set (the amount of money
--      /available/) is /less than/ the total value of the output list (the
--      amount of money /required/).
--
--      See: __'ErrUtxoBalanceInsufficient'__.
--
--  2.  The /number/ of entries in the initial UTxO set is /smaller than/ the
--      number of requested outputs.
--
--      Due to the nature of the algorithm, /at least one/ UTxO entry is
--      required /for each/ output.
--
--      See: __'ErrUtxoNotFragmentedEnough'__.
--
--  3.  Due to the particular /distribution/ of values within the initial UTxO
--      set, the algorithm depletes all entries from the UTxO set /before/ it
--      is able to pay for all requested outputs.
--
--      See: __'ErrUtxoFullyDepleted'__.
--
--  4.  The /number/ of UTxO entries needed to pay for the requested outputs
--      would /exceed/ the upper limit specified by 'calculateInputLimit'.
--
--      See: __'ErrMaximumInputCountExceeded'__.
--
-- = Motivating Principles
--
-- There are several motivating principles behind the design of the algorithm.
--
-- === Principle 1: Dust Management
--
-- The probability that random selection will choose dust entries from a UTxO
-- set increases with the proportion of dust in the set.
--
-- Therefore, for a UTxO set with a large amount of dust, there's a high
-- probability that a random subset will include a large amount of dust.
--
-- === Principle 2: Change Management
--
-- Ideally, coin selection algorithms should, over time, create a UTxO set that
-- has /useful/ outputs: outputs that will allow us to process future payments
-- with a minimum number of inputs.
--
-- If for each payment request of value __/v/__ we create a change output of
-- /roughly/ the same value __/v/__, then we will end up with a distribution of
-- change values that matches the typical value distribution of payment
-- requests.
--
-- === Principle 3: Performance Management
--
-- Searching the UTxO set for additional entries to improve our change outputs
-- is /only/ useful if the UTxO set contains entries that are sufficiently
-- small enough. But it is precisely when the UTxO set contains many small
-- entries that it is less likely for a randomly-chosen UTxO entry to push the
-- total above the upper bound.
--
randomImprove
    :: (Ord i, Ord o, MonadRandom m)
    => CoinSelectionAlgorithm i o m
randomImprove = CoinSelectionAlgorithm payForOutputs

payForOutputs
    :: (Ord i, Ord o, MonadRandom m)
    => CoinSelectionParameters i o
    -> ExceptT CoinSelectionError m (CoinSelectionResult i o)
payForOutputs params = do
    mRandomSelections <- lift $ runMaybeT $ foldM makeRandomSelection
        (inputCountMax, inputsAvailable params, []) outputsDescending
    case mRandomSelections of
        Just (inputCountRemaining, utxoRemaining, randomSelections) -> do
            (_, finalSelection, utxoRemaining') <- lift $ foldM
                improveSelection
                    (inputCountRemaining, mempty, utxoRemaining)
                    (reverse randomSelections)
            pure $ CoinSelectionResult finalSelection utxoRemaining'
        Nothing ->
            throwE errorCondition
  where
    errorCondition
      | amountAvailable < amountRequested =
          ErrUtxoBalanceInsufficient amountAvailable amountRequested
      | utxoCount < outputCount =
          ErrUtxoNotFragmentedEnough
              (fromIntegral utxoCount) (fromIntegral outputCount)
      | utxoCount <= inputCountMax =
          ErrUtxoFullyDepleted
      | otherwise =
          ErrMaximumInputCountExceeded (fromIntegral inputCountMax)
    amountAvailable =
        coinMapValue $ inputsAvailable params
    amountRequested =
        coinMapValue $ outputsRequested params
    inputCountMax = fromIntegral
        $ calculateInputLimit (inputLimit params)
        $ fromIntegral outputCount
    outputCount =
        fromIntegral $ length $ coinMapToList $ outputsRequested params
    outputsDescending =
        L.sortOn (Down . entryValue) $ coinMapToList $ outputsRequested params
    utxoCount =
        fromIntegral $ L.length $ coinMapToList $ inputsAvailable params

-- | Randomly select entries from the given UTxO set, until the total value of
--   selected entries is greater than or equal to the given output value.
--
-- Once a random selection has been made that meets the above criterion, this
-- function returns that selection as is, making no attempt to improve upon
-- the selection in any way.
--
makeRandomSelection
    :: forall i o m . MonadRandom m
    => (Integer, CoinMap i, [([CoinMapEntry i], CoinMapEntry o)])
    -> CoinMapEntry o
    -> MaybeT m (Integer, CoinMap i, [([CoinMapEntry i], CoinMapEntry o)])
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
        :: ([CoinMapEntry i], CoinMap i)
        -> MaybeT m ([CoinMapEntry i], CoinMap i)
    coverRandomly (selected, remaining)
        | L.length selected > fromIntegral inputCountRemaining =
            MaybeT $ return Nothing
        | sumEntries selected >= targetMin (mkTargetRange txout) =
            MaybeT $ return $ Just (selected, remaining)
        | otherwise =
            utxoPickRandomT remaining >>= \(picked, remaining') ->
                coverRandomly (picked : selected, remaining')

-- | Perform an improvement to random selection on a given output.
improveSelection
    :: forall i o m . (MonadRandom m, Ord i, Ord o)
    => (Integer, CoinSelection i o, CoinMap i)
    -> ([CoinMapEntry i], CoinMapEntry o)
    -> m (Integer, CoinSelection i o, CoinMap i)
improveSelection (maxN0, selection, utxo0) (inps0, txout) = do
    (maxN, inps, utxo) <- improve (maxN0, inps0, utxo0)
    return
        ( maxN
        , selection <> CoinSelection
            { inputs = coinMapFromList inps
            , outputs = coinMapFromList [txout]
            , change = mkChange txout inps
            }
        , utxo
        )
  where
    target = mkTargetRange txout

    improve
        :: (Integer, [CoinMapEntry i], CoinMap i)
        -> m (Integer, [CoinMapEntry i], CoinMap i)
    improve (maxN, inps, utxo)
        | maxN >= 1 && sumEntries inps < targetAim target = do
            runMaybeT (utxoPickRandomT utxo) >>= \case
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

    isImprovement :: CoinMapEntry i -> [CoinMapEntry i] -> Bool
    isImprovement io selected =
        let
            condA = -- (a) It doesn’t exceed a specified upper limit.
                sumEntries (io : selected) < targetMax target

            condB = -- (b) Addition gets us closer to the ideal change
                distanceA < distanceB
              where
                distanceA = C.distance
                    (targetAim target)
                    (sumEntries (io : selected))
                distanceB = C.distance
                    (targetAim target)
                    (sumEntries selected)

            -- (c) Doesn't exceed maximum number of inputs
            -- Guaranteed by the precondition on 'improve'.
        in
            condA && condB

--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------

-- | Represents a target range of /total input values/ for a given output.
--
-- In this context, /total input value/ refers to the total value of a set of
-- inputs selected to pay for a given output.
--
data TargetRange = TargetRange
    { targetMin :: Coin
        -- ^ The minimum value, corresponding to exactly the requested target
        -- amount, and a change amount of zero.
    , targetAim :: Coin
        -- ^ The ideal value, corresponding to exactly twice the requested
        -- target amount, and a change amount equal to the requested amount.
    , targetMax :: Coin
        -- ^ The maximum value, corresponding to exactly three times the
        -- requested amount, and a change amount equal to twice the requested
        -- amount.
    }

-- | Compute the target range of /total input values/ for a given output.
--
-- See 'TargetRange'.
--
mkTargetRange :: CoinMapEntry o -> TargetRange
mkTargetRange (CoinMapEntry _ c) = TargetRange
    { targetMin = c
    , targetAim = c `C.add` c
    , targetMax = c `C.add` c `C.add` c
    }

-- | Re-wrap 'utxoPickRandom' in a 'MaybeT' monad
utxoPickRandomT
    :: MonadRandom m
    => CoinMap i
    -> MaybeT m (CoinMapEntry i, CoinMap i)
utxoPickRandomT =
    MaybeT
        . fmap (\(mi, u) -> (, u) <$> mi)
        . coinMapRandomEntry

-- | Compute change outputs from a target output and a selection of inputs.
--
-- Pre-condition:
--
-- The output must be less than (or equal to) the sum of the inputs.
--
mkChange :: CoinMapEntry o -> [CoinMapEntry i] -> [Coin]
mkChange (CoinMapEntry _ out) inps =
    case difference of
        Nothing ->
            error $ mconcat
                [ "mkChange: "
                , "output must be less than or equal to sum of inputs"
                ]
        Just d | C.isZero d ->
            []
        Just d ->
            [d]
  where
    difference = sumEntries inps `C.sub` out

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

sumEntries :: [CoinMapEntry i] -> Coin
sumEntries = mconcat . fmap entryValue
