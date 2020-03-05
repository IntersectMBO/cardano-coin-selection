{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains an implementation of the __largest first__ coin
-- selection algorithm.
--
module Cardano.CoinSelection.LargestFirst (
    largestFirst
  , atLeast
  ) where

import Prelude

import Cardano.CoinSelection
    ( CoinSelection (..), CoinSelectionOptions (..), ErrCoinSelection (..) )
import Cardano.Types
    ( Coin (..), TxIn, TxOut (..), UTxO (..), balance )
import Control.Arrow
    ( left )
import Control.Monad
    ( foldM )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, throwE )
import Data.Functor
    ( ($>) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Ord
    ( Down (..) )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

-- | Generate a coin selection according to the __largest first__ algorithm.
--
-- For the given /output list/ and /starting UTxO set/, this algorithm
-- generates a /coin selection/ that is capable of paying for all of the
-- outputs, and a /remaining UTxO set/ from which spent values have been
-- removed.
--
-- === Cardinality Rules
--
-- The algorithm requires that:
--
--  1.  Each output is paid for by /one or more/ entries from the UTxO set.
--
--  2.  Each entry from the UTxO set is used to pay for /at most one/ output.
--
--      (A given entry from the UTxO set /cannot/ be used to pay for multiple
--      outputs.)
--
-- === Order of Processing
--
-- The algorithm processes /both/ the given output list /and/ the supplied UTxO
-- set in __descending order of coin value__, from largest to smallest.
--
-- At all stages of processing, the algorithm maintains a /remaining UTxO set/
-- that is steadily depleted as outputs are paid for.
--
-- === Processing an Output
--
-- For /each output/ in the (descending) output list, the algorithm repeatedly
-- selects unspent values from the /remaining UTxO set/ (in descending order)
-- until the /total selected value/ is greater than (or equal to) the output
-- value, at which point the algorithm move ons to processing the /next/
-- output.
--
-- If the /total selected value/ is greater than required for a particular
-- output, the algorithm generates a /change output/ with the exact difference
-- in value.
--
-- === Termination Conditions
--
-- The algorithm terminates with an __error__ if:
--
--  1.  The /total value/ of the starting UTxO set (the amount of money
--      /available/) is /less than/ the total value of the output list (the
--      amount of money /required/).
--
--      See: __'ErrNotEnoughMoney'__.
--
--  2.  The /number/ of entries in the starting UTxO set is /smaller than/ the
--      number of requested outputs.
--
--      Due to the nature of the algorithm, /at least one/ UTxO entry is
--      required /for each/ output.
--
--      See: __'ErrUtxoNotFragmentedEnough'__.
--
--  3.  Due to the particular /distribution/ of values within the starting UTxO
--      set, the algorithm depletes all entries from the set /before/ it is able
--      to pay for all requested outputs.
--
--      See: __'ErrInputsDepleted'__.
--
--  4.  The /number/ of UTxO entries needed to pay for the requested outputs
--      would /exceed/ the upper limit specified by 'maximumNumberOfInputs'.
--
--      See: __'ErrMaximumInputsReached'__.
--
largestFirst
    :: forall m e. Monad m
    => CoinSelectionOptions e
    -> NonEmpty TxOut
    -> UTxO
    -> ExceptT (ErrCoinSelection e) m (CoinSelection, UTxO)
largestFirst options outputsRequested utxo =
    case foldM atLeast (utxoDescending, mempty) outputsDescending of
        Just (utxoRemaining, selection) ->
            validateSelection selection $>
                (selection, UTxO $ Map.fromList utxoRemaining)
        Nothing ->
            throwE errorCondition
  where
    errorCondition
      | amountAvailable < amountRequested =
          ErrNotEnoughMoney amountAvailable amountRequested
      | utxoCount < outputCount =
          ErrUtxoNotFragmentedEnough utxoCount outputCount
      | utxoCount <= inputCountMax =
          ErrInputsDepleted
      | otherwise =
          ErrMaximumInputsReached inputCountMax
    amountAvailable =
        fromIntegral $ balance utxo
    amountRequested =
        sum $ (getCoin . coin) <$> outputsRequested
    inputCountMax =
        fromIntegral $ maximumNumberOfInputs options $ fromIntegral outputCount
    outputCount =
        fromIntegral $ NE.length outputsRequested
    outputsDescending =
        L.sortOn (Down . coin) $ NE.toList outputsRequested
    utxoCount =
        fromIntegral $ L.length $ (Map.toList . getUTxO) utxo
    utxoDescending =
        take (fromIntegral inputCountMax)
            $ L.sortOn (Down . coin . snd)
            $ Map.toList
            $ getUTxO utxo
    validateSelection =
        except . left ErrInvalidSelection . validate options

-- | Attempts to pay for a /single transaction output/ by selecting the
--   /smallest possible/ number of entries from the /head/ of the given
--   UTxO list.
--
-- Returns a /reduced/ list of UTxO entries, and a coin selection that is
-- /updated/ to include the payment.
--
-- If the total value of entries in the given UTxO list is /less than/ the
-- required output amount, this function will return 'Nothing'.
--
atLeast
    :: ([(TxIn, TxOut)], CoinSelection)
    -> TxOut
    -> Maybe ([(TxIn, TxOut)], CoinSelection)
atLeast (utxoAvailable, currentSelection) txout =
    let target = fromIntegral $ getCoin $ coin txout in
    coverTarget target utxoAvailable mempty
  where
    coverTarget
        :: Integer
        -> [(TxIn, TxOut)]
        -> [(TxIn, TxOut)]
        -> Maybe ([(TxIn, TxOut)], CoinSelection)
    coverTarget target utxoRemaining utxoSelected
        | target <= 0 = Just
            -- We've selected enough to cover the target, so stop here.
            ( utxoRemaining
            , currentSelection <> CoinSelection
                { inputs  = utxoSelected
                , outputs = [txout]
                , change  = [Coin $ fromIntegral $ abs target | target < 0]
                }
            )
        | otherwise =
            -- We haven't yet selected enough to cover the target, so attempt
            -- to select a little more and then continue.
            case utxoRemaining of
                (i, o):utxoRemaining' ->
                    let utxoSelected' = (i, o):utxoSelected
                        target' = target - fromIntegral (getCoin (coin o))
                    in
                    coverTarget target' utxoRemaining' utxoSelected'
                [] ->
                    -- The UTxO has been exhausted, so stop here.
                    Nothing
