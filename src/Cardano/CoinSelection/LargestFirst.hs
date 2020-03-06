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
-- === Summary
--
-- For the given /output list/ and /initial UTxO set/, this algorithm generates
-- a /coin selection/ that is capable of paying for all of the outputs, and a
-- /remaining UTxO set/ from which all spent values have been removed.
--
-- === State Maintained by the Algorithm
--
-- At all stages of processing, the algorithm maintains:
--
--  1.  A __/remaining UTxO list/__
--
--      This is initially equal to the given /initial UTxO set/ parameter,
--      sorted into /descending order of coin value/.
--
--      The /head/ of the list is always the remaining UTxO entry with the
--      /greatest coin value/.
--
--      Entries are incrementally removed from the /head/ of the list as the
--      algorithm proceeds, until the list is empty.
--
--  2.  An __/unpaid output list/__
--
--      This is initially equal to the given /output list/ parameter, sorted
--      into /descending order of coin value/.
--
--      The /head/ of the list is always the unpaid output with the
--      /greatest coin value/.
--
--      Entries are incrementally removed from the /head/ of the list as the
--      algorithm proceeds, until the list is empty.
--
--  3.  An __/accumulated coin selection/__
--
--      This is initially /empty/.
--
--      Entries are incrementally added as each output is paid for, until the
--      /unpaid output list/ is empty.
--
-- === Cardinality Rules
--
-- The algorithm requires that:
--
--  1.  Each output from the given /output list/ is paid for by /one or more/
--      entries from the /initial UTxO set/.
--
--  2.  Each entry from the /initial UTxO set/ is used to pay for /at most one/
--      output from the given /output list/.
--
--      (A single UTxO entry __cannot__ be used to pay for multiple outputs.)
--
-- === Order of Processing
--
-- The algorithm proceeds according to the following sequence of steps:
--
--  *   /Step 1/
--
--      Remove a single /unpaid output/ from the head of the
--      /unpaid output list/.
--
--  *   /Step 2/
--
--      Repeatedly remove entries from the head of the /remaining UTxO list/
--      until the total value of entries removed is /greater than or equal to/
--      the value of the /unpaid output/.
--
--  *   /Step 3/
--
--      Use the /removed UTxO entries/ to pay for the /unpaid output/.
--
--      This is achieved by adding the /removed UTxO entries/ to the 'inputs'
--      field of the /accumulated coin selection/, and adding the /output/ to
--      the 'outputs' field of the /accumulated coin selection/.
--
--  *   /Step 4/
--
--      If the /total selected value/ is greater than the value required for
--      the current output, generate a coin whose value is equal to the exact
--      difference, and add it to the 'change' field of the
--      /accumulated coin selection/.
--
--  *   /Step 5/
--
--      If the /unpaid output list/ is empty, __terminate__ here.
--
--      Otherwise, return to /Step 1/.
--
-- === Successful Termination
--
-- The algorithm terminates __successfully__ if the /remaining UTxO list/ is
-- not depleted before the /unpaid output list/ can be fully depleted (i.e., if
-- all the outputs have been paid for).
--
-- The /accumulated coin selection/ and /remaining UTxO list/ are returned to
-- the caller.
--
-- === Unsuccessful Termination
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
--      See: __'ErrUxtoFullyDepleted'__.
--
--  4.  The /number/ of UTxO entries needed to pay for the requested outputs
--      would /exceed/ the upper limit specified by 'maximumInputCount'.
--
--      See: __'ErrMaximumInputCountExceeded'__.
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
          ErrUtxoBalanceInsufficient amountAvailable amountRequested
      | utxoCount < outputCount =
          ErrUtxoNotFragmentedEnough utxoCount outputCount
      | utxoCount <= inputCountMax =
          ErrUxtoFullyDepleted
      | otherwise =
          ErrMaximumInputCountExceeded inputCountMax
    amountAvailable =
        fromIntegral $ balance utxo
    amountRequested =
        sum $ (getCoin . coin) <$> outputsRequested
    inputCountMax =
        fromIntegral $ maximumInputCount options $ fromIntegral outputCount
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
