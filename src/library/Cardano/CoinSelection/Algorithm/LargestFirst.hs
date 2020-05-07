{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains an implementation of the __Largest-First__ coin
-- selection algorithm.
--
module Cardano.CoinSelection.Algorithm.LargestFirst (
    largestFirst
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
    , InputLimitExceededError (..)
    , InputValueInsufficientError (..)
    , coinMapFromList
    , coinMapToList
    , coinMapValue
    )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE )
import Data.Function
    ( (&) )
import Data.Ord
    ( Down (..) )
import Data.Word
    ( Word16 )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Internal.Coin as C

-- | An implementation of the __Largest-First__ coin selection algorithm.
--
-- = Overview
--
-- The __Largest-First__ algorithm processes outputs in /descending order of/
-- /value/, from /largest/ to /smallest/.
--
-- For each output, it repeatedly selects the /largest/ remaining unspent UTxO
-- entry until the value of selected entries is greater than or equal to the
-- value of that output.
--
-- = State Maintained by the Algorithm
--
-- At all stages of processing, the algorithm maintains:
--
--  1.  A __/remaining UTxO list/__
--
--      This is initially equal to the given /initial UTxO set/ parameter,
--      sorted into /descending order of coin value/.
--
--      The /head/ of the list is always the remaining UTxO entry with the
--      /largest coin value/.
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
--      /largest coin value/.
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
-- = Cardinality Rules
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
-- = Order of Processing
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
--      Repeatedly remove UTxO entries from the head of the
--      /remaining UTxO list/ until the total value of entries removed is
--      /greater than or equal to/ the value of the /removed output/.
--
--  *   /Step 3/
--
--      Use the /removed UTxO entries/ to pay for the /removed output/.
--
--      This is achieved by:
--
--      *  adding the /removed UTxO entries/ to the 'inputs' field of the
--         /accumulated coin selection/.
--      *  adding the /removed output/ to the 'outputs' field of the
--         /accumulated coin selection/.
--
--  *   /Step 4/
--
--      If the /total value/ of the /removed UTxO entries/ is greater than the
--      value of the /removed output/, generate a coin whose value is equal to
--      the exact difference, and add it to the 'change' field of the
--      /accumulated coin selection/.
--
--  *   /Step 5/
--
--      If the /unpaid output list/ is empty, __terminate__ here.
--
--      Otherwise, return to /Step 1/.
--
-- = Termination
--
-- The algorithm terminates __successfully__ if the /remaining UTxO list/ is
-- not depleted before the /unpaid output list/ can be fully depleted (i.e., if
-- all the outputs have been paid for).
--
-- The /accumulated coin selection/ and /remaining UTxO list/ are returned to
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
--      See: __'InputValueInsufficientError'__.
--
--  2.  The /number/ of entries in the initial UTxO set is /smaller than/ the
--      number of requested outputs.
--
--      Due to the nature of the algorithm, /at least one/ UTxO entry is
--      required /for each/ output.
--
--      See: __'InputCountInsufficientError'__.
--
--  3.  Due to the particular /distribution/ of values within the initial UTxO
--      set, the algorithm depletes all entries from the UTxO set /before/ it
--      is able to pay for all requested outputs.
--
--      See: __'InputsExhaustedError'__.
--
--  4.  The /number/ of UTxO entries needed to pay for the requested outputs
--      would /exceed/ the upper limit specified by 'limit'.
--
--      See: __'InputLimitExceededError'__.
--
-- @since 1.0.0
largestFirst
    :: (Ord i, Monad m)
    => CoinSelectionAlgorithm i o m
largestFirst = CoinSelectionAlgorithm payForOutputs

payForOutputs
    :: forall i o m . (Ord i, Monad m)
    => CoinSelectionParameters i o
    -> ExceptT CoinSelectionError m (CoinSelectionResult i o)
payForOutputs params
    | amountAvailable < amountRequired =
        throwE
            $ InputValueInsufficient
            $ InputValueInsufficientError amountAvailable amountRequired
    | length inputsSelected > inputCountMax =
        throwE
            $ InputLimitExceeded
            $ InputLimitExceededError
            $ fromIntegral inputCountMax
    | otherwise =
        pure CoinSelectionResult {coinSelection, inputsRemaining}
  where
    amountAvailable =
        coinMapValue $ inputsAvailable params
    amountRequired =
        coinMapValue $ outputsRequested params
    coinSelection = CoinSelection
        { inputs =
            inputsSelected
        , outputs =
            outputsRequested params
        , change = filter (> C.zero)
            $ F.toList
            $ coinMapValue inputsSelected `C.sub` amountRequired
        }
    inputsAvailableDescending :: [CoinMapEntry i]
    inputsAvailableDescending = inputsAvailable params
        & coinMapToList
        & L.sortOn (Down . entryValue)
    inputCountMax :: Int
    inputCountMax = outputsRequested params
        & coinMapToList
        & length
        & fromIntegral @Int @Word16
        & calculateLimit (limit params)
        & fromIntegral @Word16 @Int
    inputsSelected :: CoinMap i
    inputsSelected = inputsAvailableDescending
        & fmap entryValue
        & scanl1 (<>)
        & takeUntil (>= amountRequired)
        & zip inputsAvailableDescending
        & fmap fst
        & coinMapFromList
    inputsRemaining :: CoinMap i
    inputsRemaining = inputsAvailableDescending
        & drop (length inputsSelected)
        & coinMapFromList

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []
