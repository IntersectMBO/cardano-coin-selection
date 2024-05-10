{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2024 Intersect MBO
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
-- The Largest-First coin selection algorithm considers available inputs in
-- /descending/ order of value, from /largest/ to /smallest/.
--
-- When applied to a set of requested outputs, the algorithm repeatedly selects
-- entries from the available inputs set until the total value of selected
-- entries is greater than or equal to the total value of requested outputs.
--
-- === Change Values
--
-- If the total value of selected inputs is /greater than/ the total value of
-- all requested outputs, the 'change' set of the resulting selection will
-- contain /a single coin/ with the excess value.
--
-- If the total value of selected inputs is /exactly equal to/ the total value
-- of all requested outputs, the 'change' set of the resulting selection will
-- be /empty/.
--
-- === Failure Modes
--
-- The algorithm terminates with an __error__ if:
--
--  1.  The /total value/ of 'inputsAvailable' (the amount of money
--      /available/) is /less than/ the total value of 'outputsRequested' (the
--      amount of money /required/).
--
--      See: __'InputValueInsufficientError'__.
--
--  2.  It is not possible to cover the total value of 'outputsRequested'
--      without selecting a number of inputs from 'inputsAvailable' that
--      would exceed the maximum defined by 'limit'.
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
