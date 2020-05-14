{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CoinSelection.Algorithm.LargestFirstSpec
    ( isValidLargestFirstError
    , spec
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
import Cardano.CoinSelection.Algorithm.LargestFirst
    ( largestFirst )
import Cardano.CoinSelectionSpec
    ( CoinSelectionData (..)
    , CoinSelectionFixture (..)
    , CoinSelectionTestResult (..)
    , coinSelectionAlgorithmGeneralProperties
    , coinSelectionUnitTest
    )
import Cardano.Test.Utilities
    ( Address, TxIn, excluding, unsafeCoin )
import Control.Monad
    ( unless )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Either
    ( isRight )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Property, checkCoverage, cover, property, withMaxSuccess, (.&&.), (==>) )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Internal.Coin as C

spec :: Spec
spec = do
    describe "Coin selection: largest-first algorithm: unit tests" $ do

        coinSelectionUnitTest largestFirst
            "Expect success: case #1"
            (Right $ CoinSelectionTestResult
                { rsInputs = [17]
                , rsChange = []
                , rsOutputs = [17]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [10,10,17]
                , txOutputs = [17]
                })

        coinSelectionUnitTest largestFirst
            "Expect success: case #2"
            (Right $ CoinSelectionTestResult
                { rsInputs = [17]
                , rsChange = [16]
                , rsOutputs = [1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = [1]
                })

        coinSelectionUnitTest largestFirst
            "Expect success: case #3"
            (Right $ CoinSelectionTestResult
                { rsInputs = [12, 17]
                , rsChange = [11]
                , rsOutputs = [18]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = [18]
                })

        coinSelectionUnitTest largestFirst
            "Expect success: case #4"
            (Right $ CoinSelectionTestResult
                { rsInputs = [10, 12, 17]
                , rsChange = [9]
                , rsOutputs = [30]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = [30]
                })

        coinSelectionUnitTest largestFirst
            "Expect success: case #5"
            (Right $ CoinSelectionTestResult
                { rsInputs = [6,10]
                , rsChange = [4]
                , rsOutputs = [11,1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 3
                , utxoInputs = [1,2,10,6,5]
                , txOutputs = [11, 1]
                })

        coinSelectionUnitTest largestFirst
            "Expect success: case #6"
            (Right $ CoinSelectionTestResult
                { rsInputs = [12,17,20]
                , rsChange = [6]
                , rsOutputs = [1,1,1,40]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,20,17]
                , txOutputs = [40,1,1,1]
                })

        coinSelectionUnitTest largestFirst
            "Expect success: case #7"
            (Right $ CoinSelectionTestResult
                { rsInputs = [12,17,20]
                , rsChange = [8]
                , rsOutputs = [1,40]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,20,17]
                , txOutputs = [40, 1]
                })

        coinSelectionUnitTest largestFirst
            "Expect success: case #8"
            (Right $ CoinSelectionTestResult
                { rsInputs = [10,20,20]
                , rsChange = [3]
                , rsOutputs = [6,41]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [20,20,10,5]
                , txOutputs = [41, 6]
                })

        coinSelectionUnitTest largestFirst
            "Expect success: case #9"
            (Right $ CoinSelectionTestResult
                { rsInputs = [6,10]
                , rsChange = [4]
                , rsOutputs = [1,11]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 2
                , utxoInputs = [1,2,10,6,5]
                , txOutputs = [11, 1]
                })

        coinSelectionUnitTest largestFirst
            "Expect success: fewer inputs than outputs: case #1"
            (Right $ CoinSelectionTestResult
                { rsInputs = [100]
                , rsOutputs = [1,2,3,4]
                , rsChange = [90]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 1000
                , utxoInputs = [100,100]
                , txOutputs = [1,2,3,4]
                })

        coinSelectionUnitTest largestFirst
            "Expect success: fewer inputs than outputs: case #2"
            (Right $ CoinSelectionTestResult
                { rsInputs = [100]
                , rsOutputs = [1,2,3,4]
                , rsChange = [90]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 1000
                , utxoInputs = [100,10]
                , txOutputs = [1,2,3,4]
                })

        coinSelectionUnitTest largestFirst
            "Expect success: fewer inputs than outputs: case #3"
            (Right $ CoinSelectionTestResult
                { rsInputs = [10]
                , rsOutputs = [1,2,3,4]
                , rsChange = []
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 1000
                , utxoInputs = [10,10]
                , txOutputs = [1,2,3,4]
                })

        coinSelectionUnitTest largestFirst
            "Expect success: fewer inputs than outputs: case #4"
            (Right $ CoinSelectionTestResult
                { rsInputs = [100]
                , rsOutputs = replicate 100 1
                , rsChange = []
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 1
                , utxoInputs = [100]
                , txOutputs = replicate 100 1
                })

        coinSelectionUnitTest largestFirst
            "UTxO balance not sufficient: case #1"
            (Left $ InputValueInsufficient $ InputValueInsufficientError
                (unsafeCoin @Int 39) (unsafeCoin @Int 40))
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = [40]
                })

        coinSelectionUnitTest largestFirst
            "UTxO balance not sufficient: case #2"
            (Left $ InputValueInsufficient $ InputValueInsufficientError
                (unsafeCoin @Int 39) (unsafeCoin @Int 43))
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = [40,1,1,1]
                })

        coinSelectionUnitTest largestFirst
            "UTxO balance sufficient, but maximum input count exceeded"
            (Left $ InputLimitExceeded $ InputLimitExceededError 9)
            (CoinSelectionFixture
                { maxNumOfInputs = 9
                , utxoInputs = replicate 100 1
                , txOutputs = replicate 100 1
                })

    describe "Coin selection: largest-first algorithm: properties" $ do

        it "forall (UTxO, NonEmpty TxOut), for all selected input, there's no \
            \bigger input in the UTxO that is not already in the selected \
            \inputs"
            (property $ propInputDecreasingOrder @TxIn @Address)

        it "The algorithm selects just enough inputs and no more."
            (property
                $ withMaxSuccess 10_000
                $ propSelectionMinimal @Int @Int)

        it "The algorithm produces the correct set of change."
            (checkCoverage
                $ property
                $ withMaxSuccess 10_000
                $ propChangeCorrect @Int @Int)

    coinSelectionAlgorithmGeneralProperties @Int @Int
        largestFirst "Largest-First"

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

propInputDecreasingOrder
    :: Ord i
    => CoinSelectionData i o
    -> Property
propInputDecreasingOrder (CoinSelectionData utxo txOuts) =
    isRight selection ==>
        let Right (CoinSelectionResult s _) = selection in
        prop s
  where
    prop (CoinSelection inps _ _) =
        let
            utxo' = (Map.toList . unCoinMap) $ utxo `excluding`
                Set.fromList (entryKey <$> coinMapToList inps)
        in unless (L.null utxo') $
            (L.minimum (entryValue <$> coinMapToList inps))
            `shouldSatisfy`
            (>= (L.maximum (snd <$> utxo')))
    selection = runIdentity
        $ runExceptT
        $ selectCoins largestFirst
        $ CoinSelectionParameters utxo txOuts selectionLimit
    selectionLimit = CoinSelectionLimit $ const 100

-- Confirm that a selection is minimal by removing the smallest entry from the
-- inputs and verifying that the reduced input total is no longer enough to pay
-- for the total value of all outputs.
propSelectionMinimal
    :: Ord i => CoinSelectionData i o -> Property
propSelectionMinimal (CoinSelectionData inpsAvailable outsRequested) =
    isRight result ==>
        let Right (CoinSelectionResult selection _) = result in
        prop selection
  where
    prop (CoinSelection inputsSelected _ _) =
        (coinMapValue inputsSelected
            `shouldSatisfy` (>= coinMapValue outsRequested))
        .&&.
        (coinMapValue inputsReduced
            `shouldSatisfy` (< coinMapValue outsRequested))
      where
        -- The set of selected inputs with the smallest entry removed.
        inputsReduced = inputsSelected
            & coinMapToList
            & L.sortOn entryValue
            & L.drop 1
            & coinMapFromList
    result = runIdentity
        $ runExceptT
        $ selectCoins largestFirst
        $ CoinSelectionParameters inpsAvailable outsRequested
        $ CoinSelectionLimit $ const 1000

-- Verify that the algorithm generates the correct set of change.
propChangeCorrect
    :: Ord i => CoinSelectionData i o -> Property
propChangeCorrect (CoinSelectionData inpsAvailable outsRequested) =
    isRight result ==>
        let Right (CoinSelectionResult selection _) = result in
        prop selection
  where
    prop (CoinSelection inpsSelected _ changeGenerated) =
        cover 8 (amountSelected > amountRequired)
            "amountSelected > amountRequired" $
        cover 1 (amountSelected == amountRequired)
            "amountSelected = amountRequired" $
        if amountSelected > amountRequired then
            changeGenerated `shouldBe`
                [amountSelected `C.distance` amountRequired]
        else
            changeGenerated `shouldSatisfy` null
      where
        amountSelected = coinMapValue inpsSelected
        amountRequired = coinMapValue outsRequested
    result = runIdentity
        $ runExceptT
        $ selectCoins largestFirst
        $ CoinSelectionParameters inpsAvailable outsRequested
        $ CoinSelectionLimit $ const 1000

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- Returns true if (and only if) the given error value is one that can be
-- thrown by the Largest-First algorithm.
--
isValidLargestFirstError :: CoinSelectionError -> Bool
isValidLargestFirstError = \case
    InputLimitExceeded     _ -> True
    InputValueInsufficient _ -> True
    InputCountInsufficient _ -> False
    InputsExhausted        _ -> False
