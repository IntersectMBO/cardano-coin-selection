{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CoinSelection.Algorithm.RandomImproveSpec
    ( spec
    ) where

import Prelude

import Cardano.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionAlgorithm (..)
    , CoinSelectionError (..)
    , CoinSelectionLimit (..)
    , CoinSelectionParameters (..)
    , CoinSelectionResult (..)
    , InputCountInsufficientError (..)
    , InputLimitExceededError (..)
    , InputValueInsufficientError (..)
    , InputsExhaustedError (..)
    )
import Cardano.CoinSelection.Algorithm.LargestFirst
    ( largestFirst )
import Cardano.CoinSelection.Algorithm.LargestFirstSpec
    ( isValidLargestFirstError )
import Cardano.CoinSelection.Algorithm.RandomImprove
    ( randomImprove )
import Cardano.CoinSelectionSpec
    ( CoinSelectionData (..)
    , CoinSelectionFixture (..)
    , CoinSelectionTestResult (..)
    , coinSelectionAlgorithmGeneralProperties
    , coinSelectionUnitTest
    )
import Cardano.Test.Utilities
    ( InputId, OutputId, unsafeCoin )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Random
    ( SystemDRG, getSystemDRG )
import Crypto.Random.Types
    ( withDRG )
import Data.Either
    ( isRight )
import Data.Functor.Identity
    ( Identity (..) )
import Test.Hspec
    ( Spec, before, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Property, counterexample, property, (==>) )

import qualified Data.List as L

spec :: Spec
spec = do
    describe "Coin selection : random algorithm unit tests" $ do
        let oneAda = 1_000_000

        coinSelectionUnitTest randomImprove ""
            (Right $ CoinSelectionTestResult
                { rsInputs = [1,1,1,1]
                , rsChange = [2]
                , rsOutputs = [2]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = [2]
                })

        coinSelectionUnitTest randomImprove ""
            (Right $ CoinSelectionTestResult
                { rsInputs = [1,1,1,1,1,1]
                , rsChange = [2,1]
                , rsOutputs = [2,1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = [2,1]
                })

        coinSelectionUnitTest randomImprove ""
            (Right $ CoinSelectionTestResult
                { rsInputs = [1,1,1,1,1]
                , rsChange = [2]
                , rsOutputs = [2,1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [1,1,1,1,1]
                , txOutputs = [2,1]
                })

        coinSelectionUnitTest randomImprove ""
            (Right $ CoinSelectionTestResult
                { rsInputs = [1,1,1,1]
                , rsChange = [1]
                , rsOutputs = [2,1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [1,1,1,1]
                , txOutputs = [2,1]
                })

        coinSelectionUnitTest randomImprove ""
            (Right $ CoinSelectionTestResult
                { rsInputs = [5]
                , rsChange = [3]
                , rsOutputs = [2]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [5,5,5]
                , txOutputs = [2]
                })

        coinSelectionUnitTest randomImprove ""
            (Right $ CoinSelectionTestResult
                { rsInputs = [10,10]
                , rsChange = [8,8]
                , rsOutputs = [2,2]
                }
            )
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [10,10,10]
                , txOutputs = [2,2]
                })

        coinSelectionUnitTest randomImprove "cannot cover aim, but only min"
            (Right $ CoinSelectionTestResult
                { rsInputs = [1,1,1,1]
                , rsChange = [1]
                , rsOutputs = [3]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 4
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = [3]
                })

        coinSelectionUnitTest randomImprove "REG CO-450: no fallback"
            (Right $ CoinSelectionTestResult
                { rsInputs = [oneAda, oneAda, oneAda, oneAda]
                , rsChange = [oneAda, oneAda `div` 2]
                , rsOutputs = [2*oneAda,oneAda `div` 2]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 4
                , utxoInputs = [oneAda, oneAda, oneAda, oneAda]
                , txOutputs = [2*oneAda, oneAda `div` 2]
                })

        coinSelectionUnitTest randomImprove
            "enough funds, proper fragmentation, inputs depleted"
            (Left (InputsExhausted InputsExhaustedError))
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [10,10,10,10]
                , txOutputs = [38,1]
                })

        coinSelectionUnitTest randomImprove ""
            (Left $ InputLimitExceeded $ InputLimitExceededError 2)
            (CoinSelectionFixture
                { maxNumOfInputs = 2
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = [3]
                })

        coinSelectionUnitTest randomImprove "each output needs <maxNumOfInputs"
            (Left $ InputLimitExceeded $ InputLimitExceededError 9)
            (CoinSelectionFixture
                { maxNumOfInputs = 9
                , utxoInputs = replicate 100 1
                , txOutputs = replicate 100 1
                })

        coinSelectionUnitTest randomImprove "each output needs >maxNumInputs"
            (Left $ InputLimitExceeded $ InputLimitExceededError 9)
            (CoinSelectionFixture
                { maxNumOfInputs = 9
                , utxoInputs = replicate 100 1
                , txOutputs = replicate 10 10
                })

        coinSelectionUnitTest randomImprove ""
            (Left $ InputValueInsufficient $ InputValueInsufficientError
                (unsafeCoin @Int 39) (unsafeCoin @Int 40))
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = [40]
                })

        coinSelectionUnitTest randomImprove ""
            (Left $ InputValueInsufficient $ InputValueInsufficientError
                (unsafeCoin @Int 39) (unsafeCoin @Int 43))
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = [40,1,1,1]
                })

        coinSelectionUnitTest randomImprove ""
            (Left $ InputCountInsufficient $ InputCountInsufficientError 3 4)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,20,17]
                , txOutputs = [40,1,1,1]
                })

    before getSystemDRG $
        describe "Coin selection properties : random algorithm" $ do
            it "forall (UTxO, NonEmpty TxOut), running algorithm gives not \
                \less UTxO fragmentation than LargestFirst algorithm"
                (property . propFragmentation @InputId @OutputId)
            it "forall (UTxO, NonEmpty TxOut), running algorithm gives the \
                \same errors as LargestFirst algorithm"
                (property . propErrors @InputId @OutputId)

    coinSelectionAlgorithmGeneralProperties @InputId @OutputId
        randomImprove "Random-Improve"

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

propFragmentation
    :: (Ord i, Ord o)
    => SystemDRG
    -> CoinSelectionData i o
    -> Property
propFragmentation drg (CoinSelectionData utxo txOuts) = do
    isRight selection1 && isRight selection2 ==>
        let Right (CoinSelectionResult s1 _) = selection1 in
        let Right (CoinSelectionResult s2 _) = selection2 in
        prop (s1, s2)
  where
    prop (CoinSelection inps1 _ _, CoinSelection inps2 _ _) =
        L.length inps1 `shouldSatisfy` (>= L.length inps2)
    (selection1,_) = withDRG drg $
        runExceptT $ selectCoins randomImprove params
    selection2 = runIdentity $ runExceptT $
        selectCoins largestFirst params
    selectionLimit = CoinSelectionLimit $ const 100
    params = CoinSelectionParameters utxo txOuts selectionLimit

propErrors
    :: (Ord i, Ord o, Show i, Show o)
    => SystemDRG
    -> CoinSelectionData i o
    -> Property
propErrors drg (CoinSelectionData utxo txOuts) =
    case resultRandomImprove of
        Right _ ->
            -- Largest-First should always succeed if Random-Improve succeeds.
            counterexample "case: Success"
                $ property
                $ resultLargestFirst `shouldSatisfy` isRight
        Left (InputValueInsufficient _) ->
            -- Largest-First should fail in exactly the same way when the total
            -- value available is insufficient.
            counterexample "case: InputValueInsufficient"
                $ property
                $ resultLargestFirst `shouldBe` resultRandomImprove
        Left (InputCountInsufficient _) ->
            -- Largest-First can still succeed in this case, so just check for
            -- a valid result.
            counterexample "case: InputCountInsufficient"
                $ property
                $ resultLargestFirst `shouldSatisfy` isValidLargestFirstResult
        Left (InputsExhausted _) ->
            -- Largest-First can still succeed in this case, so just check for
            -- a valid result.
            counterexample "case: InputsExhausted"
                $ property
                $ resultLargestFirst `shouldSatisfy` isValidLargestFirstResult
        Left (InputLimitExceeded _) ->
            -- Largest-First can still succeed in this case, so just check for
            -- a valid result.
            counterexample "case: InputLimitExceeded"
                $ property
                $ resultLargestFirst `shouldSatisfy` isValidLargestFirstResult
  where
    isValidLargestFirstResult = \case
        Right _ ->
            -- We assume that this is a valid result, based on the assumption
            -- that test coverage for Largest-First is sufficient.
            True
        Left x ->
            isValidLargestFirstError x
    resultRandomImprove = fst $ withDRG drg $
        runExceptT $ selectCoins randomImprove params
    resultLargestFirst = runIdentity $ runExceptT $
        selectCoins largestFirst params
    selectionLimit = CoinSelectionLimit $ const 1
    params = CoinSelectionParameters utxo txOuts selectionLimit
