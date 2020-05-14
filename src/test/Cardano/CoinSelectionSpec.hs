{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CoinSelectionSpec
    ( spec

    -- * Export used to test various coin selection implementations
    , CoinSelectionFixture (..)
    , CoinSelectionTestResult (..)
    , CoinSelectionData (..)
    , coinSelectionUnitTest
    , coinSelectionAlgorithmGeneralProperties
    ) where

-- | This module contains shared functionality for coin selection tests.
--
-- Coin selection algorithms share a common interface, and therefore it makes
-- sense for them to also share arbitrary instances and tests.

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
    , coinMapToList
    , coinMapValue
    , sumChange
    , sumInputs
    , sumOutputs
    )
import Cardano.Test.Utilities
    ( Address (..), Hash (..), ShowFmt (..), TxIn (..), unsafeCoin )
import Control.Arrow
    ( (&&&) )
import Control.Monad
    ( replicateM )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Either
    ( isRight )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes )
import Data.Set
    ( Set )
import Data.Word
    ( Word16, Word8 )
import Fmt
    ( Buildable (..), blockListF, nameF )
import Internal.Coin
    ( Coin, coinToIntegral )
import Test.Hspec
    ( Expectation, Spec, SpecWith, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Confidence (..)
    , Gen
    , Property
    , arbitraryBoundedIntegral
    , checkCoverage
    , checkCoverageWith
    , choose
    , cover
    , elements
    , generate
    , genericShrink
    , oneof
    , property
    , scale
    , vectorOf
    , withMaxSuccess
    , (.&&.)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Vector.Shuffle
    ( shuffle )

import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Internal.Coin as C
import qualified Test.QuickCheck.Monadic as QC

spec :: Spec
spec = do
    describe "CoinMap properties" $ do
        it "CoinMap coverage is adequate" $
            checkCoverage $ prop_CoinMap_coverage @Int
        it "CoinMapEntry coverage is adequate" $
            checkCoverage $ prop_CoinMapEntry_coverage @Int
        it "coinMapFromList preserves total value for each unique key" $
            checkCoverage $
                prop_coinMapFromList_preservesTotalValueForEachUniqueKey @Int
        it "coinMapFromList preserves total value" $
            checkCoverage $ prop_coinMapFromList_preservesTotalValue @Int
        it "coinMapToList preserves total value" $
            checkCoverage $ prop_coinMapToList_preservesTotalValue @Int
        it "coinMapFromList . coinMapToList = id" $
            checkCoverage $ prop_coinMapToList_coinMapFromList @Int
        it "coinMapToList . coinMapFromList = id (when no keys duplicated)" $
            checkCoverage $ prop_coinMapFromList_coinMapToList @Int
        it "coinMapToList order deterministic" $
            checkCoverageWith lowerConfidence $
                prop_coinMapToList_orderDeterministic @Int

    describe "CoinSelection properties" $ do
        it "monoidal append preserves keys" $
            checkCoverage $ prop_coinSelection_mappendPreservesKeys @Int @Int
        it "monoidal append preserves value" $
            checkCoverage $
                prop_coinSelection_mappendPreservesTotalValue @Int @Int

    describe "CoinSelectionData properties" $ do
        it "CoinSelectionData coverage is adequate" $
            checkCoverage
                $ withMaxSuccess 10_000
                $ prop_CoinSelectionData_coverage @Int @Int

  where
    lowerConfidence :: Confidence
    lowerConfidence = Confidence (10^(6 :: Integer)) 0.75

-- A collection of general properties that should apply to all algorithms
-- that implement the 'CoinSelectionAlgorithm' interface.
--
coinSelectionAlgorithmGeneralProperties
    :: (Arbitrary i, Arbitrary o, Ord i, Ord o, Show i, Show o)
    => CoinSelectionAlgorithm i o IO
    -> String
    -> Spec
coinSelectionAlgorithmGeneralProperties algorithm algorithmName =
    describe ("General properties for " <> algorithmName) $ do
        it "value inputsAvailable ≥ value outputsRequested" $
            check prop_algorithm_inputsAvailable_outputsRequested
        it "outputsSelected = outputsRequested" $
            check prop_algorithm_outputsSelected_outputsRequested
        it "inputsSelected ⊆ inputsAvailable" $
            check prop_algorithm_inputsAvailable_inputsSelected
        it "inputsRemaining ⊆ inputsAvailable" $
            check prop_algorithm_inputsAvailable_inputsRemaining
        it "inputsSelected ⋂ inputsRemaining = ∅" $
            check prop_algorithm_inputsSelected_inputsRemaining
        it "inputsSelected ⋃ inputsRemaining = inputsAvailable" $
            check prop_algorithm_inputsSelected_inputsRemaining_inputsAvailable
        it "value inputsSelected = value outputsSelected + value change" $
            check prop_algorithm_inputsSelected_outputsSelected_change
  where
    check = property . prop_algorithm algorithm

--------------------------------------------------------------------------------
-- Coin Map Properties
--------------------------------------------------------------------------------

prop_CoinMap_coverage
    :: CoinMap u
    -> Property
prop_CoinMap_coverage m = property
    $ cover 10 (null m)
        "coin map is empty"
    $ cover 10 (length m == 1)
        "coin map has one entry"
    $ cover 10 (length m >= 2)
        "coin map has multiple entries"
    $ cover 10 (1 == length (Set.fromList $ entryValue <$> coinMapToList m))
        "coin map has one unique value"
    $ cover 10 (1 < length (Set.fromList $ entryValue <$> coinMapToList m))
        "coin map has several unique values"
    True

prop_CoinMapEntry_coverage :: forall u . Ord u => [CoinMapEntry u] -> Property
prop_CoinMapEntry_coverage entries = property
    $ cover 2 (null entries)
        "coin map entry list is empty"
    $ cover 2 (length entries == 1)
        "coin map entry list has one entry"
    $ cover 2 (length entries == 2)
        "coin map entry list has two entries"
    $ cover 2 (length entries >= 3)
        "coin map entry list has multiple entries"
    $ cover 2 (not (null entries) && length uniqueKeys == 1)
        "coin map entry list has one unique key"
    $ cover 2 (not (null entries) && length uniqueKeys == 2)
        "coin map entry list has two unique keys"
    $ cover 2 (not (null entries) && length uniqueKeys >= 3)
        "coin map entry list has multiple unique keys"
    $ cover 2 (not (null entries) && null duplicateKeys)
        "coin map entry list has no duplicate keys"
    $ cover 2 (not (null entries) && length duplicateKeys == 1)
        "coin map entry list has one duplicate key"
    $ cover 2 (not (null entries) && length duplicateKeys == 2)
        "coin map entry list has two duplicate keys"
    $ cover 2 (not (null entries) && length duplicateKeys >= 3)
        "coin map entry list has multiple duplicate keys"
    True
  where
    uniqueKeys :: Set u
    uniqueKeys = entries
        & fmap entryKey
        & Set.fromList
    duplicateKeys :: Set u
    duplicateKeys = entries
        & fmap (entryKey &&& const (1 :: Int))
        & Map.fromListWith (+)
        & Map.filter (> 1)
        & Map.keysSet

prop_coinMapFromList_preservesTotalValueForEachUniqueKey
    :: (Ord u, Show u)
    => [CoinMapEntry u]
    -> Property
prop_coinMapFromList_preservesTotalValueForEachUniqueKey entries = property $
    mkEntryMap entries
        `shouldBe`
        mkEntryMap (coinMapToList (coinMapFromList entries))
  where
    mkEntryMap
        = Map.fromListWith C.add
        . fmap (entryKey &&& entryValue)

prop_coinMapFromList_preservesTotalValue
    :: Ord u
    => [CoinMapEntry u]
    -> Property
prop_coinMapFromList_preservesTotalValue entries = property $
    mconcat (entryValue <$> entries)
        `shouldBe` coinMapValue (coinMapFromList entries)

prop_coinMapToList_preservesTotalValue
    :: CoinMap u
    -> Property
prop_coinMapToList_preservesTotalValue m = property $
    mconcat (entryValue <$> coinMapToList m)
        `shouldBe` coinMapValue m

prop_coinMapToList_coinMapFromList
    :: (Ord u, Show u)
    => CoinMap u
    -> Property
prop_coinMapToList_coinMapFromList m = property $
    coinMapFromList (coinMapToList m)
        `shouldBe` m

prop_coinMapFromList_coinMapToList
    :: (Ord u, Show u)
    => [CoinMapEntry u]
    -> Property
prop_coinMapFromList_coinMapToList entries
    | duplicateKeyCount == 0 =
        property $ x `shouldBe` y
    | otherwise =
        property $ length x > length y
  where
    x = L.sort entries
    y = L.sort $ coinMapToList $ coinMapFromList entries

    duplicateKeyCount :: Int
    duplicateKeyCount = entries
        & fmap (entryKey &&& const (1 :: Int))
        & Map.fromListWith (+)
        & Map.filter (> 1)
        & Map.keysSet
        & length

prop_coinMapToList_orderDeterministic
    :: Ord u => CoinMap u -> Property
prop_coinMapToList_orderDeterministic u = monadicIO $ QC.run $ do
    let list0 = coinMapToList u
    list1 <- shuffle list0
    return $
        cover 10 (list0 /= list1) "shuffled" $
        list0 == coinMapToList (coinMapFromList list1)

--------------------------------------------------------------------------------
-- Coin Selection Properties
--------------------------------------------------------------------------------

prop_coinSelection_mappendPreservesKeys
    :: (Ord i, Ord o, Show i, Show o)
    => CoinSelection i o
    -> CoinSelection i o
    -> Property
prop_coinSelection_mappendPreservesKeys s1 s2 = property $ do
    Map.keysSet (unCoinMap $ inputs $ s1 <> s2)
        `shouldBe`
        Map.keysSet (unCoinMap $ inputs s1)
        `Set.union`
        Map.keysSet (unCoinMap $ inputs s2)
    Map.keysSet (unCoinMap $ outputs $ s1 <> s2)
        `shouldBe`
        Map.keysSet (unCoinMap $ outputs s1)
        `Set.union`
        Map.keysSet (unCoinMap $ outputs s2)

prop_coinSelection_mappendPreservesTotalValue
    :: (Ord i, Ord o)
    => CoinSelection i o
    -> CoinSelection i o
    -> Property
prop_coinSelection_mappendPreservesTotalValue s1 s2 = property $ do
    sumInputs  s1 <> sumInputs  s2 `shouldBe` sumInputs  (s1 <> s2)
    sumOutputs s1 <> sumOutputs s2 `shouldBe` sumOutputs (s1 <> s2)
    sumChange  s1 <> sumChange  s2 `shouldBe` sumChange  (s1 <> s2)
    change     s1 <> change     s2 `shouldBe` change     (s1 <> s2)

prop_CoinSelectionData_coverage
    :: CoinSelectionData i o
    -> Property
prop_CoinSelectionData_coverage (CoinSelectionData inps outs) = property
    $ cover 90 (amountAvailable >= amountRequested)
        "amountAvailable ≥ amountRequested"
    $
    (amountAvailable `shouldSatisfy` (> C.zero))
    .&&.
    (amountRequested `shouldSatisfy` (> C.zero))
  where
    amountAvailable = coinMapValue inps
    amountRequested = coinMapValue outs

--------------------------------------------------------------------------------
-- Coin Selection Algorithm Properties
--------------------------------------------------------------------------------

prop_algorithm_inputsAvailable_outputsRequested
    :: CoinSelectionData i o
    -> CoinSelectionResult i o
    -> Expectation
prop_algorithm_inputsAvailable_outputsRequested =
    \(CoinSelectionData inputsAvailable outputsRequested) -> const $
        coinMapValue inputsAvailable `shouldSatisfy`
            (>= coinMapValue outputsRequested)

prop_algorithm_outputsSelected_outputsRequested
    :: (Ord o, Show o)
    => CoinSelectionData i o
    -> CoinSelectionResult i o
    -> Expectation
prop_algorithm_outputsSelected_outputsRequested =
    \(CoinSelectionData _ outputsRequested) ->
    \(CoinSelectionResult (CoinSelection _ outputsSelected _) _) ->
        outputsSelected `shouldBe` outputsRequested

prop_algorithm_inputsAvailable_inputsSelected
    :: (Ord i, Show i)
    => CoinSelectionData i o
    -> CoinSelectionResult i o
    -> Expectation
prop_algorithm_inputsAvailable_inputsSelected =
    \(CoinSelectionData inputsAvailable _) ->
    \(CoinSelectionResult (CoinSelection inputsSelected _ _) _) ->
        inputsSelected `shouldSatisfy` (`isSubmapOf` inputsAvailable)

prop_algorithm_inputsAvailable_inputsRemaining
    :: (Ord i, Show i)
    => CoinSelectionData i o
    -> CoinSelectionResult i o
    -> Expectation
prop_algorithm_inputsAvailable_inputsRemaining =
    \(CoinSelectionData inputsAvailable _) ->
    \(CoinSelectionResult _ inputsRemaining) ->
        inputsRemaining `shouldSatisfy` (`isSubmapOf` inputsAvailable)

prop_algorithm_inputsSelected_inputsRemaining
    :: (Ord i, Show i)
    => CoinSelectionData i o
    -> CoinSelectionResult i o
    -> Expectation
prop_algorithm_inputsSelected_inputsRemaining =
    \(CoinSelectionData _ _) ->
    \(CoinSelectionResult (CoinSelection selected _ _) remaining) ->
        (selected `intersection` remaining) `shouldBe` mempty

prop_algorithm_inputsSelected_inputsRemaining_inputsAvailable
    :: (Ord i, Show i)
    => CoinSelectionData i o
    -> CoinSelectionResult i o
    -> Expectation
prop_algorithm_inputsSelected_inputsRemaining_inputsAvailable =
    \(CoinSelectionData available _) ->
    \(CoinSelectionResult (CoinSelection selected _ _) remaining) ->
        (selected `union` remaining) `shouldBe` available

prop_algorithm_inputsSelected_outputsSelected_change
    :: CoinSelectionData i o
    -> CoinSelectionResult i o
    -> Expectation
prop_algorithm_inputsSelected_outputsSelected_change = const $
    \(CoinSelectionResult (CoinSelection {inputs, outputs, change}) _) ->
        coinMapValue inputs
            `shouldBe`
            (coinMapValue outputs `C.add` mconcat change)

prop_algorithm
    :: CoinSelectionAlgorithm i o IO
    -> (CoinSelectionData i o -> CoinSelectionResult i o -> Expectation)
    -> (CoinSelectionData i o)
    -> Property
prop_algorithm algorithm verifyExpectation params =
    withMaxSuccess 1_000 $ monadicIO $ QC.run $ do
        mResult <- generateResult
        pure $ isRight mResult ==>
            let Right result = mResult in
            verifyExpectation params result
  where
    CoinSelectionData {csdInputsAvailable, csdOutputsRequested} = params
    generateResult = runExceptT
        $ selectCoins algorithm
        $ CoinSelectionParameters csdInputsAvailable csdOutputsRequested
        $ CoinSelectionLimit
        $ const $ fromIntegral $ F.length csdInputsAvailable

isSubmapOf :: Ord k => CoinMap k -> CoinMap k -> Bool
isSubmapOf (CoinMap a) (CoinMap b) = a `Map.isSubmapOf` b

intersection :: Ord k => CoinMap k -> CoinMap k -> CoinMap k
intersection (CoinMap a) (CoinMap b) = CoinMap $ a `Map.intersection` b

union :: Ord k => CoinMap k -> CoinMap k -> CoinMap k
union = (<>)

--------------------------------------------------------------------------------
-- Coin Selection - Unit Tests
--------------------------------------------------------------------------------

-- | Data for coin selection properties.
data CoinSelectionData i o = CoinSelectionData
    { csdInputsAvailable
        :: CoinMap i
    , csdOutputsRequested
        :: CoinMap o
    } deriving Show

instance (Buildable i, Buildable o) => Buildable (CoinSelectionData i o) where
    build (CoinSelectionData inps outs) = mempty
        <> nameF "inps" (blockListF $ coinMapToList inps)
        <> nameF "outs" (blockListF $ coinMapToList outs)

-- | A fixture for testing the coin selection
data CoinSelectionFixture i o = CoinSelectionFixture
    { maxNumOfInputs :: Word16
        -- ^ Maximum number of inputs that can be selected
    , utxoInputs :: [Integer]
        -- ^ Value (in Lovelace) & number of available coins in the UTxO
    , txOutputs :: [Integer]
        -- ^ Value (in Lovelace) & number of requested outputs
    }

-- | Testing-friendly format for 'CoinSelection' results of unit tests.
data CoinSelectionTestResult = CoinSelectionTestResult
    { rsInputs :: [Integer]
    , rsChange :: [Integer]
    , rsOutputs :: [Integer]
    } deriving (Eq, Show)

sortCoinSelectionTestResult
    :: CoinSelectionTestResult -> CoinSelectionTestResult
sortCoinSelectionTestResult (CoinSelectionTestResult is cs os) =
    CoinSelectionTestResult (L.sort is) (L.sort cs) (L.sort os)

-- | Generate a 'UTxO' and 'TxOut' matching the given 'Fixture', and perform
-- the given coin selection on it.
coinSelectionUnitTest
    :: CoinSelectionAlgorithm TxIn Address IO
    -> String
    -> Either CoinSelectionError CoinSelectionTestResult
    -> CoinSelectionFixture TxIn Address
    -> SpecWith ()
coinSelectionUnitTest alg lbl expected (CoinSelectionFixture n utxoF outsF) =
    it title $ do
        (utxo,txOuts) <- setup
        result <- runExceptT $ do
            CoinSelectionResult (CoinSelection inps outs chngs) _ <-
                selectCoins alg $
                    CoinSelectionParameters utxo txOuts selectionLimit
            return $ CoinSelectionTestResult
                { rsInputs = coinToIntegral . entryValue <$> coinMapToList inps
                , rsChange = coinToIntegral <$> chngs
                , rsOutputs = coinToIntegral . entryValue <$> coinMapToList outs
                }
        fmap sortCoinSelectionTestResult result
            `shouldBe` fmap sortCoinSelectionTestResult expected
  where
    selectionLimit = CoinSelectionLimit $ const n

    title :: String
    title = mempty
        <> if null lbl then "" else lbl <> ":\n\t"
        <> "max=" <> show n
        <> ", UTxO=" <> show utxoF
        <> ", Output=" <> show outsF
        <> " --> " <> show (rsInputs <$> expected)

    setup :: IO (CoinMap TxIn, CoinMap Address)
    setup = do
        utxo <- generate (genUTxO utxoF)
        outs <- generate (genOutputs outsF)
        pure (utxo, outs)

--------------------------------------------------------------------------------
-- Arbitrary Instances
--------------------------------------------------------------------------------

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

instance (Arbitrary i, Arbitrary o, Ord i, Ord o) =>
    Arbitrary (CoinSelection i o)
  where
    arbitrary = CoinSelection
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
    shrink = genericShrink

instance Arbitrary (CoinSelectionLimit) where
    arbitrary = do
        -- NOTE Functions have to be decreasing functions
        fn <- elements
            [ (maxBound -)
            , \x ->
                if x > maxBound `div` 2
                    then maxBound
                    else maxBound - (2 * x)
            , const 42
            ]
        pure $ CoinSelectionLimit fn

instance Show (CoinSelectionLimit) where
    show _ = "CoinSelectionLimit"

instance Arbitrary a => Arbitrary (NonEmpty a) where
    shrink xs = catMaybes (NE.nonEmpty <$> shrink (NE.toList xs))
    arbitrary = do
        n <- choose (1, 10)
        NE.fromList <$> vectorOf n arbitrary

instance (Arbitrary i, Arbitrary o, Ord i, Ord o) =>
    Arbitrary (CoinSelectionData i o)
  where
    shrink (CoinSelectionData inps outs) = uncurry CoinSelectionData <$> zip
        (shrink inps)
        (coinMapFromList <$> filter (not . null) (shrink (coinMapToList outs)))
    arbitrary =
        CoinSelectionData <$> genInps <*> genOuts
      where
        -- Incorporate a bias towards being able to pay for all outputs.
        genInps = genCoinMap 256
        genOuts = genCoinMap 4

        genCoinMap :: forall k . (Arbitrary k, Ord k) => Int -> Gen (CoinMap k)
        genCoinMap maxEntryCount = do
            count <- choose (1, maxEntryCount)
            coinMapFromList <$> replicateM count genCoinMapEntry

        genCoinMapEntry :: forall k . Arbitrary k => Gen (CoinMapEntry k)
        genCoinMapEntry = CoinMapEntry <$> arbitrary <*> genCoin

        -- Generate coins with a reasonably high chance of size collisions.
        genCoin :: Gen Coin
        genCoin = unsafeCoin @Int <$> choose (1, 16)

instance Arbitrary Address where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> vectorOf 8 arbitraryBoundedIntegral
        pure $ Address bytes

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = unsafeCoin @Int <$> choose (1, 100000)

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary (Hash "Tx") where
    -- No Shrinking
    arbitrary = do
        wds <- vectorOf 10 arbitrary :: Gen [Word8]
        let bs = BS.pack wds
        pure $ Hash bs

instance Arbitrary a => Arbitrary (CoinMapEntry a) where
    -- No Shrinking
    arbitrary = CoinMapEntry
        <$> arbitrary
        <*> arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (CoinMap a) where
    shrink (CoinMap m) = CoinMap <$> shrink m
    arbitrary = do
        n <- oneof
            [ pure 0
            , pure 1
            , choose (2, 100)
            ]
        entries <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ CoinMap $ Map.fromList entries

genUTxO :: [Integer] -> Gen (CoinMap TxIn)
genUTxO coins = do
    let n = length coins
    inps <- vectorOf n arbitrary
    return $ CoinMap $ Map.fromList $ zip inps (unsafeCoin <$> coins)

genOutputs :: [Integer] -> Gen (CoinMap Address)
genOutputs coins = do
    let n = length coins
    outs <- vectorOf n arbitrary
    return $ coinMapFromList $ zipWith CoinMapEntry outs (map unsafeCoin coins)
