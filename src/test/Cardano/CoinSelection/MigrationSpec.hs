{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CoinSelection.MigrationSpec
    ( spec
    ) where

import Prelude

import Cardano.CoinSelection
    ( Coin (..)
    , CoinMapEntry (..)
    , CoinSelection (..)
    , UTxO (..)
    , changeBalance
    , coinMapToList
    , inputBalance
    , utxoBalance
    )
import Cardano.CoinSelection.Fee
    ( DustThreshold (..), Fee (..), FeeEstimator (..), FeeOptions (..) )
import Cardano.CoinSelection.FeeSpec
    ()
import Cardano.CoinSelection.Migration
    ( depleteUTxO, idealBatchSize )
import Cardano.CoinSelectionSpec
    ()
import Cardano.Test.Utilities
    ( Address, Hash (..), TxIn (..) )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Word
    ( Word8 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , conjoin
    , counterexample
    , frequency
    , label
    , property
    , scale
    , vectorOf
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Monadic
    ( monadicIO, monitor, pick )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "idealBatchSize" $ do
        it "Eventually converge for decreasing functions" $ do
            property $ \coinselOpts -> do
                let batchSize = idealBatchSize coinselOpts
                label (show batchSize) True

    describe "accuracy of depleteUTxO" $ do
        let testAccuracy :: Double -> SpecWith ()
            testAccuracy r = it title $ withMaxSuccess 1000 $ monadicIO $ do
                let dust = Coin 100
                utxo <- pick (genUTxO r dust)
                batchSize <- pick genBatchSize
                feeOpts <- pick (genFeeOptions dust)
                let selections = depleteUTxO feeOpts batchSize utxo
                monitor $ label $ accuracy dust
                    (utxoBalance utxo)
                    (fromIntegral $ sum $ inputBalance <$> selections)
              where
                title :: String
                title = "dust=" <> show (round (100 * r) :: Int) <> "%"

                accuracy :: Coin -> Natural -> Natural -> String
                accuracy (Coin dust) sup real
                    | a >= 1.0 =
                        "PERFECT  (== 100%)"
                    | a > 0.99 || (sup - real) < fromIntegral dust =
                        "OKAY     (>   99%)"
                    | otherwise =
                        "MEDIOCRE (<=  99%)"
                  where
                    a = double real / double sup
                    double = fromRational @Double . fromIntegral

        mapM_ testAccuracy [ 0.01 , 0.05 , 0.10 , 0.25 , 0.50 ]

    describe "depleteUTxO properties" $ do
        it "No coin selection has outputs" $
            property $ withMaxSuccess 1000 $ prop_onlyChangeOutputs
                @(Wrapped TxIn) @Address

        it "Every coin in the selection change >= minimum threshold coin" $
            property $ withMaxSuccess 1000 $ prop_noLessThanThreshold
                @(Wrapped TxIn) @Address

        it "Total input UTxO value >= sum of selection change coins" $
            property $ withMaxSuccess 1000 $ prop_inputsGreaterThanOutputs
                @(Wrapped TxIn) @Address

        it "Every selection input is unique" $
            property $ withMaxSuccess 1000 $ prop_inputsAreUnique
                @(Wrapped TxIn) @Address

        it "Every selection input is a member of the UTxO" $
            property $ withMaxSuccess 1000 $ prop_inputsStillInUTxO
                @(Wrapped TxIn) @Address

        it "Every coin selection is well-balanced" $
            property $ withMaxSuccess 1000 $ prop_wellBalanced
                @(Wrapped TxIn) @Address

    describe "depleteUTxO regressions" $ do
        it "regression #1" $ do
            let feeOpts = FeeOptions
                    { dustThreshold = DustThreshold 9
                    , feeEstimator = FeeEstimator $ \s -> Fee
                        $ fromIntegral
                        $ 5 * (length (inputs s) + length (outputs s))
                    }
            let batchSize = 1
            let utxo = UTxO $ Map.fromList
                    [ ( TxIn
                        { txinId = Hash "|\243^\SUBg\242\231\&1\213\203"
                        , txinIx = 2
                        }
                      , Coin 2
                      )
                    ]
            property $ prop_inputsGreaterThanOutputs
                @TxIn @Address feeOpts batchSize utxo

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

-- | No coin selection has outputs
prop_onlyChangeOutputs
    :: forall i o u . (i ~ u, Ord o, Ord u, Show o)
    => FeeOptions i o
    -> Word8
    -> UTxO u
    -> Property
prop_onlyChangeOutputs feeOpts batchSize utxo = do
    let allOutputs =
            coinMapToList . outputs =<< depleteUTxO feeOpts batchSize utxo
    property (allOutputs `shouldSatisfy` null)

-- | Every coin in the selection change >= minimum threshold coin
prop_noLessThanThreshold
    :: forall i o u . (i ~ u, Ord o, Ord u)
    => FeeOptions i o
    -> Word8
    -> UTxO u
    -> Property
prop_noLessThanThreshold feeOpts batchSize utxo = do
    let allChange = change
            =<< depleteUTxO feeOpts batchSize utxo
    let undersizedCoins =
            filter (< threshold) allChange
    property (undersizedCoins `shouldSatisfy` null)
  where
    threshold = Coin $ getDustThreshold $ dustThreshold feeOpts

-- | Total input UTxO value >= sum of selection change coins
prop_inputsGreaterThanOutputs
    :: forall i o u . (i ~ u, Ord o, Ord u, Show o, Show u)
    => FeeOptions i o
    -> Word8
    -> UTxO u
    -> Property
prop_inputsGreaterThanOutputs feeOpts batchSize utxo = do
    let selections  = depleteUTxO feeOpts batchSize utxo
    let totalChange = sum (changeBalance <$> selections)
    let balanceUTxO = utxoBalance utxo
    property (balanceUTxO >= fromIntegral totalChange)
        & counterexample ("Total change balance: " <> show totalChange)
        & counterexample ("Total UTxO balance: " <> show balanceUTxO)
        & counterexample ("Selections: " <> show selections)

-- | Every selected input is unique, i.e. selected only once
prop_inputsAreUnique
    :: forall i o u . (i ~ u, Ord o, Ord u)
    => FeeOptions i o
    -> Word8
    -> UTxO u
    -> Property
prop_inputsAreUnique feeOpts batchSize utxo = do
    let selectionInputList =
            coinMapToList . inputs =<< depleteUTxO feeOpts batchSize utxo
    let selectionInputSet =
            Set.fromList selectionInputList
    Set.size selectionInputSet === length selectionInputSet

-- | Every selection input is still a member of the UTxO" $
prop_inputsStillInUTxO
    :: forall i o u . (i ~ u, Ord o, Ord u)
    => FeeOptions i o
    -> Word8
    -> UTxO u
    -> Property
prop_inputsStillInUTxO feeOpts batchSize utxo = do
    let selectionInputSet = Set.fromList $
            coinMapToList . inputs =<< depleteUTxO feeOpts batchSize utxo
    let utxoSet = Set.fromList $
            fmap (uncurry CoinMapEntry) $ Map.toList $ getUTxO utxo
    property (selectionInputSet `Set.isSubsetOf` utxoSet)

-- | Every coin selection is well-balanced (i.e. actual fees are exactly the
-- expected fees)
prop_wellBalanced
    :: forall i o u . (i ~ u, Ord o, Ord u, Show o, Show u)
    => FeeOptions i o
    -> Word8
    -> UTxO u
    -> Property
prop_wellBalanced feeOpts batchSize utxo = do
    let selections = depleteUTxO feeOpts batchSize utxo
    conjoin
        [ counterexample example (actualFee === expectedFee)
        | s <- selections
        , let actualFee = inputBalance s - changeBalance s
        , let (Fee expectedFee) = estimateFee (feeEstimator feeOpts) s
        , let example = unlines
                [ "Coin Selection: " <> show s
                , "Actual fee: " <> show actualFee
                , "Expected fee: " <> show expectedFee
                ]
        ]

--------------------------------------------------------------------------------
-- Arbitrary Instances
--------------------------------------------------------------------------------

-- A wrapper to avoid overlapping instances imported from other modules.
newtype Wrapped a = Wrapped { unwrap :: a }
    deriving (Eq, Ord, Show)

-- TODO: Move similar Arbitrary instances to a shared module for better reuse.
instance Arbitrary (Wrapped TxIn) where
    arbitrary = fmap Wrapped . TxIn
        <$> fmap unwrap arbitrary
        <*> scale (`mod` 3) arbitrary

-- TODO: Move similar Arbitrary instances to a shared module for better reuse.
instance Arbitrary (Wrapped (Hash "Tx")) where
    arbitrary = Wrapped . Hash <$> (BS.pack <$> vectorOf 32 arbitrary)

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genBatchSize :: Gen Word8
genBatchSize = choose (50, 150)

genFeeOptions :: Coin -> Gen (FeeOptions TxIn Address)
genFeeOptions (Coin dust) = do
    pure $ FeeOptions
        { feeEstimator = FeeEstimator $ \s ->
            let x = fromIntegral (length (inputs s) + length (outputs s))
            in Fee $ (dust `div` 100) * x + dust
        , dustThreshold = DustThreshold dust
        }

-- | Generate a given UTxO with a particular percentage of dust
genUTxO :: Double -> Coin -> Gen (UTxO TxIn)
genUTxO r (Coin dust) = do
    n <- choose (10, 1000)
    inps <- genTxIn n
    coins <- vectorOf n genCoin
    pure $ UTxO $ Map.fromList $ zip inps coins
  where
    genTxIn :: Int -> Gen [TxIn]
    genTxIn n = do
        ids <- vectorOf n (Hash <$> genBytes 8)
        ixs <- vectorOf n arbitrary
        pure $ zipWith TxIn ids ixs

    genBytes :: Int -> Gen ByteString
    genBytes n = B8.pack <$> vectorOf n arbitrary

    genCoin :: Gen Coin
    genCoin = Coin <$> frequency
        [ (round (100*r), choose (1, dust))
        , (round (100*(1-r)), choose (dust, 1000*dust))
        ]
