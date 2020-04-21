{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CoinSelection.FeeSpec
    ( spec
    ) where

import Prelude hiding
    ( round )

import Cardano.CoinSelection
    ( CoinMap (..)
    , CoinMapEntry (..)
    , CoinSelection (..)
    , CoinSelectionAlgorithm (..)
    , coinMapFromList
    , coinMapToList
    , sumChange
    , sumInputs
    , sumOutputs
    )
import Cardano.CoinSelection.Fee
    ( DustThreshold (..)
    , Fee (..)
    , FeeAdjustmentError (..)
    , FeeEstimator (..)
    , FeeOptions (..)
    , adjustForFee
    , coalesceDust
    , distributeFee
    , reduceChangeOutputs
    , splitCoin
    )
import Cardano.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Test.Utilities
    ( Address (..)
    , Hash (..)
    , ShowFmt (..)
    , TxIn (..)
    , unsafeCoin
    , unsafeDustThreshold
    , unsafeFee
    )
import Control.Arrow
    ( left )
import Control.Monad
    ( replicateM )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Random
    ( SystemDRG, getSystemDRG )
import Crypto.Random.Types
    ( withDRG )
import Data.Either
    ( isRight )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Monoid
    ( All (..) )
import Data.Ratio
    ( (%) )
import Fmt
    ( Buildable (..), nameF, tupleF )
import GHC.Generics
    ( Generic )
import Internal.Coin
    ( Coin, coinToIntegral )
import Internal.Rounding
    ( RoundingDirection (..), round )
import Test.Hspec
    ( Spec, SpecWith, before, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Positive (getPositive)
    , Property
    , arbitraryBoundedIntegral
    , checkCoverage
    , choose
    , cover
    , coverTable
    , disjoin
    , elements
    , expectFailure
    , generate
    , genericShrink
    , oneof
    , property
    , scale
    , tabulate
    , vectorOf
    , withMaxSuccess
    , (.&&.)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Cardano.CoinSelection as CS
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Internal.Coin as C

spec :: Spec
spec = do
    describe "Fee calculation : unit tests" $ do
        -- Change covers fee exactly, single change output
        feeUnitTest (FeeFixture
            { fInps = [20]
            , fOuts = [17]
            , fChngs = [3]
            , fUtxo = []
            , fFee = 3
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20]
            , csOuts = [17]
            , csChngs = []
            })

        -- Total change covers fee, multiple change outputs
        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [16,18]
            , fChngs = [4,2]
            , fUtxo = []
            , fFee = 6
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [16,18]
            , csChngs = []
            })

        -- Fee split evenly across change outputs
        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [18,18]
            , fChngs = [2,2]
            , fUtxo = []
            , fFee = 2
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [18,18]
            , csChngs = [1,1]
            })

        -- Fee split evenly across change outputs
        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [17,18]
            , fChngs = [3,2]
            , fUtxo = []
            , fFee = 2
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [17,18]
            , csChngs = [2,1]
            })

        -- Fee divvied, dust removed (dust = 0)
        feeUnitTest (FeeFixture
            { fInps = [20,20,20]
            , fOuts = [14,18,19]
            , fChngs = [6,2,1]
            , fUtxo = []
            , fFee = 3
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20,20]
            , csOuts = [14,18,19]
            , csChngs = [4,1,1]
            })

        -- Fee divvied, dust removed (dust = 1)
        feeUnitTest (FeeFixture
            { fInps = [20,20,20]
            , fOuts = [14,18,19]
            , fChngs = [6,2,1]
            , fUtxo = []
            , fFee = 3
            , fDust = 1
            }) (Right $ FeeOutput
            { csInps = [20,20,20]
            , csOuts = [14,18,19]
            , csChngs = [6]
            })

        -- Cannot cover fee, no extra inputs
        feeUnitTest (FeeFixture
            { fInps = [20]
            , fOuts = [17]
            , fChngs = [3]
            , fUtxo = []
            , fFee = 4
            , fDust = 0
            }) (Left $ ErrCannotCoverFee $ unsafeFee @Int 1)

        -- Cannot cover fee even with an extra (too small) inputs
        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fUtxo = [1]
            , fFee = 5
            , fDust = 0
            }) (Left $ ErrCannotCoverFee $ unsafeFee @Int 1)

        -- Can select extra inputs to exactly cover fee, no change back
        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fUtxo = [1,1]
            , fFee = 5
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,1,1]
            , csOuts = [7]
            , csChngs = []
            })

        -- Can select extra inputs to cover for fee, and leave a change back
        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fUtxo = [3]
            , fFee = 5
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,3]
            , csOuts = [7]
            , csChngs = [1]
            })

        -- Multiple change output, can select extra inputs to cover fee, no
        -- change
        feeUnitTest (FeeFixture
            { fInps = [10,10]
            , fOuts = [7,7]
            , fChngs = [3,3]
            , fUtxo = [2,2]
            , fFee = 10
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,10,2,2]
            , csOuts = [7,7]
            , csChngs = []
            })

        -- Multiple outputs, extra inputs selected, resulting change
        feeUnitTest (FeeFixture
            { fInps = [10,10]
            , fOuts = [7,7]
            , fChngs = [3,3]
            , fUtxo = [3,3]
            , fFee = 10
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,10,3,3]
            , csOuts = [7,7]
            , csChngs = [1,1]
            })

        -- Multiple change outputs, some bigger than actual Dust
        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [16,18]
            , fChngs = [4,2]
            , fUtxo = []
            , fFee = 6
            , fDust = 2
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [16,18]
            , csChngs = []
            })

        -- Change created when there was no change before
        feeUnitTest (FeeFixture
            { fInps = [1]
            , fOuts = [1]
            , fChngs = []
            , fUtxo = [2]
            , fFee = 1
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [1,2]
            , csOuts = [1]
            , csChngs = [1]
            })

        feeUnitTest (FeeFixture
            { fInps = []
            , fOuts = []
            , fChngs = []
            , fUtxo = [3]
            , fFee = 3
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [3]
            , csOuts = []
            , csChngs = []
            })

        feeUnitTest (FeeFixture
            { fInps = []
            , fOuts = []
            , fChngs = []
            , fUtxo = [2,2]
            , fFee = 3
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [2,2]
            , csOuts = []
            , csChngs = [1]
            })

        feeUnitTest (FeeFixture
            { fInps = []
            , fOuts = []
            , fChngs = []
            , fUtxo = [2,2]
            , fFee = 3
            , fDust = 1
            }) (Right $ FeeOutput
            { csInps = [2,2]
            , csOuts = []
            , csChngs = [1]
            })

    describe "Fee Calculation: Generators" $ do
        it "Arbitrary CoinSelection" $ property $ \(ShowFmt cs) ->
            property $ isValidSelection @TxIn @Address cs

    before getSystemDRG $ describe "Fee Adjustment properties" $ do
        it "Fee adjustment is deterministic when there's no extra inputs"
            (\_ -> property $ propDeterministic @TxIn @Address)
        it "Adjusting for fee (/= 0) reduces the change outputs or increase \
            \inputs"
            (property . propReducedChanges @TxIn @Address)

    describe "distributeFee" $ do
        it "fee portions are all within unity of ideal unrounded portions"
            (checkCoverage propDistributeFeeFair)
        it "fee portions are allocated optimally"
            (checkCoverage propDistributeFeeOptimal)
        it "Σ fst (distributeFee fee outs) == fee"
            (checkCoverage propDistributeFeeSame)
        it "snd (distributeFee fee outs) == outs"
            (checkCoverage propDistributeFeeOuts)
        it "expectFailure: not (any null (fst <$> distributeFee fee outs))"
            (expectFailure propDistributeFeeNoNullFee)

    describe "coalesceDust" $ do
        it "sum coins = sum (coalesceDust threshold coins)"
            (checkCoverage propCoalesceDustPreservesSum)
        it "all (/= Coin 0) (coalesceDust threshold coins)"
            (checkCoverage propCoalesceDustLeavesNoZeroCoins)
        it "leaves at most one dust coin"
            (checkCoverage propCoalesceDustLeavesAtMostOneDustCoin)
        it "length coins >= (coalesceDust threshold coins)"
            (checkCoverage propCoalesceDustNeverLengthensList)

    describe "reduceChangeOutputs" $ do
        it "data coverage is adequate"
            (checkCoverage propReduceChangeOutputsDataCoverage)
        it "preserves sum"
            (checkCoverage propReduceChangeOutputsPreservesSum)

    describe "splitCoin" $ do
        it "data coverage is adequate"
            (checkCoverage propSplitCoinDataCoverage)
        it "preserves the total sum"
            (checkCoverage propSplitCoinPreservesSum)
        it "results are all within unity of ideal unrounded results"
            (checkCoverage propSplitCoinFair)

--------------------------------------------------------------------------------
-- Fee Adjustment - Properties
--------------------------------------------------------------------------------

-- Check whether a selection is valid
isValidSelection :: CoinSelection i o -> Bool
isValidSelection s =
    sumInputs s >= sumOutputs s `C.add` sumChange s

-- | Data for running fee calculation properties
data FeeProp i o = FeeProp
    { selection :: CoinSelection i o
     -- ^ inputs from wich largestFirst can be calculated
    , availableUtxo :: CoinMap i
     -- ^ additional UTxO from which fee calculation will pick needed coins
    , feeDust :: (Integer, Integer)
     -- ^ constant fee and dust threshold
    } deriving Show

instance (Buildable i, Buildable o) =>
    Buildable (FeeProp i o) where
        build (FeeProp cc utxo opt) = mempty
            <> nameF "selection" (build cc)
            <> nameF "utxo" (build (coinMapToList utxo))
            <> nameF "options" (tupleF opt)

propDeterministic
    :: forall i o . (Ord i, Ord o, Show i, Show o)
    => ShowFmt (FeeProp i o)
    -> Property
propDeterministic (ShowFmt (FeeProp coinSel _ (fee, dust))) =
    monadicIO $ liftIO $ do
        let feeOpt = feeOptions fee dust
        let utxo = mempty
        resultOne <- runExceptT $ adjustForFee feeOpt utxo coinSel
        resultTwo <- runExceptT $ adjustForFee feeOpt utxo coinSel
        resultOne `shouldBe` resultTwo

propReducedChanges
    :: forall i o . (Show i, Show o, Ord i)
    => SystemDRG
    -> ShowFmt (FeeProp i o)
    -> Property
propReducedChanges drg (ShowFmt (FeeProp coinSel utxo (fee, dust))) = do
    isRight coinSel' ==> let Right s = coinSel' in prop s
  where
    prop s = do
        let chgs' = F.fold $ change s
        let chgs = F.fold $ change coinSel
        let inps' = CS.inputs s
        let inps = CS.inputs coinSel
        disjoin
            [ chgs' `shouldSatisfy` (<= chgs)
            , length inps' `shouldSatisfy` (>= length inps)
            ]
    feeOpt = feeOptions fee dust
    coinSel' = left show $ fst $ withDRG drg $ runExceptT $
        adjustForFee feeOpt utxo coinSel

--------------------------------------------------------------------------------
-- distributeFee - Properties
--------------------------------------------------------------------------------

-- | Helper to re-apply the pre-conditions for distributeFee
propDistributeFee
    :: ((Fee, NonEmpty Coin) -> Property)
    -> (Fee, NonEmpty Coin)
    -> Property
propDistributeFee prop (fee, outs) =
    coverTable "properties"
        [ ("fee > 0", 50)
        , ("nOuts=1", 1)
        , ("nOuts=2", 1)
        , ("nOuts=2+", 10)
        ]
        $ tabulate "properties"
            [ if fee > Fee C.zero then "fee > 0" else "fee == 0"
            , "nOuts=" <> case length outs of
                n | n <= 2 -> show n
                _ -> "2+"
            ]
        $ prop (fee, outs)

-- | Verify that fees are distributed fairly across outputs, so that every
--   rounded fee portion is always within unity of the ideal unrounded fee
--   portion.
propDistributeFeeFair
    :: (Fee, NonEmpty Coin)
    -> Property
propDistributeFeeFair (fee, coins) = (.&&.)
    (F.all (uncurry (<=)) (NE.zip fees feeUpperBounds))
    (F.all (uncurry (>=)) (NE.zip fees feeLowerBounds))
  where
    fees = fst <$> distributeFee fee coins

    feeUpperBounds = unsafeFee @Integer . ceiling . computeIdealFee <$> coins
    feeLowerBounds = unsafeFee @Integer . floor   . computeIdealFee <$> coins

    computeIdealFee :: Coin -> Rational
    computeIdealFee c
        = coinToIntegral c
        * coinToIntegral (unFee fee)
        % coinToIntegral (F.fold coins)

-- | Verify that fees are distributed optimally across coins, such that the
-- absolute deviation from the ideal (unrounded) fee distribution is minimal.
propDistributeFeeOptimal
    :: Fee
    -> NonEmpty Coin
    -> Property
propDistributeFeeOptimal fee coins = property $
    computeDeviation (fst <$> distributeFee fee coins)
        `shouldBe` minimumPossibleDeviation
  where
    -- Compute the deviation of a fee portion distribution from the ideal
    -- unrounded fee portion distribution.
    computeDeviation :: NonEmpty Fee -> Rational
    computeDeviation feesRounded = F.sum $
        NE.zipWith zipper
            feesRounded
            idealUnroundedDistribution
      where
        zipper :: Fee -> Rational -> Rational
        zipper (Fee roundedFee) unroundedFee = abs $
            fromIntegral (coinToIntegral @Integer roundedFee) - unroundedFee

    -- The minimum deviation across all possible distributions for the given
    -- fee and set of coins.
    minimumPossibleDeviation :: Rational
    minimumPossibleDeviation =
        F.minimum $ computeDeviation <$> allPossibleDistributions

    -- The set of all possible fee distributions for the given fee and coins.
    allPossibleDistributions :: [NonEmpty Fee]
    allPossibleDistributions = filter isValidDistribution $ NE.zipWith
        (\f roundDir ->
            unsafeFee @Integer $ fromIntegral @Integer $ round roundDir f)
        idealUnroundedDistribution <$> allPossibleRoundings
      where
        -- Indicates whether the given distribution has the correct total fee.
        isValidDistribution :: NonEmpty Fee -> Bool
        isValidDistribution r = F.fold r == fee

        -- All possible ways to round an unrounded fee distribution.
        allPossibleRoundings :: [NonEmpty RoundingDirection]
        allPossibleRoundings = traverse (const [RoundUp, RoundDown]) coins

    -- The ideal unrounded fee distribution.
    idealUnroundedDistribution :: NonEmpty Rational
    idealUnroundedDistribution = computeIdealFee <$> coins
      where
        computeIdealFee :: Coin -> Rational
        computeIdealFee c
            = coinToIntegral c
            * coinToIntegral (unFee fee)
            % coinToIntegral (F.fold coins)

-- | Sum of the fees divvied over each output is the same as the initial total
-- fee.
propDistributeFeeSame
    :: (Fee, NonEmpty Coin)
    -> Property
propDistributeFeeSame = propDistributeFee $ \(fee, outs) ->
    F.fold (fst <$> distributeFee fee outs) === fee

-- | distributeFee doesn't change any of the outputs
propDistributeFeeOuts
    :: (Fee, NonEmpty Coin)
    -> Property
propDistributeFeeOuts = propDistributeFee $ \(fee, outs) ->
    (snd <$> distributeFee fee outs) === outs

-- | distributeFee never generates null fees for a given output.
--
-- This is NOT a property. It is here to illustrate that this can happen in
-- practice, and is known as a possible outcome for the distributeFee function
-- (it is fine for one of the output to be assigned no fee). The only reason
-- this would happen is because there would be less outputs than the fee amount
-- which is probably never going to happen in practice...
propDistributeFeeNoNullFee
    :: (Fee, NonEmpty Coin)
    -> Property
propDistributeFeeNoNullFee (fee, outs) =
    not (null outs) ==> withMaxSuccess 100000 prop
  where
    prop = property $ Fee C.zero `F.notElem` (fst <$> distributeFee fee outs)

--------------------------------------------------------------------------------
-- coalesceDust - Properties
--------------------------------------------------------------------------------

data CoalesceDustData = CoalesceDustData
    { cddThreshold :: DustThreshold
    , cddCoins :: NonEmpty Coin
    } deriving (Eq, Generic, Show)

instance Arbitrary CoalesceDustData where
    arbitrary = do
        coinCount <- genCoinCount
        coins <- (:|) <$> genCoin <*> replicateM coinCount genCoin
        threshold <- DustThreshold <$> oneof
            [ -- Two possibilities:
              genCoin
              -- ^ A completely fresh coin.
            , elements (F.toList coins)
              -- ^ A coin picked from the existing coin set.
            ]
        pure $ CoalesceDustData threshold coins
      where
        genCoin = unsafeCoin @Int <$> oneof [pure 0, choose (1, 100)]
        genCoinCount = choose (0, 10)
    shrink = genericShrink

propCoalesceDustPreservesSum :: CoalesceDustData -> Property
propCoalesceDustPreservesSum (CoalesceDustData threshold coins) =
    property $
    let total = F.fold coins in
    cover 8 (total == C.zero) "sum coins = 0" $
    cover 8 (total /= C.zero) "sum coins ≠ 0" $
    total == F.fold (coalesceDust threshold coins)

propCoalesceDustLeavesNoZeroCoins :: CoalesceDustData -> Property
propCoalesceDustLeavesNoZeroCoins (CoalesceDustData threshold coins) =
    property $
    cover 4 (F.all  (== C.zero) coins) "∀ coin ∈ coins . coin = 0" $
    cover 4 (F.elem    (C.zero) coins) "∃ coin ∈ coins . coin = 0" $
    cover 8 (F.notElem (C.zero) coins) "∀ coin ∈ coins . coin > 0" $
    F.notElem C.zero $ coalesceDust threshold coins

propCoalesceDustLeavesAtMostOneDustCoin :: CoalesceDustData -> Property
propCoalesceDustLeavesAtMostOneDustCoin (CoalesceDustData threshold coins) =
    property $
    let result = coalesceDust threshold coins in
    -- Check that we cover different kinds of extreme threshold conditions:
    cover 2 (F.all (<  threshold') coins) "∀ coin ∈ coins . coin < threshold" $
    cover 2 (F.all (>  threshold') coins) "∀ coin ∈ coins . coin > threshold" $
    cover 2 (F.all (== threshold') coins) "∀ coin ∈ coins . coin = threshold" $
    cover 2 (F.all (/= threshold') coins) "∀ coin ∈ coins . coin ≠ threshold" $
    -- Check that we cover typical threshold conditions:
    let haveMixture = getAll $ mconcat $ All <$>
          [ F.any (<  threshold') coins
          , F.any (== threshold') coins
          , F.any (>  threshold') coins
          ] in
    cover 8 haveMixture "have mixture of coin values in relation to threshold" $
    -- Check that we cover different result lengths:
    cover 8 (null result)        "length result = 0" $
    cover 8 (length result == 1) "length result = 1" $
    cover 8 (length result >= 2) "length result ≥ 2" $
    case result of
        [ ] -> F.fold coins == C.zero
        [x] -> F.fold coins == x
        cxs -> all (> threshold') cxs
  where
    threshold' = unDustThreshold threshold

propCoalesceDustNeverLengthensList :: CoalesceDustData -> Property
propCoalesceDustNeverLengthensList (CoalesceDustData threshold coins) =
    property $ length coins >= length (coalesceDust threshold coins)

--------------------------------------------------------------------------------
-- reduceChangeOutputs - Properties
--------------------------------------------------------------------------------

data ReduceChangeOutputsData = ReduceChangeOutputsData
    { rcodFee :: Fee
    , rcodThreshold :: DustThreshold
    , rcodCoins :: [Coin]
    } deriving (Eq, Generic, Show)

instance Arbitrary ReduceChangeOutputsData where
    arbitrary = do
        coalesceDustData <- arbitrary
        let threshold = cddThreshold coalesceDustData
        let coins = F.toList $ cddCoins coalesceDustData
        let coinSum = sum $ coinToIntegral <$> coins
        fee <- unsafeFee <$> oneof
            [ pure 0
            , choose (1, safePred coinSum)
            , pure coinSum
            , choose (coinSum + 1, coinSum * 2)
            ]
        pure $ ReduceChangeOutputsData fee threshold coins
      where
        safePred :: Integer -> Integer
        safePred x
            | x > 0 = x - 1
            | otherwise = 0
    shrink = genericShrink

propReduceChangeOutputsDataCoverage :: ReduceChangeOutputsData -> Property
propReduceChangeOutputsDataCoverage
    (ReduceChangeOutputsData (Fee fee) _ coins) =
        let coinSum = F.fold coins in
        property
            -- Test coverage of fee amount, relative to sum of coins:
            $ cover 100 (fee >= C.zero)
                "fee >= 0"
            $ cover 8 (fee == C.zero)
                "fee = 0"
            $ cover 8 (length (filter (> C.zero) coins) == 1)
                "one non-empty coin"
            $ cover 8 (length (filter (> C.zero) coins) == 2)
                "two non-empty coins"
            $ cover 8 (length (filter (> C.zero) coins) >= 3)
                "several non-empty coins"
            $ cover 8 (any (> C.zero) coins && C.zero < fee && fee < coinSum)
                "0 < fee < sum coins"
            $ cover 8 (any (> C.zero) coins && fee == coinSum)
                "fee = sum coins"
            $ cover 8 (any (> C.zero) coins && fee > coinSum)
                "fee > sum coins"
            True

propReduceChangeOutputsPreservesSum :: ReduceChangeOutputsData -> Property
propReduceChangeOutputsPreservesSum
    (ReduceChangeOutputsData (Fee fee) threshold coins) = property check
  where
    coinsRemaining = reduceChangeOutputs threshold (Fee fee) coins
    -- We can only expect the total sum to be preserved if the supplied coins
    -- are enough to pay for the fee:
    check
        | fee < F.fold coins =
            F.fold coins == F.fold coinsRemaining `C.add` fee
        | otherwise =
            null coinsRemaining

--------------------------------------------------------------------------------
-- splitCoin - Properties
--------------------------------------------------------------------------------

data SplitCoinData = SplitCoinData
    { scdCoinToSplit :: Coin
    , scdCoinsToIncrease :: [Coin]
    } deriving (Eq, Generic, Show)

instance Arbitrary SplitCoinData where
    arbitrary = do
        coinToSplit <- genCoin
        n <- oneof
            [ pure 0
            , pure 1
            , choose (2, 10)
            ]
        coinsToIncrease <- replicateM n genCoin
        pure $ SplitCoinData coinToSplit coinsToIncrease
      where
        genCoin :: Gen Coin
        genCoin = oneof
            [ pure $ unsafeCoin @Integer 0
            , pure $ unsafeCoin @Integer 1
            , unsafeCoin @Int . (+ 1) . getPositive <$> arbitrary
            ]
    shrink = genericShrink

propSplitCoinDataCoverage :: SplitCoinData -> Property
propSplitCoinDataCoverage (SplitCoinData coinToSplit coinsToIncrease) =
    property
        $ cover 8 (null coinsToIncrease)
            "list of coins is empty"
        $ cover 8 (length coinsToIncrease == 1)
            "list of coins is singleton"
        $ cover 8 (length coinsToIncrease > 1)
            "list of coins has multiple entries"
        $ cover 8 (C.zero `elem` coinsToIncrease)
            "list of coins has at least one zero coin"
        $ cover 8 (any (> C.zero) coinsToIncrease)
            "list of coins has at least one non-zero coin"
        $ cover 8 (coinToSplit == C.zero)
            "coin to split is zero"
        $ cover 8 (coinToSplit > C.zero)
            "coin to split is non-zero"
        $ cover 8 (length coinsToIncrease > coinToIntegral coinToSplit)
            "coin to split is smaller than number of coins to increase"
        True

propSplitCoinPreservesSum :: SplitCoinData -> Property
propSplitCoinPreservesSum (SplitCoinData coinToSplit coinsToIncrease) =
    property $ totalBefore `shouldBe` totalAfter
  where
    totalAfter = F.fold (splitCoin coinToSplit coinsToIncrease)
    totalBefore = coinToSplit `C.add` F.fold coinsToIncrease

propSplitCoinFair :: (Coin, NonEmpty Coin) -> Property
propSplitCoinFair (coinToSplit, coinsToIncrease) = (.&&.)
    (F.all (uncurry (<=)) (NE.zip results upperBounds))
    (F.all (uncurry (>=)) (NE.zip results lowerBounds))
  where
    results = NE.fromList $ splitCoin coinToSplit $ NE.toList coinsToIncrease

    upperBounds = unsafeCoin @Integer . ceiling
        . computeIdealResult <$> coinsToIncrease
    lowerBounds = unsafeCoin @Integer . floor
        . computeIdealResult <$> coinsToIncrease

    computeIdealResult :: Coin -> Rational
    computeIdealResult c
        = fromIntegral (coinToIntegral @Integer c)
        + coinToIntegral @Integer coinToSplit
        % fromIntegral (length coinsToIncrease)

--------------------------------------------------------------------------------
-- Fee Adjustment - Unit Tests
--------------------------------------------------------------------------------

feeOptions
    :: Integer
    -> Integer
    -> FeeOptions i o
feeOptions fee dust = FeeOptions
    { feeEstimator = FeeEstimator $
        \_ -> unsafeFee fee
    , dustThreshold =
        unsafeDustThreshold dust
    }

feeUnitTest
    :: FeeFixture
    -> Either FeeAdjustmentError FeeOutput
    -> SpecWith ()
feeUnitTest (FeeFixture inpsF outsF chngsF utxoF feeF dustF) expected =
    it title $ do
        (utxo, sel) <- setup @TxIn @Address
        result <- runExceptT $ do
            (CoinSelection inps outs chngs) <-
                adjustForFee (feeOptions feeF dustF) utxo sel
            return $ FeeOutput
                { csInps = coinToIntegral . entryValue <$> coinMapToList inps
                , csOuts = coinToIntegral . entryValue <$> coinMapToList outs
                , csChngs = coinToIntegral <$> chngs
                }
        fmap sortFeeOutput result `shouldBe` fmap sortFeeOutput expected
  where
    setup
        :: forall i o . (Arbitrary i, Arbitrary o, Ord i, Ord o)
        => IO (CoinMap i, CoinSelection i o)
    setup = do
        utxo <- generate (genInputs $ unsafeCoin <$> utxoF)
        inps <- (fmap (uncurry CoinMapEntry) . Map.toList . unCoinMap) <$>
            generate (genInputs $ unsafeCoin <$> inpsF)
        outs <- generate (genOutputs $ unsafeCoin <$> outsF)
        let chngs = map unsafeCoin chngsF
        pure (utxo, CoinSelection (coinMapFromList inps) outs chngs)

    title :: String
    title = mempty
        <> "CoinSelection (inps=" <> show inpsF
        <> "outs=" <> show outsF
        <> "chngs=" <> show chngsF
        <> "), UTxO=" <> show utxoF
        <> "), fee=" <> show feeF
        <> " --> " <> show expected

-- | A fixture for testing the fee calculation
data FeeFixture = FeeFixture
    { fInps :: [Integer]
        -- ^ Value (in Lovelace) & number of coins in inputs
    , fOuts :: [Integer]
        -- ^ Value (in Lovelace) & number of requested outputs
    , fChngs :: [Integer]
        -- ^ Value (in Lovelace) & number of changes
    , fUtxo :: [Integer]
        -- ^ Value (in Lovelace) & number of available coins in the UTxO
    , fFee :: Integer
        -- ^ Value (in Lovelace) of rigid fee
    , fDust :: Integer
        -- ^ Value (in Lovelace) of dust
    } deriving Show

-- | A fee calculation output
data FeeOutput = FeeOutput
    { csInps :: [Integer]
        -- ^ Value (in Lovelace) & number of available coins in the UTxO
    , csOuts :: [Integer]
        -- ^ Value (in Lovelace) & number of requested outputs
    , csChngs :: [Integer]
        -- ^ Value (in Lovelace) & number of changes
    } deriving (Show, Eq)

sortFeeOutput :: FeeOutput -> FeeOutput
sortFeeOutput (FeeOutput is os cs) =
    FeeOutput (L.sort is) (L.sort os) (L.sort cs)

--------------------------------------------------------------------------------
-- Arbitrary Instances
--------------------------------------------------------------------------------

deriving newtype instance Arbitrary a => Arbitrary (ShowFmt a)

genInputs :: (Arbitrary i, Ord i) => [Coin] -> Gen (CoinMap i)
genInputs coins = do
    let n = length coins
    inps <- vectorOf n arbitrary
    return $ CoinMap $ Map.fromList $ zip inps coins

genOutputs :: (Arbitrary o, Ord o) => [Coin] -> Gen (CoinMap o)
genOutputs coins = do
    let n = length coins
    outs <- vectorOf n arbitrary
    return $ coinMapFromList $ zipWith CoinMapEntry outs coins

genSelection
    :: (Arbitrary i, Ord i, Ord o)
    => CoinMap o
    -> Gen (CoinSelection i o)
genSelection outs = do
    let opts = CS.CoinSelectionOptions (const 100)
    utxo <- vectorOf (length outs * 3) arbitrary >>= genInputs
    case runIdentity $ runExceptT $ selectCoins largestFirst opts utxo outs of
        Left _ -> genSelection outs
        Right (s,_) -> return s

instance Arbitrary TxIn where
    shrink _ = []
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary Coin where
    arbitrary = unsafeCoin @Int <$> choose (1, 100_000)
    shrink = fmap unsafeCoin . filter (> 0) . shrink . coinToIntegral @Int

instance Arbitrary DustThreshold where
    arbitrary = unsafeDustThreshold @Int <$> choose (0, 100)
    shrink = fmap DustThreshold . shrink . unDustThreshold

instance Arbitrary Fee where
    arbitrary = unsafeFee @Int <$> choose (1, 100_000)
    shrink = fmap Fee . shrink . unFee

instance (Arbitrary i, Arbitrary o, Ord i, Ord o) =>
    Arbitrary (FeeProp i o)
  where
    shrink (FeeProp cs utxo opts) =
        case Map.toList $ unCoinMap utxo of
            [] ->
                map (\cs' -> FeeProp cs' utxo opts) (shrink cs)
            us ->
                concatMap (\cs' ->
                    [ FeeProp cs' mempty opts
                    , FeeProp cs' (CoinMap $ Map.fromList (drop 1 us)) opts
                    ]
                ) (shrink cs)
    arbitrary = do
        cs <- arbitrary
        utxo <- choose (0, 50)
            >>= \n -> vectorOf n arbitrary
            >>= genInputs
        fee <- choose (100000, 500000)
        dust <- choose (0, 10000)
        return $ FeeProp cs utxo (fee, dust)

instance Arbitrary (Hash "Tx") where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> vectorOf 8 arbitraryBoundedIntegral
        pure $ Hash bytes

instance Arbitrary Address where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> vectorOf 8 arbitraryBoundedIntegral
        pure $ Address bytes

instance (Arbitrary i, Arbitrary o, Ord i, Ord o) =>
    Arbitrary (CoinSelection i o)
  where
    shrink sel = case unCoinSelection sel of
        ([_], [_], []) ->
            []
        (inps, outs, chgs) ->
            let
                inps' = if length inps > 1 then drop 1 inps else inps
                outs' = if length outs > 1 then drop 1 outs else outs
                chgs' = if not (null chgs) then drop 1 chgs else chgs
            in
            filter (\s -> s /= sel && isValidSelection s) $
                mkCoinSelection <$>
                    [ (inps', outs', chgs')
                    , (inps', outs , chgs )
                    , (inps , outs', chgs )
                    , (inps , outs , chgs')
                    ]
      where
        unCoinSelection s =
            (coinMapToList $ inputs s, coinMapToList $ outputs s, change s)
        mkCoinSelection (is, os, cs) =
            CoinSelection (coinMapFromList is) (coinMapFromList os) cs
    arbitrary = do
        outs <- choose (1, 10)
            >>= \n -> vectorOf n arbitrary
            >>= genOutputs
        genSelection outs

data FeeParameters i o = FeeParameters
    { feePerTransaction
        :: Fee
        -- ^ Base fee for a transaction.
    , feePerTransactionEntry
        :: Fee
        -- ^ Incremental fee for each input, output, and change output.
    } deriving (Eq, Generic, Show)

instance Arbitrary (FeeParameters i o) where
    arbitrary = do
        feePerTransaction <- unsafeFee @Int <$> choose (0, 10)
        feePerTransactionEntry <- unsafeFee @Int <$> choose (0, 10)
        pure $ FeeParameters {feePerTransaction, feePerTransactionEntry}
    shrink = genericShrink

feeEstimatorFromParameters :: FeeParameters i o -> FeeEstimator i o
feeEstimatorFromParameters
    FeeParameters {feePerTransaction, feePerTransactionEntry} =
        FeeEstimator $ \s -> Fee $ maybe C.zero
            (C.add (unFee feePerTransaction))
            (C.mul
                (unFee feePerTransactionEntry)
                (length (inputs s) + length (outputs s)))

instance Arbitrary (FeeOptions i o) where
    arbitrary = do
        dustThreshold <- unsafeDustThreshold @Int <$> choose (0, 10)
        feeEstimator <- feeEstimatorFromParameters <$> arbitrary
        return $ FeeOptions {dustThreshold, feeEstimator}

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = do
        tailLength <- choose (0, 10)
        (:|) <$> arbitrary <*> replicateM tailLength arbitrary
    shrink = genericShrink

instance Show (FeeOptions i o) where
    show (FeeOptions _ dust) = show dust
