{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Vector.ShuffleSpec
    ( spec
    ) where

import Prelude

import Data.List.NonEmpty
    ( NonEmpty (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Confidence (..)
    , NonEmptyList (..)
    , Positive (..)
    , PrintableString (..)
    , Property
    , Testable
    , arbitrary
    , checkCoverageWith
    , cover
    , genericShrink
    , label
    , vectorOf
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, pick, run )
import Test.Vector.Shuffle
    ( mkSeed, shuffle, shuffleNonEmpty, shuffleWith )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "shuffle" $ do
        it "every list can be shuffled, ultimately" $
            check prop_shuffleCanShuffle
        it "shuffle is non-deterministic" $
            check prop_shuffleNotDeterministic
        it "sort (shuffle xs) == sort xs" $
            check prop_shufflePreserveElements

    describe "shuffleNonEmpty" $ do
        it "every non-empty list can be shuffled, ultimately" $
            check prop_shuffleNonEmptyCanShuffle
        it "shuffleNonEmpty is non-deterministic" $
            check prop_shuffleNonEmptyNotDeterministic
        it "sort (shuffleNonEmpty xs) == sort xs" $
            check prop_shuffleNonEmptyPreserveElements

    describe "shuffleWith / mkSeed" $ do
        it "shuffling with the same seed is deterministic" $
            check prop_shuffleWithDeterministic
        it "different seed means different shuffles" $
            check prop_shuffleDifferentSeed

  where
    check :: forall p. Testable p => p -> Property
    check = checkCoverageWith lowerConfidence

    lowerConfidence :: Confidence
    lowerConfidence = Confidence (10^(6 :: Integer)) 0.75

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_shuffleCanShuffle
    :: NonEmptyList Int
    -> Property
prop_shuffleCanShuffle (NonEmpty xs) = monadicIO $ run $ do
    xs' <- shuffle xs
    return $ cover 90 (xs /= xs') "shuffled" ()

prop_shuffleNonEmptyCanShuffle
    :: NonEmpty Int
    -> Property
prop_shuffleNonEmptyCanShuffle xs = monadicIO $ run $ do
    xs' <- shuffleNonEmpty xs
    return $ cover 90 (xs /= xs') "shuffled" ()

prop_shuffleNotDeterministic
    :: NonEmptyList Int
    -> Property
prop_shuffleNotDeterministic (NonEmpty xs) = monadicIO $ run $ do
    xs1 <- shuffle xs
    xs2 <- shuffle xs
    return $ cover 90 (xs1 /= xs2) "not deterministic" ()

prop_shuffleNonEmptyNotDeterministic
    :: NonEmpty Int
    -> Property
prop_shuffleNonEmptyNotDeterministic xs = monadicIO $ run $ do
    xs1 <- shuffleNonEmpty xs
    xs2 <- shuffleNonEmpty xs
    return $ cover 90 (xs1 /= xs2) "not deterministic" ()

prop_shufflePreserveElements
    :: [Int]
    -> Property
prop_shufflePreserveElements xs = monadicIO $ run $ do
    xs' <- shuffle xs
    return $ cover 90 (not $ null xs) "non-empty" (L.sort xs == L.sort xs')

prop_shuffleNonEmptyPreserveElements
    :: NonEmpty Int
    -> Property
prop_shuffleNonEmptyPreserveElements xs = monadicIO $ run $ do
    xs' <- shuffleNonEmpty xs
    return $ cover 90 (not $ null xs) "non-empty" (NE.sort xs == NE.sort xs')

-- ∀(g :: RandomGen).
-- ∀(es :: [a]).
--
-- shuffleWith g es == shuffleWith g es
prop_shuffleWithDeterministic
    :: PrintableString
    -> NonEmptyList Int
    -> Property
prop_shuffleWithDeterministic (PrintableString seed) (NonEmpty xs) =
    monadicIO $ do
        ys0 <- run $ shuffleWith (mkSeed $ T.pack seed) xs
        ys1 <- run $ shuffleWith (mkSeed $ T.pack seed) xs
        monitor $ cover 90 (length xs > 1) "non singleton"
        assert (ys0 == ys1)

-- ∀(x0 : Text, x1 : Text). g0 = mkSeed x0, g1 = mkSeed x1
-- ∃(Δ: Int).
-- ∀(es :: [a]).
--
-- g0 ≠g1, length es > Δ⇒ shuffleWith g0 es ≠shuffleWith g1 es
prop_shuffleDifferentSeed
    :: (PrintableString, PrintableString)
    -> Positive Int
    -> Property
prop_shuffleDifferentSeed (x0, x1) (Positive len) = do
    x0 /= x1 ==> monadicIO $ do
        let g0 = mkSeed $ T.pack $ getPrintableString x0
        let g1 = mkSeed $ T.pack $ getPrintableString x1
        es <- pick $ vectorOf len (arbitrary @Int)
        ys0 <- run $ shuffleWith g0 es
        ys1 <- run $ shuffleWith g1 es
        monitor $ label (prettyLen es)
        monitor $ cover 90 (ys0 /= ys1) "different"
  where
    prettyLen :: [a] -> String
    prettyLen xs = case length xs of
        n | n <= 1 -> "singleton"
        n | n <= 10 -> "small list"
        _ -> "big list"

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink
