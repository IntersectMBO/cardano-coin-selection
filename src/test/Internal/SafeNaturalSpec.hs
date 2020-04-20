{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.SafeNaturalSpec
    ( spec
    ) where

import Prelude

import Data.Maybe
    ( catMaybes, fromMaybe )
import Internal.SafeNatural
    ( SafeNatural )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    , Property
    , checkCoverage
    , cover
    , oneof
    , property
    )

import qualified Internal.SafeNatural as SN

spec :: Spec
spec = do

    describe "SafeNatural properties" $ do
        it "Only construction of non-negative values is possible." $
            checkCoverage prop_construction
        it "Coverage of generated values is acceptable." $
            checkCoverage prop_generation
        it "Addition" $
            checkCoverage prop_add
        it "Multiplication" $
            checkCoverage prop_mul
        it "Subtraction" $
            checkCoverage prop_sub
        it "Division" $
            checkCoverage prop_div
        it "Modulus" $
            checkCoverage prop_mod
        it "Distance" $
            checkCoverage prop_distance

prop_construction
    :: Integer
    -> Property
prop_construction i = property
    $ cover 10 (i < 0)
        "input is negative"
    $ cover 10 (i > 0)
        "input is positive"
    $ cover 2 (i == 0)
        "input is zero"
    $ if i < 0
        then SN.fromIntegral i `shouldBe` Nothing
        else (SN.toIntegral <$> SN.fromIntegral i) `shouldBe` Just i

prop_generation
    :: SafeNatural
    -> Property
prop_generation n = property
    $ cover 2 (n == SN.zero)
        "value is zero"
    $ cover 2 (n == SN.one)
        "value is one"
    $ cover 10 (n > SN.one)
        "value is more than one"
    True

prop_add :: SafeNatural -> SafeNatural -> Property
prop_add x y = property $
    SN.toIntegral @Integer (x `SN.add` y)
        `shouldBe`
        (SN.toIntegral x + SN.toIntegral y)

prop_mul :: SafeNatural -> SafeNatural -> Property
prop_mul x y = property $
    SN.toIntegral @Integer (x `SN.mul` y)
        `shouldBe`
        (SN.toIntegral x * SN.toIntegral y)

prop_sub :: SafeNatural -> SafeNatural -> Property
prop_sub x y = property
    $ cover 4 (x == y)
        "values are equal"
    $ cover 8 (x < y)
        "x < y"
    $ cover 8 (x > y)
        "x > y"
    $ case x `SN.sub` y of
        Nothing ->
            x `shouldSatisfy` (< y)
        Just r ->
            SN.toIntegral x - SN.toIntegral y
                `shouldBe` SN.toIntegral @Integer r

prop_div :: SafeNatural -> SafeNatural -> Property
prop_div x y = property
    $ cover 2 (SN.isZero y)
        "denominator is zero"
    $ cover 8 (y > SN.zero)
        "denominator is positive"
    $ case (x `SN.div` y) of
        Nothing ->
            y `shouldBe` SN.zero
        Just r ->
            SN.toIntegral x `div` SN.toIntegral y
                `shouldBe` SN.toIntegral @Integer r

prop_mod :: SafeNatural -> SafeNatural -> Property
prop_mod x y = property
    $ cover 2 (SN.isZero y)
        "denominator is zero"
    $ cover 8 (y > SN.zero)
        "denominator is positive"
    $ case (x `SN.mod` y) of
        Nothing ->
            y `shouldBe` SN.zero
        Just r ->
            SN.toIntegral x `mod` SN.toIntegral y
                `shouldBe` SN.toIntegral @Integer r

prop_distance :: SafeNatural -> SafeNatural -> Property
prop_distance x y = property
    $ cover 4 (x == y)
        "values are equal"
    $ cover 8 (x < y)
        "x < y"
    $ cover 8 (x > y)
        "x > y"
    $ SN.toIntegral @Integer (x `SN.distance` y)
        `shouldBe`
        abs (SN.toIntegral x - SN.toIntegral y)

instance Arbitrary SafeNatural where
    arbitrary = oneof
        [ pure SN.zero
        , pure SN.one
        , somethingElse
        ]
      where
        somethingElse =
            fromMaybe SN.zero
            . SN.fromIntegral @Integer
            . getNonNegative <$> arbitrary
    shrink n = catMaybes
        $ SN.fromIntegral @Integer <$> shrink (SN.toIntegral @Integer n)
