{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.CoinSpec
    ( spec
    ) where

import Prelude

import Data.Maybe
    ( catMaybes, fromMaybe )
import Internal.Coin
    ( Coin )
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

import qualified Internal.Coin as C

spec :: Spec
spec = do

    describe "Coin properties" $ do
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
        then C.coinFromIntegral i `shouldBe` Nothing
        else (C.coinToIntegral <$> C.coinFromIntegral i) `shouldBe` Just i

prop_generation
    :: Coin
    -> Property
prop_generation n = property
    $ cover 2 (n == C.zero)
        "value is zero"
    $ cover 2 (n == C.one)
        "value is one"
    $ cover 10 (n > C.one)
        "value is more than one"
    True

prop_add :: Coin -> Coin -> Property
prop_add x y = property $
    C.coinToIntegral @Integer (x `C.add` y)
        `shouldBe`
        (C.coinToIntegral x + C.coinToIntegral y)

prop_mul :: Coin -> Integer -> Property
prop_mul x y = property
    $ cover 2 (y == 0)
        "scaling factor is zero"
    $ cover 8 (y < 0)
        "scaling factor is negative"
    $ cover 8 (y > 0)
        "scaling factor is positive"
    $ case (x `C.mul` y) of
        Nothing ->
            y `shouldSatisfy` (< 0)
        Just r ->
            (C.coinToIntegral x * y) `shouldBe` C.coinToIntegral @Integer r

prop_sub :: Coin -> Coin -> Property
prop_sub x y = property
    $ cover 4 (x == y)
        "values are equal"
    $ cover 8 (x < y)
        "x < y"
    $ cover 8 (x > y)
        "x > y"
    $ case x `C.sub` y of
        Nothing ->
            x `shouldSatisfy` (< y)
        Just r ->
            C.coinToIntegral x - C.coinToIntegral y
                `shouldBe` C.coinToIntegral @Integer r

prop_div :: Coin -> Integer -> Property
prop_div x y = property
    $ cover 2 (y == 0)
        "denominator is zero"
    $ cover 8 (y < 0)
        "denominator is negative"
    $ cover 8 (y > 0)
        "denominator is positive"
    $ case (x `C.div` y) of
        Nothing ->
            y `shouldSatisfy` (<= 0)
        Just r ->
            (C.coinToIntegral x `div` y)
                `shouldBe` C.coinToIntegral @Integer r

prop_mod :: Coin -> Integer -> Property
prop_mod x y = property
    $ cover 2 (y == 0)
        "denominator is zero"
    $ cover 8 (y < 0)
        "denominator is negative"
    $ cover 8 (y > 0)
        "denominator is positive"
    $ case (x `C.mod` y) of
        Nothing ->
            y `shouldSatisfy` (<= 0)
        Just r ->
            (C.coinToIntegral x `mod` y)
                `shouldBe` C.coinToIntegral @Integer r

prop_distance :: Coin -> Coin -> Property
prop_distance x y = property
    $ cover 4 (x == y)
        "values are equal"
    $ cover 8 (x < y)
        "x < y"
    $ cover 8 (x > y)
        "x > y"
    $ C.coinToIntegral @Integer (x `C.distance` y)
        `shouldBe`
        abs (C.coinToIntegral x - C.coinToIntegral y)

instance Arbitrary Coin where
    arbitrary = oneof
        [ pure C.zero
        , pure C.one
        , somethingElse
        ]
      where
        somethingElse =
            fromMaybe C.zero
            . C.coinFromIntegral @Integer
            . getNonNegative <$> arbitrary
    shrink n = catMaybes
        $ C.coinFromIntegral @Integer <$> shrink (C.coinToIntegral @Integer n)
