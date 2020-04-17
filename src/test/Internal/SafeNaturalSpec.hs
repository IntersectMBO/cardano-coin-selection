module Internal.SafeNaturalSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Property, checkCoverage, cover, property )

import qualified Internal.SafeNatural as SN

spec :: Spec
spec = do

    describe "SafeNatural properties" $ do
        it "Only construction of non-negative values is possible." $
            checkCoverage prop_construction

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
