{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.TypesSpec
    ( spec
    ) where

import Prelude

import Cardano.Types
    ( Address (..)
    , Coin (..)
    , Dom (..)
    , Hash (..)
    , ShowFmt (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    , balance
    , excluding
    , isSubsetOf
    , isValidCoin
    , restrictedBy
    , restrictedTo
    )
import Data.Set
    ( Set, (\\) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , checkCoverage
    , choose
    , cover
    , oneof
    , property
    , scale
    , vectorOf
    , (===)
    )
import Test.Unsafe
    ( unsafeFromHex )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "Generators are valid" $ do
        it "Arbitrary Coin" $ property isValidCoin

    describe "Lemma 2.1 - Properties of UTxO operations" $ do
        it "2.1.1) ins⊲ u ⊆ u"
            (checkCoverage $ prop_2_1_1 @TxIn)
        it "2.1.2) ins⋪ u ⊆ u"
            (checkCoverage $ prop_2_1_2 @TxIn)
        it "2.1.3) u ⊳ outs ⊆ u"
            (checkCoverage $ prop_2_1_3 @TxIn)
        it "2.1.4) ins⊲ (u ⋃ v) = (ins⊲ u) ⋃ (ins⊲ v)"
            (checkCoverage $ prop_2_1_4 @TxIn)
        it "2.1.5) ins⋪ (u ⋃ v) = (ins⋪ u) ⋃ (ins⋪ v)"
            (checkCoverage $ prop_2_1_5 @TxIn)
        it "2.1.6) (dom u ⋂ ins) ⊲ u = ins⊲ u"
            (checkCoverage $ prop_2_1_6 @TxIn)
        it "2.1.7) (dom u ⋂ ins) ⋪ u = ins⋪ u"
            (checkCoverage $ prop_2_1_7 @TxIn)
        it "2.1.8) (dom u ⋃ ins) ⋪ (u ⋃ v) = (ins ⋃ dom u) ⋪ v"
            (checkCoverage $ prop_2_1_8 @TxIn)
        it "2.1.9) ins⋪ u = (dom u \\ ins)⊲ u"
            (checkCoverage $ prop_2_1_9 @TxIn)

    describe "Lemma 2.6 - Properties of balance" $ do
        it "2.6.1) dom u ⋂ dom v ==> balance (u ⋃ v) = balance u + balance v"
            (checkCoverage $ prop_2_6_1 @TxIn)
        it "2.6.2) balance (ins⋪ u) = balance u - balance (ins⊲ u)"
            (checkCoverage $ prop_2_6_2 @TxIn)

{-------------------------------------------------------------------------------
       Wallet Specification - Lemma 2.1 - Properties of UTxO operations
-------------------------------------------------------------------------------}

prop_2_1_1 :: Ord u => (Set u, UTxO u) -> Property
prop_2_1_1 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `restrictedBy` ins) `isSubsetOf` u

prop_2_1_2 :: Ord u => (Set u, UTxO u) -> Property
prop_2_1_2 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `excluding` ins) `isSubsetOf` u

prop_2_1_3 :: Ord u => (Set TxOut, UTxO u) -> Property
prop_2_1_3 (outs, u) =
    cover 50 cond "u ⋂ outs ≠ ∅" (property prop)
  where
    cond = not $ Set.null $
        Set.fromList (Map.elems (getUTxO u)) `Set.intersection` outs
    prop = (u `restrictedTo` outs) `isSubsetOf` u

prop_2_1_4 :: (Ord u, Show u) => (Set u, UTxO u, UTxO u) -> Property
prop_2_1_4 (ins, u, v) =
    cover 50 cond "(dom u ⋃ dom v) ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ Set.union (dom u) (dom v) `Set.intersection` ins
    prop =
        ((u <> v) `restrictedBy` ins)
            ===
        (u `restrictedBy` ins) <> (v `restrictedBy` ins)

prop_2_1_5 :: (Ord u, Show u) => (Set u, UTxO u, UTxO u) -> Property
prop_2_1_5 (ins, u, v) =
    cover 50 cond "(dom u ⋃ dom v) ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ Set.union (dom u) (dom v) `Set.intersection` ins
    prop =
        ((u <> v) `excluding` ins)
            ===
        (u `excluding` ins) <> (v `excluding` ins)

prop_2_1_6 :: (Ord u, Show u) => (Set u, UTxO u) -> Property
prop_2_1_6 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        (u `restrictedBy` (dom u `Set.intersection` ins))
            ===
        (u `restrictedBy` ins)

prop_2_1_7 :: (Ord u, Show u) => (Set u, UTxO u) -> Property
prop_2_1_7 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        (u `excluding` (dom u `Set.intersection` ins))
            ===
        (u `excluding` ins)

prop_2_1_8 :: (Ord u, Show u) => (Set u, UTxO u, UTxO u) -> Property
prop_2_1_8 (ins, u, v) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        ((u <> v) `excluding` (dom u <> ins))
            ===
        v `excluding` (ins <> dom u)

prop_2_1_9 :: (Ord u, Show u) => (Set u, UTxO u) -> Property
prop_2_1_9 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `excluding` ins) === u `restrictedBy` (dom u \\ ins)

{-------------------------------------------------------------------------------
       Wallet Specification - Lemma 2.6 - Properties of Balance
-------------------------------------------------------------------------------}

prop_2_6_1 :: Ord u => (UTxO u, UTxO u) -> Property
prop_2_6_1 (u, v) =
    cover 50 cond "u ≠ ∅ , v ≠ ∅" (property prop)
  where
    -- NOTE:
    -- A precondition (u ⋂ v = ∅ ) is hard to satisfy because our generators
    -- are built in order to not be 'too entropic'. So, we better just create
    -- a v' that has no overlap with u.
    v' = v `excluding` dom u
    cond = not (u `isSubsetOf` mempty || v' `isSubsetOf` mempty)
    prop = balance (u <> v') === balance u + balance v'

prop_2_6_2 :: Ord u => (Set u, UTxO u) -> Property
prop_2_6_2 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        balance (u `excluding` ins)
            ===
        balance u - balance (u `restrictedBy` ins)

{-------------------------------------------------------------------------------
                            Arbitrary Instances

    Arbitrary instances define here aren't necessarily reflecting on real-life
    scenario, but they help test the property above by constructing data
    structures that don't have much entropy and therefore, allow us to even test
    something when checking for intersections and set restrictions!
-------------------------------------------------------------------------------}

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

instance Arbitrary (Hash "Tx") where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Hash $ unsafeFromHex
            "0000000000000000000000000000000000000000000000000000000000000001"
        , pure $ Hash $ unsafeFromHex
            "0000000000000000000000000000000000000000000000000000000000000002"
        , pure $ Hash $ unsafeFromHex
            "0000000000000000000000000000000000000000000000000000000000000003"
        ]

instance Arbitrary Address where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Address "ADDR01"
        , pure $ Address "ADDR02"
        , pure $ Address "ADDR03"
        ]

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = Coin <$> choose (0, 3)

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a crazy high indexes

instance (Arbitrary u, Ord u) => Arbitrary (UTxO u) where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (0, 10)
        utxo <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ UTxO $ Map.fromList utxo
