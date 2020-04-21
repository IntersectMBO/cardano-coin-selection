{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Internal.Coin
    ( Coin (..)
    , coinFromIntegral
    , coinFromNatural
    , coinToIntegral
    , coinToNatural
    ) where

import Prelude

import GHC.Generics
    ( Generic )
import Internal.SafeNatural
    ( SafeNatural )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (Quiet) )

import qualified Internal.SafeNatural as SN

-- | Represents a non-negative integral amount of currency.
--
-- Use 'coinFromNatural' to create a coin from a natural number.
--
-- Use 'coinToNatural' to convert a coin into a natural number.
--
newtype Coin = Coin { unCoin :: SafeNatural }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet Coin)

instance Monoid Coin where
    mempty = Coin (SN.getSum mempty)

instance Semigroup Coin where
    Coin a <> Coin b = Coin $ a `SN.add` b

-- | Creates a coin from an integral number.
--
-- Returns a coin if (and only if) the given input is not negative.
--
coinFromIntegral :: Integral i => i -> Maybe Coin
coinFromIntegral = fmap Coin . SN.fromIntegral

-- | Creates a coin from a natural number.
--
coinFromNatural :: Natural -> Coin
coinFromNatural = Coin . SN.fromNatural

-- | Converts the given coin into an integral number.
--
coinToIntegral :: Integral i => Coin -> i
coinToIntegral = SN.toIntegral . unCoin

-- | Converts the given coin into a natural number.
--
coinToNatural :: Coin -> Natural
coinToNatural = SN.toNatural . unCoin
