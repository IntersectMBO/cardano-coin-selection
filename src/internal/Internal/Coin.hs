{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Internal.Coin
    ( Coin (..)
    , coin
    , coinToIntegral
    ) where

import Prelude

import GHC.Generics
    ( Generic )
import Internal.SafeNatural
    ( SafeNatural )
import Quiet
    ( Quiet (Quiet) )

import qualified Internal.SafeNatural as SN

-- | Represents a non-negative integer amount of currency.
--
newtype Coin = Coin { unCoin :: SafeNatural }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet Coin)

instance Monoid Coin where
    mempty = Coin (SN.getSum mempty)

instance Semigroup Coin where
    Coin a <> Coin b = Coin $ a `SN.add` b

-- | A smart constructor for the 'Coin' type.
--
-- Returns a coin if (and only if) the given input is not negative.
--
coin :: Integral i => i -> Maybe Coin
coin = fmap Coin . SN.fromIntegral

-- | Converts the given coin value to an integral value.
--
coinToIntegral :: Integral i => Coin -> i
coinToIntegral = SN.toIntegral . unCoin
