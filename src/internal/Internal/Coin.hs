{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

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

newtype Coin = Coin { unCoin :: SafeNatural }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet Coin)

instance Monoid Coin where
    mempty = Coin (SN.getSum mempty)

instance Semigroup Coin where
    Coin a <> Coin b = Coin $ a `SN.add` b

coin :: Integral i => i -> Maybe Coin
coin = fmap Coin . SN.fromIntegral

coinToIntegral :: Integral i => Coin -> i
coinToIntegral = SN.toIntegral . unCoin
