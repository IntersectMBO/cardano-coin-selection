{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Internal.Coin
    (
      -- * Types
      Coin

      -- Construction and Deconstruction
    , coinFromIntegral
    , coinFromNatural
    , coinToIntegral
    , coinToNatural

      -- * Unary Operations
    , pred
    , succ

      -- * Binary Operations
    , add
    , sub
    , mul
    , div
    , mod

      -- * Calculating Distances
    , distance

      -- * Value Tests
    , isZero

      -- * Special Values
    , zero
    , one

    ) where

import Prelude hiding
    ( div, fromIntegral, mod, pred, succ )

import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (Quiet) )

import qualified Prelude

-- | Represents a non-negative integral amount of currency.
--
-- Use 'coinFromNatural' to create a coin from a natural number.
--
-- Use 'coinToNatural' to convert a coin into a natural number.
--
newtype Coin = Coin { unCoin :: Natural }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet Coin)

-- | Creates a coin from an integral number.
--
-- Returns a coin if (and only if) the given input is not negative.
--
coinFromIntegral :: Integral i => i -> Maybe Coin
coinFromIntegral i
    | i >= 0    = Just $ Coin $ Prelude.fromIntegral i
    | otherwise = Nothing

-- | Creates a coin from a natural number.
--
coinFromNatural :: Natural -> Coin
coinFromNatural = Coin

-- | Converts the given coin into an integral number.
--
coinToIntegral :: Integral i => Coin -> i
coinToIntegral (Coin i) = Prelude.fromIntegral i

-- | Converts the given coin into a natural number.
--
coinToNatural :: Coin -> Natural
coinToNatural = unCoin

add :: Coin -> Coin -> Coin
add (Coin x) (Coin y) = Coin $ x + y

sub :: Coin -> Coin -> Maybe Coin
sub (Coin x) (Coin y) = coinFromIntegral $ toInteger x - toInteger y

mul :: Integral i => Coin -> i -> Maybe Coin
mul (Coin x) y = coinFromIntegral $ toInteger x * toInteger y

div :: Integral i => Coin -> i -> Maybe Coin
div (Coin x) y
    | y <= 0    = Nothing
    | otherwise = coinFromIntegral $ toInteger x `Prelude.div` toInteger y

mod :: Integral i => Coin -> i -> Maybe Coin
mod (Coin x) y
    | y <= 0    = Nothing
    | otherwise = coinFromIntegral $ toInteger x `Prelude.mod` toInteger y

distance :: Coin -> Coin -> Coin
distance (Coin x) (Coin y)
    | x >= y    = Coin $ x - y
    | otherwise = Coin $ y - x

pred :: Coin -> Maybe Coin
pred x = x `sub` one

succ :: Coin -> Coin
succ x = x `add` one

isZero :: Coin -> Bool
isZero = (== zero)

zero :: Coin
zero = Coin 0

one :: Coin
one = Coin 1

newtype Sum a = Sum { getSum :: a }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet (Sum a))

instance Monoid Coin where
    mempty = zero

instance Semigroup Coin where
    (<>) = add
