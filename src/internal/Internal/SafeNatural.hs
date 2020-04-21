{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Internal.SafeNatural
    (
      -- * Types
      SafeNatural

      -- Construction and Deconstruction
    , fromIntegral
    , fromNatural
    , toIntegral
    , toNatural

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

      -- * Monodial Operations
    , Sum (..)
    , Product (..)

    ) where

import Prelude hiding
    ( div, fromIntegral, mod, pred, succ )

import Data.Coerce
    ( coerce )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (Quiet) )

import qualified Prelude

newtype SafeNatural = SafeNatural { unSafeNatural :: Natural }
    deriving stock (Eq, Generic, Ord)
    deriving newtype Show

toIntegral :: Integral i => SafeNatural -> i
toIntegral (SafeNatural i) = Prelude.fromIntegral i

fromIntegral :: Integral i => i -> Maybe SafeNatural
fromIntegral i
    | i >= 0    = Just $ SafeNatural $ Prelude.fromIntegral i
    | otherwise = Nothing

fromNatural :: Natural -> SafeNatural
fromNatural = SafeNatural

toNatural :: SafeNatural -> Natural
toNatural = unSafeNatural

add :: SafeNatural -> SafeNatural -> SafeNatural
add (SafeNatural x) (SafeNatural y) = SafeNatural $ x + y

sub :: SafeNatural -> SafeNatural -> Maybe SafeNatural
sub (SafeNatural x) (SafeNatural y)
    | x >= y    = Just $ SafeNatural $ x - y
    | otherwise = Nothing

mul :: SafeNatural -> SafeNatural -> SafeNatural
mul (SafeNatural x) (SafeNatural y) = SafeNatural $ x * y

div :: SafeNatural -> SafeNatural -> Maybe SafeNatural
div (SafeNatural n) (SafeNatural d)
    | d == 0 = Nothing
    | otherwise = Just $ SafeNatural $ n `Prelude.div` d

mod :: SafeNatural -> SafeNatural -> Maybe SafeNatural
mod (SafeNatural n) (SafeNatural d)
    | d == 0 = Nothing
    | otherwise = Just $ SafeNatural $ n `Prelude.mod` d

distance :: SafeNatural -> SafeNatural -> SafeNatural
distance (SafeNatural x) (SafeNatural y)
    | x >= y    = SafeNatural $ x - y
    | otherwise = SafeNatural $ y - x

pred :: SafeNatural -> Maybe SafeNatural
pred x = x `sub` one

succ :: SafeNatural -> SafeNatural
succ x = x `add` one

isZero :: SafeNatural -> Bool
isZero = (== zero)

zero :: SafeNatural
zero = SafeNatural 0

one :: SafeNatural
one = SafeNatural 1

newtype Sum a = Sum { getSum :: a }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet (Sum a))

instance Monoid (Sum SafeNatural) where
    mempty = Sum zero

instance Semigroup (Sum SafeNatural) where
    (<>) = coerce ((+) :: Natural -> Natural -> Natural)

newtype Product a = Product { getProduct :: a }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet (Product a))

instance Monoid (Product SafeNatural) where
    mempty = Product one

instance Semigroup (Product SafeNatural) where
    (<>) = coerce ((*) :: Natural -> Natural -> Natural)
