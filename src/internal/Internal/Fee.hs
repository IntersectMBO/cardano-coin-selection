{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Internal.Fee
    ( Fee (..)
    , feeFromIntegral
    , feeFromNatural
    , feeToIntegral
    , feeToNatural
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

-- | Represents a non-negative fee to be paid on a transaction.
--
-- Use 'feeToNatural' to convert a fee into a natural number.
--
newtype Fee = Fee { unFee :: SafeNatural }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet Fee)

instance Monoid Fee where
    mempty = Fee (SN.getSum mempty)

instance Semigroup Fee where
    Fee a <> Fee b = Fee $ a `SN.add` b

-- | Creates a fee from an integral number.
--
-- Returns a fee if (and only if) the given input is not negative.
--
feeFromIntegral :: Integral i => i -> Maybe Fee
feeFromIntegral = fmap Fee . SN.fromIntegral

-- | Creates a fee from a natural number.
--
feeFromNatural :: Natural -> Fee
feeFromNatural = Fee . SN.fromNatural

-- | Converts the given fee into an integral number.
--
feeToIntegral :: Integral i => Fee -> i
feeToIntegral = SN.toIntegral . unFee

-- | Converts the given fee into a natural number.
--
feeToNatural :: Fee -> Natural
feeToNatural = SN.toNatural . unFee
