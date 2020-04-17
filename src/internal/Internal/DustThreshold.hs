{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Internal.DustThreshold
    ( DustThreshold (..)
    , dustThreshold
    , dustThresholdToIntegral
    ) where

import Prelude

import GHC.Generics
    ( Generic )
import Internal.SafeNatural
    ( SafeNatural )
import Quiet
    ( Quiet (Quiet) )

import qualified Internal.SafeNatural as SN

-- | Defines the maximum size of a dust coin.
--
-- Change values that are less than or equal to this threshold will not be
-- included in transactions.
--
-- This type is isomorphic to 'Coin'.
--
newtype DustThreshold = DustThreshold
    { unDustThreshold :: SafeNatural }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet DustThreshold)

-- | A smart constructor for the 'DustThreshold' type.
--
-- Returns a dust threshold if (and only if) the given input is not negative.
--
dustThreshold :: Integral i => i -> Maybe DustThreshold
dustThreshold = fmap DustThreshold . SN.fromIntegral

-- | Converts the given dust threshold to an integral value.
--
dustThresholdToIntegral :: Integral i => DustThreshold -> i
dustThresholdToIntegral = SN.toIntegral . unDustThreshold
