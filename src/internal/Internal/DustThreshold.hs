{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Internal.DustThreshold
    ( DustThreshold (..)
    , dustThresholdFromIntegral
    , dustThresholdFromNatural
    , dustThresholdToIntegral
    , dustThresholdToNatural
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

-- | Defines the maximum size of a dust coin.
--
-- Change values that are less than or equal to this threshold will not be
-- included in coin selections.
--
-- Use 'dustThresholdFromNatural' to create a dust threshold from a natural
-- number.
--
newtype DustThreshold = DustThreshold
    { unDustThreshold :: SafeNatural }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet DustThreshold)

-- | Creates a dust threshold from an integral number.
--
-- Returns a dust threshold if (and only if) the given input is not negative.
--
dustThresholdFromIntegral :: Integral i => i -> Maybe DustThreshold
dustThresholdFromIntegral = fmap DustThreshold . SN.fromIntegral

-- | Creates a dust threshold from a natural number.
--
dustThresholdFromNatural :: Natural -> DustThreshold
dustThresholdFromNatural = DustThreshold . SN.fromNatural

-- | Converts the given dust threshold into an integral number.
--
dustThresholdToIntegral :: Integral i => DustThreshold -> i
dustThresholdToIntegral = SN.toIntegral . unDustThreshold

-- | Converts the given dust threshold into a natural number.
--
dustThresholdToNatural :: DustThreshold -> Natural
dustThresholdToNatural = SN.toNatural . unDustThreshold
