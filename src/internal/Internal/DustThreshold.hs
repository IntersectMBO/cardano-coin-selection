{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

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

newtype DustThreshold = DustThreshold
    { unDustThreshold :: SafeNatural }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet DustThreshold)

dustThreshold :: Integral i => i -> Maybe DustThreshold
dustThreshold = fmap DustThreshold . SN.fromIntegral

dustThresholdToIntegral :: Integral i => DustThreshold -> i
dustThresholdToIntegral = SN.toIntegral . unDustThreshold
