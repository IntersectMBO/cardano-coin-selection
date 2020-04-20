{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Internal.Fee
    ( Fee (..)
    , fee
    , feeToIntegral
    ) where

import Prelude

import GHC.Generics
    ( Generic )
import Internal.SafeNatural
    ( SafeNatural )
import Quiet
    ( Quiet (Quiet) )

import qualified Internal.SafeNatural as SN

-- | Represents a fee to be paid on a transaction.
--
-- This type is isomorphic to 'Coin'.
--
newtype Fee = Fee { unFee :: SafeNatural }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet Fee)

instance Monoid Fee where
    mempty = Fee (SN.getSum mempty)

instance Semigroup Fee where
    Fee a <> Fee b = Fee $ a `SN.add` b

-- | A smart constructor for the 'Fee' type.
--
-- Returns a fee if (and only if) the given input is not negative.
--
fee :: Integral i => i -> Maybe Fee
fee = fmap Fee . SN.fromIntegral

-- | Converts the given fee value to an integral value.
--
feeToIntegral :: Integral i => Fee -> i
feeToIntegral = SN.toIntegral . unFee
