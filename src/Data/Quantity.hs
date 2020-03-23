{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Representation of values with an associated (free) unit of measure. Useful to
-- disambiguate primitive types like 'Int' or 'String' which can be in different
-- bases depending on the context.

module Data.Quantity
    ( -- * Polymorphic Quantity
      Quantity(..)
    ) where

import Prelude

import Control.DeepSeq
    ( NFData )
import Data.Proxy
    ( Proxy (..) )
import Fmt
    ( Buildable (..), fmt )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )
import Quiet
    ( Quiet (Quiet) )

-- | @Quantity (unit :: Symbol) a@ is a primitive @a@  multiplied by an @unit@.
--
-- Example:
--
-- Instead of providing the unit implicitly as a comment, or a part of a name
--
-- >>> a :: Word32 -- in lovelace
--
-- we can write
--
-- >>> a :: Quantity "lovelace" Word32
--
-- which now has a different type from
--
-- >>> b :: Quantity "lovelace/byte" Word32
--
-- so mixing them up is more difficult.
--
-- The unit is mostly a phantom type, but it is also included in the
-- @ToJSON@/@FromJSON@ instances.
--
-- >>> Aeson.encode $ Quantity @"lovelace" 14
-- {"unit":"lovelace","quantity":14}
newtype Quantity (unit :: Symbol) a = Quantity { getQuantity :: a }
    deriving stock (Eq, Generic, Ord)
    deriving newtype (Bounded, Enum)
    deriving Show via (Quiet (Quantity unit a))

instance Functor (Quantity any) where
    fmap f (Quantity a) = Quantity (f a)

instance NFData a => NFData (Quantity unit a)

-- Builds (Quantity "lovelace" Word64) as "42 lovelace"
instance (KnownSymbol unit, Buildable a) => Buildable (Quantity unit a) where
    build (Quantity a) = build a <> fmt " " <> build u
      where
        u = symbolVal (Proxy :: Proxy unit)
