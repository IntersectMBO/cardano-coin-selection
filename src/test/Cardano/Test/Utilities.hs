{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utility functions, types, and type class instances used purely for testing.
--
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Cardano.Test.Utilities
    (
    -- * Input Identifiers
      InputId
    , mkInputId
    , genInputId

    -- * Output Identifiers
    , OutputId
    , mkOutputId
    , genOutputId

    -- * Formatting
    , ShowFmt (..)

    -- * UTxO Operations
    , excluding
    , isSubsetOf
    , restrictedBy
    , restrictedTo

    -- * Unsafe Operations
    , unsafeCoin
    , unsafeDustThreshold
    , unsafeFee
    , unsafeFromHex

    ) where

import Prelude

import Cardano.CoinSelection
    ( CoinMap (..), CoinMapEntry (..), CoinSelection (..), coinMapToList )
import Cardano.CoinSelection.Fee
    ( DustThreshold (..), Fee (..) )
import Control.DeepSeq
    ( NFData (..) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (..), blockListF, fmt, listF, nameF )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )
import Internal.Coin
    ( Coin, coinFromIntegral )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Gen, arbitraryBoundedIntegral, vectorOf )

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Internal.Coin as C

--------------------------------------------------------------------------------
-- Unique Identifiers
--------------------------------------------------------------------------------

newtype UniqueId (tag :: Symbol) = UniqueId { unUniqueId :: ByteString }
    deriving stock (Eq, Generic, Ord)

instance NFData (UniqueId tag)

-- Generate a unique identifier of a given length in bytes.
genUniqueId :: Int -> Gen (UniqueId tag)
genUniqueId n = UniqueId . BS.pack <$> vectorOf n arbitraryBoundedIntegral

instance forall tag . KnownSymbol tag => Show (UniqueId tag) where
    show
        = ((<>) (symbolVal (Proxy :: Proxy tag)))
        . ((<>) " ")
        . T.unpack
        . T.decodeUtf8
        . convertToBase Base16
        . unUniqueId

instance KnownSymbol tag => Buildable (UniqueId tag) where
    build = build . show

--------------------------------------------------------------------------------
-- Input Identifiers
--------------------------------------------------------------------------------

type InputId = UniqueId "InputId"

mkInputId :: ByteString -> InputId
mkInputId = UniqueId

genInputId :: Int -> Gen InputId
genInputId = genUniqueId

--------------------------------------------------------------------------------
-- Output Identifiers
--------------------------------------------------------------------------------

type OutputId = UniqueId "OutputId"

genOutputId :: Int -> Gen OutputId
genOutputId = genUniqueId

mkOutputId :: ByteString -> OutputId
mkOutputId = UniqueId

--------------------------------------------------------------------------------
-- Unsafe Operations
--------------------------------------------------------------------------------

unsafeCoin :: (Integral i, Show i) => i -> Coin
unsafeCoin i = fromMaybe die $ coinFromIntegral i
  where
    die = error $ mconcat
        [ "Test suite attempted to create a coin with negative value: "
        , show i
        ]

unsafeDustThreshold :: (Integral i, Show i) => i -> DustThreshold
unsafeDustThreshold i = DustThreshold $ fromMaybe die $ coinFromIntegral i
  where
    die = error $ mconcat
        [ "Test suite attempted to create a dust theshold with negative value: "
        , show i
        ]

unsafeFee :: (Integral i, Show i) => i -> Fee
unsafeFee i = Fee $ fromMaybe die $ coinFromIntegral i
  where
    die = error $ mconcat
        [ "Test suite attempted to create a fee with negative value: "
        , show i
        ]

-- | Decode an hex-encoded 'ByteString' into raw bytes, or fail.
unsafeFromHex :: HasCallStack => ByteString -> ByteString
unsafeFromHex =
    either (error . show) id . convertFromBase @ByteString @ByteString Base16

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

-- | A polymorphic wrapper type with a custom 'Show' instance to display data
--   through 'Buildable' instances.
newtype ShowFmt a = ShowFmt { unShowFmt :: a }
    deriving (Generic, Eq, Ord)

instance NFData a => NFData (ShowFmt a)

instance Buildable a => Show (ShowFmt a) where
    show (ShowFmt a) = fmt (build a)

--------------------------------------------------------------------------------
-- UTxO Operations
--------------------------------------------------------------------------------

-- | ins⋪ u
excluding :: Ord u => CoinMap u -> Set u -> CoinMap u
excluding (CoinMap utxo) =
    CoinMap . Map.withoutKeys utxo

-- | a ⊆ b
isSubsetOf :: Ord u => CoinMap u -> CoinMap u -> Bool
isSubsetOf (CoinMap a) (CoinMap b) =
    a `Map.isSubmapOf` b

-- | ins⊲ u
restrictedBy :: Ord u => CoinMap u -> Set u -> CoinMap u
restrictedBy (CoinMap utxo) =
    CoinMap . Map.restrictKeys utxo

-- | u ⊳ outs
restrictedTo :: CoinMap u -> Set Coin -> CoinMap u
restrictedTo (CoinMap utxo) outs =
    CoinMap $ Map.filter (`Set.member` outs) utxo

--------------------------------------------------------------------------------
-- Buildable Instances
--------------------------------------------------------------------------------

instance Buildable Coin where
    build = build . fromIntegral @Natural @Integer . C.coinToIntegral

instance Buildable a => Buildable (CoinMapEntry a) where
    build a = mempty
        <> build (entryKey a)
        <> ":"
        <> build (entryValue a)

instance (Buildable i, Buildable o) => Buildable (CoinSelection i o) where
    build s = mempty
        <> nameF "inputs"
            (blockListF $ coinMapToList $ inputs s)
        <> nameF "outputs"
            (blockListF $ coinMapToList $ outputs s)
        <> nameF "change"
            (listF $ change s)
