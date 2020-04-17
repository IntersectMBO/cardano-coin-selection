{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
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
    -- * Addresses
      Address (..)

    -- * Hashes
    , Hash (..)

    -- * Formatting
    , ShowFmt (..)

    -- * Transactions
    , TxIn (..)
    , TxOut (..)

    -- * UTxO Operations
    , excluding
    , isSubsetOf
    , restrictedBy
    , restrictedTo

    -- * Unsafe Operations
    , unsafeCoin
    , unsafeDustThreshold
    , unsafeFromHex

    ) where

import Prelude

import Cardano.CoinSelection
    ( CoinMap (..), CoinMapEntry (..), CoinSelection (..), coinMapToList )
import Control.DeepSeq
    ( NFData (..) )
import Data.ByteArray
    ( ByteArrayAccess )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Data.Set
    ( Set )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..)
    , blockListF
    , fmt
    , listF
    , nameF
    , ordinalF
    , prefixF
    , suffixF
    )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( Symbol )
import Internal.Coin
    ( Coin (..), coin )
import Internal.DustThreshold
    ( DustThreshold (..), dustThreshold )
import Internal.SafeNatural
    ( SafeNatural )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (Quiet) )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import qualified Internal.SafeNatural as SN

--------------------------------------------------------------------------------
-- Addresses
--------------------------------------------------------------------------------

newtype Address = Address
    { unAddress :: ByteString }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet Address)

instance NFData Address

instance Buildable Address where
    build addr = mempty
        <> prefixF 8 addrF
        <> "..."
        <> suffixF 8 addrF
      where
        addrF = build (toText addr)
        toText = T.decodeUtf8
            . convertToBase Base16
            . unAddress

--------------------------------------------------------------------------------
-- Unsafe Operations
--------------------------------------------------------------------------------

unsafeCoin :: (Integral i, Show i) => i -> Coin
unsafeCoin i = fromMaybe die $ coin i
  where
    die = error $ mconcat
        [ "Test suite attempted to create a coin with negative value: "
        , show i
        ]

unsafeDustThreshold :: (Integral i, Show i) => i -> DustThreshold
unsafeDustThreshold i = fromMaybe die $ dustThreshold i
  where
    die = error $ mconcat
        [ "Test suite attempted to create a dust theshold with negative value: "
        , show i
        ]

-- | Decode an hex-encoded 'ByteString' into raw bytes, or fail.
unsafeFromHex :: HasCallStack => ByteString -> ByteString
unsafeFromHex =
    either (error . show) id . convertFromBase @ByteString @ByteString Base16

--------------------------------------------------------------------------------
-- Hashes
--------------------------------------------------------------------------------

newtype Hash (tag :: Symbol) = Hash { getHash :: ByteString }
    deriving stock (Eq, Generic, Ord)
    deriving newtype (ByteArrayAccess)
    deriving Show via (Quiet (Hash tag))

instance NFData (Hash tag)

instance Buildable (Hash tag) where
    build h = mempty
        <> prefixF 8 builder
      where
        builder = build . toText $ h
        toText = T.decodeUtf8 . convertToBase Base16 . getHash

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
-- Transactions
--------------------------------------------------------------------------------

data TxIn = TxIn
    { txinId
        :: !(Hash "Tx")
    , txinIx
        :: !Word32
    } deriving (Show, Generic, Eq, Ord)

instance NFData TxIn

instance Buildable TxIn where
    build txin = mempty
        <> ordinalF (txinIx txin + 1)
        <> " "
        <> build (txinId txin)

data TxOut = TxOut
    { txoutAddress
        :: !Address
    , txoutCoin
        :: !Coin
    } deriving (Show, Generic, Eq, Ord)

instance Buildable TxOut where
    build txout = mempty
        <> build (txoutCoin txout)
        <> " @ "
        <> prefixF 8 addrF
        <> "..."
        <> suffixF 8 addrF
      where
        addrF = build $ txoutAddress txout

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
    build = build . unCoin

instance Buildable SafeNatural where
    build = build . fromIntegral @Natural @Integer . SN.toIntegral

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
