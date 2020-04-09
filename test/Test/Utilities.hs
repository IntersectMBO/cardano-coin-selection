{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}

-- | Utility functions and types used purely for testing.
--
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Test.Utilities
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

    ) where

import Prelude

import Cardano.Types
    ( Coin (..) )
import Control.DeepSeq
    ( NFData (..) )
import Data.ByteArray
    ( ByteArrayAccess )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..), fmt, ordinalF, prefixF, suffixF )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Symbol )
import Quiet
    ( Quiet (Quiet) )

import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                                  Addresses
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
                                   Hashes
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
                                Formatting
-------------------------------------------------------------------------------}

-- | A polymorphic wrapper type with a custom 'Show' instance to display data
--   through 'Buildable' instances.
newtype ShowFmt a = ShowFmt { unShowFmt :: a }
    deriving (Generic, Eq, Ord)

instance NFData a => NFData (ShowFmt a)

instance Buildable a => Show (ShowFmt a) where
    show (ShowFmt a) = fmt (build a)

{-------------------------------------------------------------------------------
                                Transactions
-------------------------------------------------------------------------------}

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
    { address
        :: !Address
    , coin
        :: !Coin
    } deriving (Show, Generic, Eq, Ord)

instance NFData TxOut

instance Buildable TxOut where
    build txout = mempty
        <> build (coin txout)
        <> " @ "
        <> prefixF 8 addrF
        <> "..."
        <> suffixF 8 addrF
      where
        addrF = build $ address txout
