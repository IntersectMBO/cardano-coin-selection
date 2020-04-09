{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Cardano.Types
    (
    -- * Coin
      Coin (..)
    , coinIsValid

    -- * UTxO
    , UTxO (..)
    , utxoBalance
    , utxoPickRandom

    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..) )
import Crypto.Number.Generate
    ( generateBetween )
import Crypto.Random.Types
    ( MonadRandom )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), blockListF' )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (Quiet) )

import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
                                     Coin
-------------------------------------------------------------------------------}

-- | Coins are stored as Lovelace (reminder: 1 Lovelace = 1e-6 ADA)
newtype Coin = Coin
    { getCoin :: Word64 }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet Coin)

instance NFData Coin

instance Bounded Coin where
    minBound = Coin 0
    maxBound = Coin 45000000000000000

instance Buildable Coin where
    build = build . getCoin

coinIsValid :: Coin -> Bool
coinIsValid c = c >= minBound && c <= maxBound

{-------------------------------------------------------------------------------
                                    UTxO
-------------------------------------------------------------------------------}

newtype UTxO u = UTxO
    { getUTxO :: Map u Coin }
    deriving stock (Eq, Generic, Ord)
    deriving newtype (Semigroup, Monoid)
    deriving Show via (Quiet (UTxO u))

instance NFData u => NFData (UTxO u)

instance Buildable u => Buildable (UTxO u) where
    build (UTxO utxo) =
        blockListF' "-" utxoF (Map.toList utxo)
      where
        utxoF (inp, out) = build inp <> " => " <> build out

-- | Pick a random element from a UTxO, returns 'Nothing' if the UTxO is empty.
-- Otherwise, returns the selected entry and, the UTxO minus the selected one.
utxoPickRandom
    :: MonadRandom m
    => UTxO u
    -> m (Maybe (u, Coin), UTxO u)
utxoPickRandom (UTxO utxo)
    | Map.null utxo =
        return (Nothing, UTxO utxo)
    | otherwise = do
        ix <- fromEnum <$> generateBetween 0 (toEnum (Map.size utxo - 1))
        return (Just $ Map.elemAt ix utxo, UTxO $ Map.deleteAt ix utxo)

-- | Compute the balance of a UTxO.
utxoBalance :: UTxO u -> Natural
utxoBalance =
    Map.foldl' fn 0 . getUTxO
  where
    fn :: Natural -> Coin -> Natural
    fn tot out = tot + fromIntegral (getCoin out)
