{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
module Cardano.Types
    (
    -- * Coin
      Coin (..)
    , isValidCoin

    -- * UTxO
    , UTxO (..)
    , balance
    , pickRandom
    , excluding
    , isSubsetOf
    , restrictedBy
    , restrictedTo
    , Dom (..)

    -- * Utilities
    , invariant
    , distance
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..) )
import Crypto.Number.Generate
    ( generateBetween )
import Crypto.Random.Types
    ( MonadRandom )
import Data.Kind
    ( Type )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
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
import qualified Data.Set as Set

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

isValidCoin :: Coin -> Bool
isValidCoin c = c >= minBound && c <= maxBound

{-------------------------------------------------------------------------------
                                    UTxO
-------------------------------------------------------------------------------}

newtype UTxO u = UTxO
    { getUTxO :: Map u Coin }
    deriving stock (Eq, Generic, Ord)
    deriving newtype (Semigroup, Monoid)
    deriving Show via (Quiet (UTxO u))

instance NFData u => NFData (UTxO u)

instance Dom (UTxO u) where
    type DomElem (UTxO u) = u
    dom (UTxO utxo) = Map.keysSet utxo

instance Buildable u => Buildable (UTxO u) where
    build (UTxO utxo) =
        blockListF' "-" utxoF (Map.toList utxo)
      where
        utxoF (inp, out) = build inp <> " => " <> build out

-- | Pick a random element from a UTxO, returns 'Nothing' if the UTxO is empty.
-- Otherwise, returns the selected entry and, the UTxO minus the selected one.
pickRandom
    :: MonadRandom m
    => UTxO u
    -> m (Maybe (u, Coin), UTxO u)
pickRandom (UTxO utxo)
    | Map.null utxo =
        return (Nothing, UTxO utxo)
    | otherwise = do
        ix <- fromEnum <$> generateBetween 0 (toEnum (Map.size utxo - 1))
        return (Just $ Map.elemAt ix utxo, UTxO $ Map.deleteAt ix utxo)

-- | Compute the balance of a UTxO.
balance :: UTxO u -> Natural
balance =
    Map.foldl' fn 0 . getUTxO
  where
    fn :: Natural -> Coin -> Natural
    fn tot out = tot + fromIntegral (getCoin out)

-- | ins⋪ u
excluding :: Ord u => UTxO u -> Set u -> UTxO u
excluding (UTxO utxo) =
    UTxO . Map.withoutKeys utxo

-- | a ⊆ b
isSubsetOf :: Ord u => UTxO u -> UTxO u -> Bool
isSubsetOf (UTxO a) (UTxO b) =
    a `Map.isSubmapOf` b

-- | ins⊲ u
restrictedBy :: Ord u => UTxO u -> Set u -> UTxO u
restrictedBy (UTxO utxo) =
    UTxO . Map.restrictKeys utxo

-- | u ⊳ outs
restrictedTo :: UTxO u -> Set Coin -> UTxO u
restrictedTo (UTxO utxo) outs =
    UTxO $ Map.filter (`Set.member` outs) utxo

{-------------------------------------------------------------------------------
                               Polymorphic Types
-------------------------------------------------------------------------------}

-- | Allows us to define the "domain" of any type — @UTxO@ in particular — and
-- use 'dom' to refer to the /inputs/ of an /UTxO/.
--
-- This is the terminology used in the [Formal Specification for a Cardano
-- Wallet](https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/wallet/formal-specification-for-a-cardano-wallet.pdf).
class Dom a where
    type DomElem a :: Type
    dom :: a -> Set (DomElem a)

-- | Checks whether or not an invariant holds, by applying the given predicate
--   to the given value.
--
-- If the invariant does not hold (indicated by the predicate function
-- returning 'False'), throws an error with the specified message.
--
-- >>> invariant "not empty" [1,2,3] (not . null)
-- [1, 2, 3]
--
-- >>> invariant "not empty" [] (not . null)
-- *** Exception: not empty
invariant
    :: String
        -- ^ The message
    -> a
        -- ^ The value to test
    -> (a -> Bool)
        -- ^ The predicate
    -> a
invariant msg a predicate =
    if predicate a then a else error msg

-- | Compute distance between two numeric values |a - b|
distance :: (Ord a, Num a) => a -> a -> a
distance a b =
    if a < b then b - a else a - b
