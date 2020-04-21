{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides general functions and types relating to coin selection.
--
-- The 'CoinSelectionAlgorithm' type provides a /common interface/ to coin
-- selection algorithms.
--
-- The 'CoinSelection' type represents the /result/ of running a coin selection
-- algorithm.
--
module Cardano.CoinSelection
    (
      -- * Coin
      Coin
    , coinFromNatural
    , coinToNatural

      -- * Coin Map
    , CoinMap (..)
    , CoinMapEntry (..)
    , coinMapFromList
    , coinMapToList
    , coinMapValue

      -- * Coin Selection
    , CoinSelection (..)
    , sumInputs
    , sumOutputs
    , sumChange

      -- * Coin Selection Algorithm
    , CoinSelectionAlgorithm (..)
    , CoinSelectionOptions (..)
    , CoinSelectionError (..)

      -- # Internal Functions
    , coinMapRandomEntry

    ) where

import Prelude

import Control.Arrow
    ( (&&&) )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Crypto.Number.Generate
    ( generateBetween )
import Crypto.Random.Types
    ( MonadRandom )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )
import Internal.Coin
    ( Coin, coinFromNatural, coinToNatural )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (Quiet) )

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Coin Map
--------------------------------------------------------------------------------

-- | A mapping from unique keys to associated 'Coin' values.
--
-- A 'CoinMap' can be used to represent:
--
--   * a UTxO set, where each key within the map refers to an unspent output
--     from a previous transaction.
--
--   * a set of 'inputs' to a 'CoinSelection', where each input is an entry
--     selected from a UTxO set by a 'CoinSelectionAlgorithm'.
--
--   * a set of 'outputs' for a 'CoinSelection', where each key within the map
--     refers to the address of a payment recipient.
--
-- A 'CoinMap' can be constructed with the 'coinMapFromList' function.
--
-- The total value of a 'CoinMap' is given by the 'coinMapValue' function.
--
newtype CoinMap a = CoinMap { unCoinMap :: Map a Coin }
    deriving (Eq, Generic)
    deriving Show via (Quiet (CoinMap a))

instance Foldable CoinMap where
    foldMap f = F.fold . fmap (f . entryKey) . coinMapToList

instance Ord a => Monoid (CoinMap a) where
    mempty = CoinMap mempty

instance Ord a => Semigroup (CoinMap a) where
    CoinMap a <> CoinMap b = CoinMap $ Map.unionWith (<>) a b

-- | An entry for a 'CoinMap'.
--
data CoinMapEntry a = CoinMapEntry
    { entryKey
        :: a
        -- ^ The unique key associated with this entry.
    , entryValue
        :: Coin
        -- ^ The coin value associated with this entry.
    } deriving (Eq, Generic, Ord, Show)

-- | Constructs a 'CoinMap' from a list of entries.
--
-- See 'CoinMapEntry'.
--
coinMapFromList :: Ord a => [CoinMapEntry a] -> CoinMap a
coinMapFromList = CoinMap
    . Map.fromListWith (<>)
    . fmap (entryKey &&& entryValue)

-- | Converts a 'CoinMap' to a list of entries.
--
-- See 'CoinMapEntry'.
--
coinMapToList :: CoinMap a -> [CoinMapEntry a]
coinMapToList = fmap (uncurry CoinMapEntry) . Map.toList . unCoinMap

-- | Calculates the total coin value associated with a 'CoinMap'.
--
coinMapValue :: CoinMap a -> Coin
coinMapValue = mconcat . fmap entryValue . coinMapToList

--------------------------------------------------------------------------------
-- Coin Selection
--------------------------------------------------------------------------------

-- | Provides a common interface for coin selection algorithms.
--
-- The function 'selectCoins', when applied to the given /initial UTxO set/
-- and /output set/, generates a 'CoinSelection' that is capable of paying
-- for all of the outputs, and a /remaining UTxO set/ from which all spent
-- values have been removed.
--
-- Each entry in the /initial UTxO set/ refers to a unique unspent output from
-- a previous transaction, together with its corresponding value. The algorithm
-- will select from among the entries in this set to pay for entries in the
-- output set, placing the selected entries in the 'inputs' field of the
-- resulting 'CoinSelection'.
--
-- Each entry in the /output set/ refers to a unique payment recipient together
-- with the value to pay to that recipient. The 'outputs' field of the
-- resulting 'CoinSelection' will be equal to this set.
--
-- The total value of the initial UTxO set must be /greater than or equal to/
-- the total value of the output set, as given by the 'coinMapValue' function.
--
newtype CoinSelectionAlgorithm i o m e = CoinSelectionAlgorithm
    { selectCoins
        :: CoinSelectionOptions i o e
        -> CoinMap i
        -> CoinMap o
        -> ExceptT (CoinSelectionError e) m (CoinSelection i o, CoinMap i)
    }

-- | Represents the /result/ of running a coin selection algorithm.
--
-- See 'CoinSelectionAlgorithm'.
--
data CoinSelection i o = CoinSelection
    { inputs :: CoinMap i
      -- ^ A /subset/ of the original UTxO set that was passed to the coin
      -- selection algorithm, containing only the entries that were /selected/
      -- by the coin selection algorithm.
    , outputs :: CoinMap o
      -- ^ The original set of output payments passed to the coin selection
      -- algorithm, whose total value is covered by the 'inputs'.
    , change :: [Coin]
      -- ^ A set of change values to be paid back to the originator of the
      -- payment.
    }
    deriving (Generic, Show, Eq)

instance (Ord i, Ord o) => Semigroup (CoinSelection i o) where
    a <> b = CoinSelection
        { inputs = inputs a <> inputs b
        , outputs = outputs a <> outputs b
        , change = change a <> change b
        }

instance (Ord i, Ord o) => Monoid (CoinSelection i o) where
    mempty = CoinSelection mempty mempty mempty

-- | Calculate the total sum of all 'inputs' for the given 'CoinSelection'.
sumInputs :: CoinSelection i o -> Coin
sumInputs = coinMapValue . inputs

-- | Calculate the total sum of all 'outputs' for the given 'CoinSelection'.
sumOutputs :: CoinSelection i o -> Coin
sumOutputs =  coinMapValue . outputs

-- | Calculate the total sum of all 'change' for the given 'CoinSelection'.
sumChange :: CoinSelection i o -> Coin
sumChange = mconcat . change

-- | Represents a set of options to be passed to a coin selection algorithm.
--
data CoinSelectionOptions i o e = CoinSelectionOptions
    { maximumInputCount
        :: Word8 -> Word8
            -- ^ Calculate the maximum number of inputs allowed for a given
            -- number of outputs.
    , validate
        :: CoinSelection i o -> Either e ()
            -- ^ Validate the given coin selection, returning a backend-specific
            -- error.
    } deriving (Generic)

-- | Represents the set of possible failures that can occur when attempting
--   to produce a 'CoinSelection'.
--
data CoinSelectionError e
    = ErrUtxoBalanceInsufficient Coin Coin
    -- ^ The UTxO balance was insufficient to cover the total payment amount.
    --
    -- Records the /UTxO balance/, as well as the /total value/ of the payment
    -- we tried to make.
    --
    | ErrUtxoNotFragmentedEnough Natural Natural
    -- ^ The UTxO was not fragmented enough to support the required number of
    -- transaction outputs.
    --
    -- Records the /number/ of UTxO entries, as well as the /number/ of the
    -- transaction outputs.
    --
    | ErrUtxoFullyDepleted
    -- ^ Due to the particular distribution of values within the UTxO set, all
    -- available UTxO entries were depleted before all the requested
    -- transaction outputs could be paid for.
    --
    | ErrMaximumInputCountExceeded Natural
    -- ^ The number of UTxO entries needed to cover the requested payment
    -- exceeded the upper limit specified by 'maximumInputCount'.
    --
    -- Records the value of 'maximumInputCount'.
    --
    | ErrInvalidSelection e
    -- ^ The coin selection generated was reported to be invalid by the backend.
    --
    -- Records the /backend-specific error/ that occurred while attempting to
    -- validate the selection.
    --
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Internal Functions
--------------------------------------------------------------------------------

-- Selects an entry at random from a 'CoinMap', returning both the selected
-- entry and the map with the entry removed.
--
-- If the given map is empty, this function returns 'Nothing'.
--
coinMapRandomEntry
    :: MonadRandom m
    => CoinMap a
    -> m (Maybe (CoinMapEntry a), CoinMap a)
coinMapRandomEntry (CoinMap m)
    | Map.null m =
        return (Nothing, CoinMap m)
    | otherwise = do
        ix <- fromEnum <$> generateBetween 0 (toEnum (Map.size m - 1))
        let entry = uncurry CoinMapEntry $ Map.elemAt ix m
        let remainder = CoinMap $ Map.deleteAt ix m
        return (Just entry, remainder)
