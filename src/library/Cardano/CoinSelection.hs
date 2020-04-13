{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides general functions and types relating to coin selection and fee
-- balancing.
--
module Cardano.CoinSelection
    (
      -- * Coin
      Coin (..)
    , coinIsValid

      -- * Coin Map
    , CoinMap (..)
    , CoinMapEntry (..)
    , coinMapFromList
    , coinMapToList
    , coinMapValue
    , coinMapRandomEntry

      -- * Coin Selection
    , CoinSelection (..)

      -- * Coin Selection Algorithm
    , CoinSelectionAlgorithm (..)
    , CoinSelectionOptions (..)
    , CoinSelectionError (..)

      -- * Calculating Balances
    , inputBalance
    , outputBalance
    , changeBalance
    , feeBalance

    ) where

import Prelude

import Control.Arrow
    ( (&&&) )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Crypto.Number.Generate
    ( generateBetween )
import Crypto.Random.Types
    ( MonadRandom )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word64, Word8 )
import Fmt
    ( Buildable (..), blockListF, listF, nameF )
import GHC.Generics
    ( Generic )
import Quiet
    ( Quiet (Quiet) )

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------

-- | A non-negative integer value that represents a number of Lovelace.
--
-- One Ada is equal to 1,000,000 Lovelace.
--
newtype Coin = Coin
    { unCoin :: Word64 }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet Coin)

instance Monoid Coin where
    mempty = minBound

instance Semigroup Coin where
    Coin a <> Coin b = Coin (a + b)

instance NFData Coin

instance Bounded Coin where
    minBound =
        Coin 0
    maxBound =
        Coin 45_000_000_000_000_000
        -- = 45 billion Ada × 1 million Lovelace/Ada:

instance Buildable Coin where
    build = build . unCoin

coinIsValid :: Coin -> Bool
coinIsValid c = c >= minBound && c <= maxBound

--------------------------------------------------------------------------------
-- Coin Map
--------------------------------------------------------------------------------

newtype CoinMap a = CoinMap { unCoinMap :: Map a Coin }
    deriving (Eq, Generic)
    deriving Show via (Quiet (CoinMap a))

instance Foldable CoinMap where
    foldMap f = F.fold . fmap (f . entryKey) . coinMapToList

instance Ord a => Monoid (CoinMap a) where
    mempty = CoinMap mempty

instance Ord a => Semigroup (CoinMap a) where
    CoinMap a <> CoinMap b = CoinMap $ Map.unionWith (<>) a b

data CoinMapEntry a = CoinMapEntry
    { entryKey
        :: a
    , entryValue
        :: Coin
    } deriving (Eq, Generic, Ord, Show)

instance Buildable a => Buildable (CoinMapEntry a) where
    build a = mempty
        <> build (entryKey a)
        <> ":"
        <> build (entryValue a)

coinMapFromList :: Ord a => [CoinMapEntry a] -> CoinMap a
coinMapFromList = CoinMap
    . Map.fromListWith (<>)
    . fmap (entryKey &&& entryValue)

coinMapToList :: CoinMap a -> [CoinMapEntry a]
coinMapToList = fmap (uncurry CoinMapEntry) . Map.toList . unCoinMap

coinMapValue :: CoinMap a -> Coin
coinMapValue = mconcat . fmap entryValue . coinMapToList

-- | Selects an entry at random from a 'CoinMap', returning both the selected
--   entry and the map with the entry removed.
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

--------------------------------------------------------------------------------
-- Coin Selection
--------------------------------------------------------------------------------

-- | Represents a /coin selection algorithm/.
--
-- The function 'selectCoins', when applied to the given /output list/ and
-- /initial UTxO set/, generates a 'CoinSelection' that is capable of paying
-- for all of the outputs, and a /remaining UTxO set/ from which all spent
-- values have been removed.
--
newtype CoinSelectionAlgorithm i o m e = CoinSelectionAlgorithm
    { selectCoins
        :: CoinSelectionOptions i o e
        -> CoinMap i
        -> CoinMap o
        -> ExceptT (CoinSelectionError e) m (CoinSelection i o, CoinMap i)
    }

-- | Represents the result of running a /coin selection algorithm/.
--
-- See 'CoinSelectionAlgorithm'.
--
data CoinSelection i o = CoinSelection
    { inputs :: CoinMap i
      -- ^ A /subset/ of the original 'UTxO' that was passed to the coin
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

instance (Buildable i, Buildable o) => Buildable (CoinSelection i o) where
    build s = mempty
        <> nameF "inputs"
            (blockListF $ coinMapToList $ inputs s)
        <> nameF "outputs"
            (blockListF $ coinMapToList $ outputs s)
        <> nameF "change"
            (listF $ change s)

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

-- | Calculate the total sum of all 'inputs' for the given 'CoinSelection'.
inputBalance :: CoinSelection i o -> Coin
inputBalance = coinMapValue . inputs

-- | Calculate the total sum of all 'outputs' for the given 'CoinSelection'.
outputBalance :: CoinSelection i o -> Coin
outputBalance =  coinMapValue . outputs

-- | Calculate the total sum of all 'change' for the given 'CoinSelection'.
changeBalance :: CoinSelection i o -> Coin
changeBalance = mconcat . change

-- | Calculates the fee associated with a given 'CoinSelection'.
feeBalance :: CoinSelection i o -> Coin
feeBalance sel = Coin
    $ unCoin ( inputBalance sel)
    - unCoin (outputBalance sel)
    - unCoin (changeBalance sel)

-- | Represents the set of possible failures that can occur when attempting
--   to produce a 'CoinSelection'.
--
data CoinSelectionError e
    = ErrUtxoBalanceInsufficient Word64 Word64
    -- ^ The UTxO balance was insufficient to cover the total payment amount.
    --
    -- Records the /UTxO balance/, as well as the /total value/ of the payment
    -- we tried to make.
    --
    | ErrUtxoNotFragmentedEnough Word64 Word64
    -- ^ The UTxO was not fragmented enough to support the required number of
    -- transaction outputs.
    --
    -- Records the /number/ of UTxO entries, as well as the /number/ of the
    -- transaction outputs.
    --
    | ErrUxtoFullyDepleted
    -- ^ Due to the particular distribution of values within the UTxO set, all
    -- available UTxO entries were depleted before all the requested
    -- transaction outputs could be paid for.
    --
    | ErrMaximumInputCountExceeded Word64
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
