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
-- The 'CoinSelection' type represents a __coin selection__, the basis for a
-- /transaction/ in a UTxO-based blockchain.
--
-- The 'CoinSelectionAlgorithm' type provides a __common interface__ to
-- algorithms that generate coin selections.
--
module Cardano.CoinSelection
    (
      -- * Coin Selections
      CoinSelection (..)
    , sumInputs
    , sumOutputs
    , sumChange

      -- * Coin Selection Algorithms
    , CoinSelectionAlgorithm (..)
    , CoinSelectionParameters (..)
    , CoinSelectionResult (..)
    , CoinSelectionLimit (..)

      -- * Coins
    , Coin
    , coinFromNatural
    , coinToNatural

      -- * Coin Maps
    , CoinMap (..)
    , CoinMapEntry (..)
    , coinMapFromList
    , coinMapToList
    , coinMapValue

      -- * Coin Selection Errors
    , CoinSelectionError (..)
    , InputValueInsufficientError (..)
    , InputCountInsufficientError (..)
    , InputLimitExceededError (..)
    , InputsExhaustedError (..)

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

-- | Provides a __common interface__ for coin selection algorithms.
--
-- The function 'selectCoins', when applied to the given
-- 'CoinSelectionParameters' object (with /available inputs/ and /requested/
-- /outputs/), will generate a 'CoinSelectionResult' (with /remaining inputs/
-- and a /coin selection/).
--
newtype CoinSelectionAlgorithm i o m = CoinSelectionAlgorithm
    { selectCoins
        :: CoinSelectionParameters i o
        -> ExceptT CoinSelectionError m (CoinSelectionResult i o)
    }

-- | The complete set of parameters required for a 'CoinSelectionAlgorithm'.
--
-- The 'inputsAvailable' and 'outputsRequested' fields are both maps of unique
-- keys to associated 'Coin' values, where:
--
--   * Each key-value pair in the 'inputsAvailable' map corresponds to an
--     __unspent output__ from a previous transaction that is /available/
--     /for selection as an input/ by the coin selection algorithm. The /key/
--     is a unique reference to that output, and the /value/ is the amount of
--     unspent value associated with it.
--
--   * Each key-value pair in the 'outputsRequested' map corresponds to a
--     __payment__ whose value is /to be paid for/ by the coin selection
--     algorithm. The /key/ is a unique reference to a payment recipient,
--     and the /value/ is the amount of money to pay to that recipient.
--
-- A coin selection algorithm will select a __subset__ of inputs from
-- 'inputsAvailable' in order to pay for __all__ the outputs in
-- 'outputsRequested', where:
--
--   * Inputs __selected__ by the algorithm are included in the 'inputs'
--     set of the generated 'CoinSelection'.
--
--   * Inputs __not__ selected by the algorithm are included in the
--     'inputsRemaining' set of the 'CoinSelectionResult'.
--
-- The number of inputs that can selected is limited by 'limit'.
--
-- The total value of 'inputsAvailable' must be /greater than or equal to/
-- the total value of 'outputsRequested', as given by the 'coinMapValue'
-- function.
--
data CoinSelectionParameters i o = CoinSelectionParameters
    { inputsAvailable :: CoinMap i
        -- ^ The set of inputs available for selection.
    , outputsRequested :: CoinMap o
        -- ^ The set of outputs requested for payment.
    , limit :: CoinSelectionLimit
        -- ^ A limit on the number of inputs that can be selected.
    }
    deriving Generic

-- | Represents the __result__ of running a coin selection algorithm.
--
-- See 'CoinSelectionAlgorithm'.
--
data CoinSelectionResult i o = CoinSelectionResult
    { coinSelection :: CoinSelection i o
        -- ^ The generated coin selection.
    , inputsRemaining :: CoinMap i
        -- ^ The set of inputs that were __not__ selected.
    }

-- | A __coin selection__ is the basis for a /transaction/.
--
-- It consists of a selection of 'inputs', 'outputs', and 'change'.
--
-- The 'inputs' and 'outputs' fields are both maps of unique keys to associated
-- 'Coin' values, where:
--
--   * Each key-value pair in the 'inputs' map corresponds to an
--     __unspent output__ from a previous transaction (also known as a UTxO).
--     The /key/ is a unique reference to that output, and the /value/ is the
--     amount of unspent value associated with it.
--
--   * Each key-value pair in the 'outputs' map corresponds to a __payment__.
--     The /key/ is a unique reference to a payment recipient, and the /value/
--     is the amount of money to pay to that recipient.
--
-- The 'change' field is a set of coins to be returned to the originator of the
-- transaction.
--
-- The 'CoinSelectionAlgorithm' type provides a common interface for generating
-- coin selections.
--
data CoinSelection i o = CoinSelection
    { inputs :: CoinMap i
      -- ^ The set of inputs.
    , outputs :: CoinMap o
      -- ^ The set of outputs.
    , change :: [Coin]
      -- ^ The set of change.
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

-- | Defines an __inclusive upper bound__ on the /number/ of inputs that
--   a 'CoinSelectionAlgorithm' is allowed to select.
--
newtype CoinSelectionLimit = CoinSelectionLimit
    { calculateLimit
        :: Word8 -> Word8
            -- ^ Calculate the maximum number of inputs allowed for a given
            -- number of outputs.
    } deriving Generic

-- | Represents the set of possible failures that can occur when attempting
--   to produce a 'CoinSelection' with a 'CoinSelectionAlgorithm'.
--
-- See 'selectCoins'.
--
data CoinSelectionError
    = InputValueInsufficient
        InputValueInsufficientError
    | InputCountInsufficient
        InputCountInsufficientError
    | InputLimitExceeded
        InputLimitExceededError
    | InputsExhausted
        InputsExhaustedError
    deriving (Eq, Show)

-- | Indicates that the total value of 'inputsAvailable' is less than the total
--   value of 'outputsRequested', making it /impossible/ to cover all payments,
--   /regardless/ of which algorithm is chosen.
--
data InputValueInsufficientError =
    InputValueInsufficientError
    { inputValueAvailable :: Coin
        -- ^ The total value of 'inputsAvailable'.
    , inputValueRequired :: Coin
        -- ^ The total value of 'outputsRequested'.
    }
    deriving (Eq, Show)

-- | Indicates that the total count of entries in 'inputsAvailable' is /fewer/
--   /than/ required by the algorithm. The number required depends on the
--   particular algorithm implementation.
--
data InputCountInsufficientError =
    InputCountInsufficientError
    { inputCountAvailable :: Natural
        -- ^ The number of entries in 'inputsAvailable'.
    , inputCountRequired :: Natural
        -- ^ The number of entries required.
    }
    deriving (Eq, Show)

-- | Indicates that all available entries in 'inputsAvailable' were depleted
--   /before/ all the payments in 'outputsRequested' could be paid for.
--
-- This condition can occur /even if/ the total value of 'inputsAvailable' is
-- greater than or equal to the total value of 'outputsRequested', due to
-- differences in the way that algorithms select inputs.
--
data InputsExhaustedError =
    InputsExhaustedError
    deriving (Eq, Show)

-- | Indicates that the coin selection algorithm is unable to cover the total
--   value of 'outputsRequested' without exceeding the maximum number of inputs
--   defined by 'limit'.
--
-- See 'calculateLimit'.
--
newtype InputLimitExceededError =
    InputLimitExceededError
    { calculatedInputLimit :: Word8 }
    deriving (Eq, Show)

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
