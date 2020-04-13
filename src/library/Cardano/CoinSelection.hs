{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

      -- * UTxO
    , UTxO (..)
    , utxoBalance
    , utxoPickRandom

      -- * Coin Selection
    , CoinSelection (..)
    , Input (..)
    , Output (..)

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

import Control.DeepSeq
    ( NFData (..) )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Crypto.Number.Generate
    ( generateBetween )
import Crypto.Random.Types
    ( MonadRandom )
import Data.List
    ( foldl' )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word64, Word8 )
import Fmt
    ( Buildable (..), blockListF, blockListF', listF, nameF )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (Quiet) )

import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------

-- | A non-negative integer value that represents a number of Lovelace.
--
-- One Ada is equal to 1,000,000 Lovelace.
--
newtype Coin = Coin
    { getCoin :: Word64 }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet Coin)

instance NFData Coin

instance Bounded Coin where
    minBound =
        Coin 0
    maxBound =
        Coin 45_000_000_000_000_000
        -- = 45 billion Ada × 1 million Lovelace/Ada:

instance Buildable Coin where
    build = build . getCoin

coinIsValid :: Coin -> Bool
coinIsValid c = c >= minBound && c <= maxBound

--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

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

-- | Selects an element at random from a UTxO set, returning both the selected
--   entry and the UTxO set with the element removed.
--
-- If the given UTxO set is empty, this function returns 'Nothing'.
--
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
newtype CoinSelectionAlgorithm i o u m e = CoinSelectionAlgorithm
    { selectCoins
        :: CoinSelectionOptions i o e
        -> NonEmpty (Output o)
        -> UTxO u
        -> ExceptT (CoinSelectionError e) m (CoinSelection i o, UTxO u)
    }

-- | Represents the result of running a /coin selection algorithm/.
--
-- See 'CoinSelectionAlgorithm'.
--
data CoinSelection i o = CoinSelection
    { inputs :: [Input i]
      -- ^ A /subset/ of the original 'UTxO' that was passed to the coin
      -- selection algorithm, containing only the entries that were /selected/
      -- by the coin selection algorithm.
    , outputs :: [Output o]
      -- ^ The original set of output payments passed to the coin selection
      -- algorithm, whose total value is covered by the 'inputs'.
    , change :: [Coin]
      -- ^ A set of change values to be paid back to the originator of the
      -- payment.
    } deriving (Generic, Show, Eq)

-- NOTE:
--
-- We don't check for duplicates when combining selections because we assume
-- they are constructed from independent elements.
--
-- As an alternative to the current implementation, we could 'nub' the list or
-- use a 'Set'.
--
instance Semigroup (CoinSelection i o) where
    a <> b = CoinSelection
        { inputs = inputs a <> inputs b
        , outputs = outputs a <> outputs b
        , change = change a <> change b
        }

instance Monoid (CoinSelection i o) where
    mempty = CoinSelection [] [] []

instance (Buildable i, Buildable o) => Buildable (CoinSelection i o) where
    build s = mempty
        <> nameF "inputs"
            (blockListF' "-" build $ inputs s)
        <> nameF "outputs"
            (blockListF $ outputs s)
        <> nameF "change"
            (listF $ change s)

-- | An input for a coin selection.
--
-- See 'CoinSelection'.
--
data Input i = Input
    { inputId
        :: !i
        -- ^ A unique identifier for this input.
    , inputValue
        :: !Coin
    } deriving (Eq, Generic, Ord, Show)

instance Buildable i => Buildable (Input i) where
    build i = mempty
        <> build (inputValue i)
        <> " @ "
        <> build (inputId i)

-- | An output for a coin selection.
--
-- See 'CoinSelection'.
--
data Output o = Output
    { outputId
        :: !o
        -- ^ A unique identifier for this output.
    , outputValue
        :: !Coin
    } deriving (Eq, Generic, Ord, Show)

instance Buildable o => Buildable (Output o) where
    build o = mempty
        <> build (outputValue o)
        <> " @ "
        <> build (outputId o)

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
inputBalance :: CoinSelection i o -> Word64
inputBalance =  foldl' (\total -> addCoin total . inputValue) 0 . inputs

-- | Calculate the total sum of all 'outputs' for the given 'CoinSelection'.
outputBalance :: CoinSelection i o -> Word64
outputBalance =  foldl' (\total -> addCoin total . outputValue) 0 . outputs

-- | Calculate the total sum of all 'change' for the given 'CoinSelection'.
changeBalance :: CoinSelection i o -> Word64
changeBalance = foldl' addCoin 0 . change

-- | Calculates the fee associated with a given 'CoinSelection'.
feeBalance :: CoinSelection i o -> Word64
feeBalance sel = inputBalance sel - outputBalance sel - changeBalance sel

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

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

addCoin :: Integral a => a -> Coin -> a
addCoin total c = total + (fromIntegral (getCoin c))
