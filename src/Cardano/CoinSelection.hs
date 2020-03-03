{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
      -- * Coin Selection
      CoinSelection(..)
    , inputBalance
    , outputBalance
    , changeBalance
    , feeBalance
    , ErrCoinSelection (..)
    , CoinSelectionOptions (..)
    ) where

import Prelude

import Cardano.Types
    ( Coin (..), TxIn, TxOut (..) )
import Data.List
    ( foldl' )
import Data.Word
    ( Word64, Word8 )
import Fmt
    ( Buildable (..), blockListF, blockListF', listF, nameF )
import GHC.Generics
    ( Generic )

{-------------------------------------------------------------------------------
                                Coin Selection
-------------------------------------------------------------------------------}

data CoinSelection = CoinSelection
    { inputs  :: [(TxIn, TxOut)]
      -- ^ Picked inputs.
    , outputs :: [TxOut]
      -- ^ Picked outputs.
    , change  :: [Coin]
      -- ^ Resulting change.
    } deriving (Generic, Show, Eq)

-- NOTE:
--
-- We don't check for duplicates when combining selections because we assume
-- they are constructed from independent elements.
--
-- As an alternative to the current implementation, we could 'nub' the list or
-- use a 'Set'.
--
instance Semigroup CoinSelection where
    a <> b = CoinSelection
        { inputs = inputs a <> inputs b
        , outputs = outputs a <> outputs b
        , change = change a <> change b
        }

instance Monoid CoinSelection where
    mempty = CoinSelection [] [] []

instance Buildable CoinSelection where
    build (CoinSelection inps outs chngs) = mempty
        <> nameF "inputs" (blockListF' "-" inpsF inps)
        <> nameF "outputs" (blockListF outs)
        <> nameF "change" (listF chngs)
      where
        inpsF (txin, txout) = build txin <> " (~ " <> build txout <> ")"

data CoinSelectionOptions e = CoinSelectionOptions
    { maximumNumberOfInputs
        :: Word8 -> Word8
            -- ^ Calculate the maximum number of inputs allowed for a given
            -- number of outputs.
    , validate
        :: CoinSelection -> Either e ()
            -- ^ Validate the given coin selection, returning a backend-specific
            -- error.
    } deriving (Generic)

-- | Calculate the sum of all input values.
inputBalance :: CoinSelection -> Word64
inputBalance =  foldl' (\total -> addTxOut total . snd) 0 . inputs

-- | Calculate the sum of all output values.
outputBalance :: CoinSelection -> Word64
outputBalance = foldl' addTxOut 0 . outputs

-- | Calculate the sum of all output values.
changeBalance :: CoinSelection -> Word64
changeBalance = foldl' addCoin 0 . change

feeBalance :: CoinSelection -> Word64
feeBalance sel = inputBalance sel - outputBalance sel - changeBalance sel

addTxOut :: Integral a => a -> TxOut -> a
addTxOut total = addCoin total . coin

addCoin :: Integral a => a -> Coin -> a
addCoin total c = total + (fromIntegral (getCoin c))

data ErrCoinSelection e
    = ErrNotEnoughMoney Word64 Word64
    -- ^ The UTxO was exhausted during input selection.
    --
    -- Records the balance of the UTxO, as well as the size of the payment we
    -- tried to make.
    --
    | ErrUtxoNotEnoughFragmented Word64 Word64
    -- ^ The UTxO was not fragmented enough to support the required number of
    -- transaction outputs.
    --
    -- Records the number of UTxO entries, as well as the number of the
    -- transaction outputs.
    --
    | ErrMaximumInputsReached Word64
    -- ^ The maximum number of allowed inputs was not enough to cover the total
    -- payment amount.
    --
    | ErrInputsDepleted
    -- ^ All available inputs were depleted, even though the UTxO was
    -- sufficiently fragmented and with enough funds to cover payment.
    --
    | ErrInvalidSelection e
    -- ^ The generated coin selection was reported to be invalid by the backend.
    --
    -- Records the backend-specific error that occurred while attempting to
    -- validate the selection.
    --
    deriving (Show, Eq)
