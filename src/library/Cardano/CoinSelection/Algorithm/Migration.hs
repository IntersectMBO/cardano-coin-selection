{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains an algorithm for migrating all funds from one wallet
-- to another.
--
-- See 'selectCoins'.
--
module Cardano.CoinSelection.Algorithm.Migration
    (
      -- * Coin Selection for Migration
      selectCoins
    , BatchSize (..)
    , idealBatchSize
    ) where

import Prelude

import Cardano.CoinSelection
    ( CoinMap
    , CoinMapEntry (..)
    , CoinSelection (..)
    , CoinSelectionLimit (..)
    , coinMapFromList
    , coinMapToList
    , coinMapValue
    , sumChange
    , sumInputs
    )
import Cardano.CoinSelection.Fee
    ( DustThreshold (..)
    , Fee (..)
    , FeeBalancingPolicy (..)
    , FeeEstimator (..)
    , FeeOptions (..)
    , isDust
    )
import Control.Monad.Trans.State
    ( State, evalState, get, put )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Maybe
    ( fromMaybe )
import Data.Word
    ( Word16 )
import GHC.Generics
    ( Generic )
import Internal.Coin
    ( Coin, coinFromIntegral, coinToIntegral )

import qualified Internal.Coin as C

--------------------------------------------------------------------------------
-- Coin Selection for Migration
--------------------------------------------------------------------------------

-- | Creates a __series__ of coin selections that, when published as
--   transactions, will have the effect of migrating all funds from one
--   wallet to another.
--
-- Since UTxO-based blockchains typically impose limits on the sizes of
-- individual transactions, and since individual UTxO sets can contain
-- /arbitrarily/ many entries, migrating all funds from one wallet to another
-- may require the creation of /several/ transactions.
--
-- This function therefore /partitions/ the given set of inputs into multiple
-- /batches/ of up to __/b/__ inputs, where __/b/__ is specified by the given
-- 'BatchSize' parameter. (See 'idealBatchSize' for an automatic way to
-- calculate a suitable batch size.)
--
-- For each batch of inputs, this function creates a separate 'CoinSelection'
-- with the given 'inputs' /and/ a generated 'change' set, where the 'change'
-- set represents the value to be transferred to the target wallet, carefully
-- adjusted to deduct a fee in accordance with the given 'FeeOptions'
-- parameter. The set of 'outputs' for each coin selection is /purposefully/
-- left empty, as /all/ value is captured in the 'change' set.
--
-- @since 1.0.0
selectCoins
    :: forall i o . (Ord i, Ord o)
    => FeeOptions i o
        -- ^ The fee options.
    -> BatchSize
        -- ^ The maximum number of inputs to include in each selection.
    -> CoinMap i
        -- ^ The UTxO set to migrate.
    -> [CoinSelection i o]
selectCoins options (BatchSize batchSize) utxo =
    evalState migrate (coinMapToList utxo)
  where
    FeeOptions {dustThreshold, feeEstimator, feeBalancingPolicy} = options

    migrate :: State [CoinMapEntry i] [CoinSelection i o]
    migrate = do
        batch <- getNextBatch
        if null batch then
            pure []
        else case adjustForFee (mkCoinSelection batch) of
            Nothing -> pure []
            Just coinSel -> do
                rest <- migrate
                pure (coinSel:rest)

    -- Construct a provisional 'CoinSelection' from the given selected inputs.
    -- Note that the selection may look a bit weird at first sight as it has
    -- no outputs (we are paying everything to ourselves!).
    mkCoinSelection :: [CoinMapEntry i] -> CoinSelection i o
    mkCoinSelection inputEntries = CoinSelection {inputs, outputs, change}
      where
        inputs = coinMapFromList inputEntries
        outputs = mempty
        change
            | null nonDustInputCoins && totalInputValue >= smallestNonDustCoin =
                [smallestNonDustCoin]
            | otherwise =
                nonDustInputCoins
        nonDustInputCoins = filter
            (not . isDust dustThreshold)
            (entryValue <$> inputEntries)
        smallestNonDustCoin = C.succ $ unDustThreshold dustThreshold
        totalInputValue = coinMapValue inputs

    -- | Attempt to balance the coin selection by reducing or increasing the
    -- change values based on the computed fees.
    adjustForFee :: CoinSelection i o -> Maybe (CoinSelection i o)
    adjustForFee !coinSel = case change coinSel of
        -- If there's no change, nothing to adjust
        [] -> Nothing

        -- No difference between required and computed, we're done
        (_ : _) | diff == 0 -> Just coinSel

        -- Otherwise, we have 2 cases:
        --
        -- 1/ diff < 0
        -- We aren't giving enough as fee, so we need to reduce one output.
        --
        -- 2/ diff > 0
        -- We have some surplus so we add it to an arbitrary output
        --
        -- If both cases we can simply modify one output by adding `diff`, the
        -- sign of `diff` making for the right modification.
        -- We then recursively call ourselves for this might reduce the number
        -- of outputs and change the fee.
        (c : cs) -> do
            let coinSel' = coinSel
                    { change = modifyFirst (c :| cs) (applyDiff diff) }
            let costOfSurplus
                    = fromIntegral
                    $ C.coinToNatural
                    $ C.distance
                        (unFee $ estimateFee feeEstimator coinSel')
                        (unFee $ estimateFee feeEstimator coinSel )
            if
                -- Adding the change costs less than not having it, so it's
                -- worth trying.
                | costOfSurplus < actualFee ->
                    adjustForFee coinSel'

                -- Adding the change costs more than not having it, If we don't
                -- require strict balancing, we can leave the selection as-is.
                | feeBalancingPolicy == RequireMinimalFee ->
                    pure coinSel

                -- Adding the change costs more than not having it. So,
                -- depending on our balancing policy, we may stop the balancing
                -- right here, or, if we must balance the selection discard the
                -- whole selection: it can't be balanced with this algorithm.
                --
                -- Note that this last extreme case is reached when using an
                -- unstable fee policy (where values of outputs can influence
                -- the policy) AND, require transactions to be 100% balanced.
                -- This is a silly thing to do.
                | otherwise ->
                    Nothing
      where
        applyDiff :: Integer -> Coin -> Coin
        applyDiff i c
            = fromMaybe C.zero
            $ coinFromIntegral (i + coinToIntegral c)

        diff :: Integer
        diff = actualFee - requiredFee
          where
            requiredFee
                = coinToIntegral $ unFee
                $ estimateFee feeEstimator coinSel

        actualFee :: Integer
        actualFee
            = coinToIntegral (sumInputs coinSel)
            - coinToIntegral (sumChange coinSel)

    -- | Apply the given function to the first coin of the list. If the
    -- operation makes the 'Coin' smaller than the dust threshold, the coin is
    -- discarded.
    modifyFirst :: NonEmpty Coin -> (Coin -> Coin) -> [Coin]
    modifyFirst (c :| cs) op
        | c' <= threshold = cs
        | otherwise = c' : cs
      where
        c' = op c
        threshold = unDustThreshold dustThreshold

    getNextBatch :: State [a] [a]
    getNextBatch = do
        xs <- get
        let (batch, rest) = splitAt (fromIntegral batchSize) xs
        put rest
        pure batch

-- | An upper limit for the number of 'inputs' to include in each coin selection
--   generated by 'selectCoins'.
--
-- @since 1.0.0
newtype BatchSize = BatchSize Word16
    deriving (Eq, Generic, Ord, Show)

-- | Calculate an ideal batch size based on the given coin selection limit.
--
-- @since 1.0.0
idealBatchSize :: CoinSelectionLimit -> BatchSize
idealBatchSize coinselOpts = BatchSize $ fixPoint 1
  where
    fixPoint :: Word16 -> Word16
    fixPoint !n
        | maxN n <= n = n
        | n == maxBound = n
        | otherwise = fixPoint (n + 1)
      where
        maxN :: Word16 -> Word16
        maxN = calculateLimit coinselOpts
