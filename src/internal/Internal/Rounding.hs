{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: © 2018-2024 Intersect MBO
-- License: Apache-2.0
--
-- Provides internal types and functions relating to rounding of fractional
-- numbers.
--
module Internal.Rounding
    ( RoundingDirection (..)
    , round
    ) where

import Prelude hiding
    ( round )

-- | Indicates a rounding direction to be used when converting from a
--   fractional value to an integral value.
--
-- See 'round'.
--
data RoundingDirection
    = RoundUp
      -- ^ Round up to the nearest integral value.
    | RoundDown
      -- ^ Round down to the nearest integral value.
    deriving (Eq, Show)

-- | Use the given rounding direction to round the given fractional value,
--   producing an integral result.
--
round :: (RealFrac a, Integral b) => RoundingDirection -> a -> b
round = \case
    RoundUp -> ceiling
    RoundDown -> floor
