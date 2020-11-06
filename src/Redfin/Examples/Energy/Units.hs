{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Examples.Energy.Units
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2018
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- Units of measurment for REDFIN programs.
--
-- Currently this module defined dimensions and units of measurement relevant
-- to 'Redfin.Examples.Energy'.
-----------------------------------------------------------------------------
module Redfin.Examples.Energy.Units (
        -- * Re-export units modules
        -- * This module exports unit definitions according to the SI system of units
        --   (Meters, Grams, Seconds etc.)
        module Data.Units.SI,
        -- * Defines prefixes from the SI standard (milli, micro etc.)
        module Data.Units.SI.Prefixes,
        -- * This module exports definitions for the SI system and related combinators
        --   such as (%) and (#).
        module Data.Metrology.Poly,

        -- * These units, dimensions and ad-hoc coercions are used in 'Redfin.Examples.Energy' example.
        --   TODO: re-export facilities for user-defined units and dimensions.
        Year (..), Time, Power, Energy,
        toMilliSeconds, toMilliWatts, absU
    ) where

import           Data.Metrology.Poly
import           Data.Metrology.Show
import           Data.Metrology.SI           ()
import qualified Data.Metrology.SI.PolyTypes as SI
import           Data.Metrology.Unsafe
import           Data.Units.SI
import           Data.Units.SI.Prefixes
import           Redfin.Data.Fixed
import           Redfin.Types

absU :: Num n => Qu d1 l n -> Qu d1 l n
absU (Qu a) = Qu (abs a)

data Year = Year
instance Unit Year where
    type BaseUnit Year = Second
    conversionRatio _ = 60 * 60 * 24 * 366

type Time = SI.Time 'DefaultLCSU Fixed

type Power = SI.Power 'DefaultLCSU Fixed

type Energy = Time %* Power

toMilliSeconds :: Time -> Value
toMilliSeconds t = unsafeFixedToValue (t # milli Second)

toMilliWatts :: Power -> Value
toMilliWatts p = unsafeFixedToValue (p # milli Watt)
