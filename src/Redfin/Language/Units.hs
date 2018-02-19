{-# LANGUAGE DataKinds, StandaloneDeriving, TypeOperators #-}

module Redfin.Language.Units (
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
        toMilliSeconds, toMilliWatts
    ) where

import Data.Metrology.Poly
import Data.Metrology.SI ()  -- DefaultLCSU instances
import qualified Data.Metrology.SI.PolyTypes as SI
import Data.Units.SI
import Data.Units.SI.Prefixes
import Data.Metrology.Show
import Data.Metrology.Unsafe
import Redfin.Data.Fixed
import Redfin

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