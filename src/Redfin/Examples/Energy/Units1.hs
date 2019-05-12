{-# LANGUAGE DataKinds, TypeOperators #-}
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
module Redfin.Examples.Energy.Units1 where

import Data.Metrology.Poly
import Data.Metrology.SI ()  -- DefaultLCSU instances
import qualified Data.Metrology.SI.PolyTypes as SI
import Data.Units.SI
import Data.Units.SI.Prefixes
import Data.Metrology.Show
import Data.Metrology.Unsafe
import Redfin.Data.Fixed
import Redfin

import Data.SBV hiding ((%), (#))

absU :: Num n => Qu d1 l n -> Qu d1 l n
absU (Qu a) = Qu (abs a)

data Year = Year
instance Unit Year where
    type BaseUnit Year = Second
    conversionRatio _ = 60 * 60 * 24 * 366

type Time = SI.Time 'DefaultLCSU SDouble

type Power = SI.Power 'DefaultLCSU SDouble

type Energy = Time %* Power

es :: Time -> Time -> Power -> Power -> Energy
es t1 t2 p1 p2 = ((absU (t1 |-| t2)) |*| (p1 |+| p2)) |/| 2

-- x :: Energy
-- x = (es (10 % Second) (0 % Second) (10 % Watt) (30 % Watt))

forgetUnits :: Energy -> SDouble
forgetUnits x = x # (milli Second :* milli Watt)

toMilliSeconds :: Time -> Value
toMilliSeconds t = fromSDouble sRoundTowardZero (t # milli Second)

toMilliWatts :: Power -> Value
toMilliWatts p = fromSDouble sRoundTowardZero (p # milli Watt)

-- toMilliSeconds :: Time -> SDouble
-- toMilliSeconds t = (t # milli Second)

-- toMilliWatts :: Power -> SDouble
-- toMilliWatts p = (p # milli Watt)