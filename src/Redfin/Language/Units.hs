{-# LANGUAGE DataKinds, StandaloneDeriving, TypeOperators #-}

module Redfin.Language.Units (Year (..), Time, Power, Energy
                             , module Data.Units.SI
                             , module Data.Units.SI.Prefixes
                             , module Data.Metrology.Poly ) where

import Data.Metrology.Poly
import Data.Metrology.SI   ()  -- DefaultLCSU instances
import qualified Data.Metrology.SI.PolyTypes as SI
import Data.Units.SI
import Data.Units.SI.Prefixes
import Data.Metrology.Show
import Data.Metrology.Unsafe
import Redfin.Data.Fixed

data Year = Year
instance Unit Year where
  type BaseUnit Year = Second
  conversionRatio _ = 60 * 60 * 24 * 365.242

type Time = SI.Time 'DefaultLCSU Fixed

type Power = SI.Power 'DefaultLCSU Fixed

type Energy = Time %* Power

t :: Time
t = 30 % Year

t' :: Fixed
t' = ((1 % Watt :: Power) # milli Watt)