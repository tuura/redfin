-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Simulate
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2017-2020
--
-- Maintainer  :  mail@geo2a.info
-- Stability   :  experimental
--
-- Simulating REDFIN programs.
--
-----------------------------------------------------------------------------
module Redfin.Simulate (simulate) where

import           Data.SBV

import           Redfin.Decode
import           Redfin.Types

simulate :: Int -> State -> State
simulate steps state
    | steps <= 0 = state
    | otherwise  = ite halted state (simulate (steps - 1) nextState)
  where
    halted    = readArray (flags state) (flagId Halt)
    nextState = snd $ redfin executeInstruction state
