-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Simulate
-- Copyright   :  (c) Andrey Mokhov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- Simulating REDFIN programs.
--
-----------------------------------------------------------------------------
module Redfin.Simulate (simulate) where

import Data.SBV

import Redfin
import Redfin.Decode

simulate :: Int -> State -> State
simulate steps state
    | steps <= 0 = state
    | otherwise  = ite halted state (simulate (steps - 1) nextState)
  where
    halted    = readArray (flags state) (flagId Halt)
    nextState = snd $ redfin executeInstruction state
