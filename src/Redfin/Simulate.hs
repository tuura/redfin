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
module Redfin.Simulate (simulate, simulateWithTrace) where

import           Control.Monad.IO.Class
import           Data.SBV               hiding (SFunArray, SymArray (..))
import           Data.SBV.Control
import           Redfin.SBV

import qualified Debug.Trace            as Debugger

import           Redfin
import           Redfin.Decode
import           Redfin.Listing
import           Redfin.Types

-- | Symbolically simulate REDFIN for a number of 'steps'
--   starting from the 'state'.
simulate :: Int -> State -> State
simulate = simulateWithTrace Nothing

-- | Simulate REDFIN with debug output at each step
simulateWithTrace :: Maybe (State -> String) -> Int -> State -> State
simulateWithTrace tracer steps state =
  let tracer' = maybe id (\f -> Debugger.trace (f state)) tracer
  in tracer' $ if steps <= 0 then state
               else
                 let halted = readArray (flags state) (flagId Halt)
                 in ite halted
                        state
                        (simulateWithTrace tracer (steps - 1) nextState)
  where
    nextState = snd $ transform executeInstruction state
