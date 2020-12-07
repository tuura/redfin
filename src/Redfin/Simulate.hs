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
--
--   Note that currently the program will be executed for as
--   much steps as we say, even if it terminates earlier, in
--   which case the later instructions will all be 'halt' and
--   the state won't change.
simulate :: Int -> State -> State
simulate = simulateWithTrace Nothing

-- | Simulate REDFIN with debug output at each step
simulateWithTrace :: Maybe (State -> String) -> Int -> State -> State
simulateWithTrace tracer steps state =
  let tracer' = maybe id (\f -> Debugger.trace (f state)) tracer
  in tracer' $ if steps <= 0 then state
               else simulate (steps - 1) nextState
  where
    nextState = snd $ transform executeInstruction state
