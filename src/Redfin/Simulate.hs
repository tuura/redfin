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
module Redfin.Simulate (
    TerminationCondition, haltIsRaised, simulateUntil, simulate
    ) where

import qualified Data.Map.Strict as Map

import Redfin
import Redfin.Decoder

-- | A predicate on Redfin states to determine when to terminate the simulation.
type TerminationCondition = State -> Bool

-- | The 'haltIsRaised' predicate terminates the simulation as soon as the
-- 'Halt' flag is raised.
haltIsRaised :: TerminationCondition
haltIsRaised state = Map.lookup Halt (flags state) == Just True

-- | Simulate Redfin from a given initial state until the termination condition
-- is satisfied. The final state is returned.
simulateUntil :: TerminationCondition -> State -> State
simulateUntil stop initalState = go initalState
  where
    go state | stop state = state
             | otherwise  = go $ next state
    next = snd . redfin executeInstruction

-- | Simulate Redfin from a given initial state using the 'haltIsRaised' as the
-- termination condition.
simulate :: State -> State
simulate = simulateUntil haltIsRaised
