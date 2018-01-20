-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Verify
-- Copyright   :  (c) Andrey Mokhov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- Simulating REDFIN programs.
--
-----------------------------------------------------------------------------
module Redfin.Verify (verify) where

import Data.SBV

import Redfin
import Redfin.Decode

verify :: Int -> State -> State
verify steps state
    | steps == 0 = state
    | otherwise  = ite halted state (verify (steps - 1) nextState)
  where
    halted    = readArray (flags state) (flagId Halt)
    nextState = snd $ redfin executeInstruction state
