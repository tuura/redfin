-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Examples.Add
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2018
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- A tiny example. Adds two numbers in Haskell and Assembly and
-- checks the two programs to be equivalent.
--
-----------------------------------------------------------------------------
module Redfin.Examples.Add where

import           Data.SBV                     hiding (( # ), (%))
import           Prelude                      hiding (read)
import           Redfin.Assembly              hiding (abs, div)
import qualified Redfin.Assembly              as Assembly
import           Redfin.Data.Fixed
import           Redfin.Examples.Common
import           Redfin.Examples.Energy.Units
import           Redfin.Language.Expression
import           Redfin.Listing
import           Redfin.Simulate
import           Redfin.Types
import           Text.Pretty.Simple           (pPrint)

addHaskell :: Integral a => a -> a -> a
addHaskell x y = x + y

addHighLevel :: Script
addHighLevel = do
    let x     = read $ IntegerVariable 0
        y     = read $ IntegerVariable 1
        temp  = Temporary 3
        stack = Stack 5
    compile r0 stack temp (addHaskell x y)
    halt

addLowLevel :: Script
addLowLevel = do
    let { x = 0; y = 1 }
    ld r0 x
    add r0 y
    halt

equivalence :: Symbolic SBool
equivalence = do
    x <- forall "t1"
    y <- forall "t2"
    -- constrain $ t1 .>= 0 &&& t1 .<= toMilliSeconds (30 % Year)
    -- constrain $ t2 .>= 0 &&& t2 .<= toMilliSeconds (30 % Year)
    -- constrain $ p1 .>= 0 &&& p1 .<= toMilliWatts (1 % Watt)
    -- constrain $ p2 .>= 0 &&& p2 .<= toMilliWatts (1 % Watt)
    emptyRegs <- mkRegisters "registers" []
    emptyFlags <- mkFlags "flags" []
    progLL <- assemble addLowLevel
    progHL <- assemble addHighLevel
    mem <- mkMemory "memory" [(0, x), (1, y), (3, 100)]
    let steps = 100
        finalStateLL = simulate steps $ boot progLL emptyRegs mem emptyFlags
        finalStateHL = simulate steps $ boot progHL emptyRegs mem emptyFlags
        resultLL = readArray (registers finalStateLL) 0
        resultHL = readArray (registers finalStateHL) 0
    pure $ resultLL .== resultHL
