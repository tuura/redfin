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

import           Control.Monad.IO.Class       (liftIO)
import           Data.SBV                     hiding (SFunArray, SymArray (..),
                                               ( # ), (%))
import           Data.SBV.Internals           (Result (..), SBVRunMode (..),
                                               runSymbolic)
import           Prelude                      hiding (read)
import           Redfin.SBV
import           Text.Pretty.Simple           (pPrint)

import           Redfin
import           Redfin.Assembly              hiding (abs, div)
import qualified Redfin.Assembly              as Assembly
import           Redfin.Data.Fixed
import           Redfin.Examples.Common
import           Redfin.Examples.Energy.Units
import           Redfin.Language.Expression
import           Redfin.Listing
import           Redfin.Simulate
import           Redfin.Types

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

noOverflow :: Script -> Symbolic SBool
noOverflow src = do
    x <- forall "x"
    y <- forall "y"
    constrain $ x .>= 0 .&& x .<= 1000
    constrain $ y .>= 0 .&& y .<= 1000
    let prog = assemble src
        steps = 3
    let mem = mkMemory [(0, x), (1, y)]
    let initialState = boot prog defaultRegisters mem defaultFlags
        finalState = simulate steps initialState
    let halted = readArray (flags finalState) (flagId Halt)
        overflow = readArray (flags finalState) (flagId Overflow)
    pure $ halted .&& sNot overflow

equivalence :: Symbolic SBool
equivalence = do
    x <- forall "x"
    y <- forall "y"
    let progLL = assemble addLowLevel
    let progHL = assemble addHighLevel
    let mem = mkMemory [(0, x), (1, y), (3, 100)]
    let steps = 100
    let initialStateLL = boot progLL defaultRegisters mem defaultFlags
        initialStateHL = boot progHL defaultRegisters mem defaultFlags
    let finalStateLL = simulate steps initialStateLL
        finalStateHL = simulate steps initialStateHL
    let resultLL = readArray (registers finalStateLL) 0
        resultHL = readArray (registers finalStateHL) 0
    pure $ resultLL .== resultHL
