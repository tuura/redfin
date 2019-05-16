-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Examples.MotorControl
-- Copyright   :  (c) Jakob Lechner, Georgy Lukyanov 2019
--
-- Maintainer  :  g.lukyanov2@ncl.ac.uk
-- Stability   :  experimental
--
-- Stepper motor control example program for REDFIN.
--
-----------------------------------------------------------------------------
module Redfin.Examples.MotorControl where

import           Prelude hiding (read)
import           Control.Monad.IO.Class (liftIO)
import           Text.Pretty.Simple (pPrint)
import           Data.SBV hiding (label)
import           Redfin
import           Redfin.Assembly hiding (div, abs)
import qualified Redfin.Assembly as Assembly
import           Redfin.Listing
import           Redfin.Simulate
import           Redfin.Language.Expression
import           Redfin.Examples.Common

-- | Example program
motorControl :: Script
motorControl = do -- Don't forget the 'do'!
    -- Declare named memory locations if necessary
    let { x = 0; y = 1}
    -- the four registers must be referred as r0, r1, r2, r3, like in the
    -- following stub command:
    add r0 x -- add the value from memory location 0, which is referred by 'x',
             -- to the value stored in the register r0
    "end" @@ halt -- labels are declared with the @@ operator preceeded by the
                  -- label's name in quotes and followed by the command that
                  -- would be executed if a jump was made to the labeled
                  -- location in the program

-- | SBV treats simulation as trivial symbolic execution, hence to simulate a
--   program, we will use the same constructs as with proving, but the memory
--   will be filled with constnats instead of symbolic variables.
--
--  To execute simulation, type into ghci:
--  proveWith prover simulateMotorControl
simulateMotorControl :: Symbolic SBool
simulateMotorControl = do
    -- To simulate a program with concrete values we need to init the memory
    -- with literal constants instead of symbolic variables, like that:
    let x = 42
        mem = initialiseMemory [ (0, x)  -- put 'x' into the memory location 0
                               , (1, 10) -- put 10 into the memory location 1
                               ] -- all uninitialised memory location contain 0
        -- now we prepare an initil state
        initialState = boot motorControl mem
        -- and obtain the final state by running the simulation
        -- for at most 1000 steps
        finalState = simulate 1000 initialState

    -- Extract the registers, relevant parts of the memory and the flags and
    -- print all that
    liftIO . putStrLn $ "------------------------"
    liftIO . putStrLn $ "Registers: "
    liftIO . mapM_ print $ dumpRegisters $ registers $ finalState
    liftIO . putStrLn $ "------------------------"

    liftIO . putStrLn $ "Flags: "
    liftIO . mapM_ print $ dumpFlags     $ flags $ finalState
    liftIO . putStrLn $ "------------------------"

    liftIO . putStrLn $ "Memory: "
    liftIO . mapM_ print $ dumpMemory [1,2,3] $ memory $ finalState
    liftIO . putStrLn $ "------------------------"

    return true