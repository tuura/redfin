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

import           Control.Monad.IO.Class     (liftIO)
import           Data.SBV                   hiding (label)
import           Prelude                    hiding (abs, div, read)
import           Redfin
import           Redfin.Assembly
import qualified Redfin.Assembly            as Assembly
import           Redfin.Examples.Common
import           Redfin.Language.Expression
import           Redfin.Listing
import           Redfin.Simulate
import           Text.Pretty.Simple         (pPrint)

-- -- | Example program
-- motorControl :: Script
-- motorControl = do -- Don't forget the 'do'!
--     -- Declare named memory locations if necessary
--     let { a_max = 0; v_max = 1; dist = 2; s = 3; v = 4; s_decel = 5;
--           decel_steps = 6; temp = 7; v_next = 8; }

--     -- compute s_decel
--     "start" @@ ld r0 v
--     div r0 a_max
--     st r0 decel_steps                    -- decel_steps = v / a_max
--     add_si r0 1                          -- decel_steps + 1
--     st r0 temp
--     ld r0 a_max
--     mul r0 decel_steps                   -- a_max * decel_steps
--     mul r0 temp                          -- a_max * (decel_steps + 1)
--     sra_i r0 1                           -- s_decel = a_max * (decel_steps + 1) / 2

--     ld r1 decel_steps
--     mul r1 a_max
--     cmpeq r1 v                           -- a_max * decel_steps == v ?
--     goto_ct "store_s_decel"
--     add r0 v                             -- s_decel += v
--     "store_s_decel" @@ st r0 s_decel     -- store s_decel

--     -- compute v_next
--     ld r0 v
--     add r0 a_max                        -- v_next = v + a_max
--     cmplt r0 dist                       -- v_next < dist ?
--     goto_ct "keep_v_next1"
--     ld r0 dist                          -- overwrite v_next with dist
--     "keep_v_next1" @@ cmplt r0 v_max    -- v_next < v_max ?
--     goto_ct "keep_v_next2"
--     ld r0 v_max                         -- overwrite v_next with v_max
--     "keep_v_next2" @@ st r0 v_next      -- store v_next value

--     -- set speed according to final distance
--     add r0 s_decel
--     add r0 s
--     cmpgt r0 dist                       -- s + s_decel + v_next > dist ?
--     goto_ct "keep_speed"
--     ld r1 v_next                        -- accelerate
--     goto "set_v"
--     "keep_speed" @@ ld r0 s
--     add r0 s_decel
--     add r0 v
--     cmpgt r0 dist                       -- s + s_decel + v > dist ?
--     goto_ct "decelerate"
--     ld r1 v                             -- keep speed of v
--     goto "set_v"
--     "decelerate" @@ ld r0 v
--     ld r1 decel_steps
--     mul r1 a_max                       -- decel_steps * a_max
--     st r1 temp
--     cmpgt r0 temp                      -- v > n * a_max ?
--     goto_ct "set_v"
--     ld r1 v
--     sub r1 a_max                       -- v - a_max
--     "set_v" @@ st r1 v                 -- store decreased value of v

--     -- speed check
--     ld_i r0 0
--     st r0 temp
--     cmpeq r1 temp                     -- v == 0?
--     goto_cf "inc_s"                   -- speed is non-zero: continue

--     -- in case speed is 0, check if full distance has been covered:
--     ld r0 s
--     cmpeq r0 dist
--     goto_cf "reaccelerate"
--     halt                              -- we have reached our destination
--     "reaccelerate" @@ ld r0 dist
--     sub r0 s                          -- dist - s
--     cmplt r0 a_max
--     goto_ct "set_v2"
--     ld r0 a_max
--     "set_v2" @@ st r0 v

--     "inc_s" @@ ld r0 s                -- s += v
--     add r0 v
--     st r0 s
--     goto "start"


-- -- | SBV treats simulation as trivial symbolic execution, hence to simulate a
-- --   program, we will use the same constructs as with proving, but the memory
-- --   will be filled with constnats instead of symbolic variables.
-- --
-- --  To execute simulation, type into ghci:
-- --  proveWith prover simulateMotorControl
-- simulateMotorControl :: Symbolic SBool
-- simulateMotorControl = do
--     -- To simulate a program with concrete values we need to init the memory
--     -- with literal constants instead of symbolic variables, like that:
--     let mem = initialiseMemory [ (0, 2)   -- a_max = 2
--                                , (1, 30)  -- v_max = 30
--                                , (2, 101) -- dist  = 101
--                                ] -- all uninitialised memory location contain 0
--         -- now we prepare an initil state
--         initialState = boot motorControl mem
--         -- and obtain the final state by running the simulation
--         -- for at most 1000 steps
--         finalState = simulate 10000 initialState

--     -- Extract the registers, relevant parts of the memory and the flags and
--     -- print all that
--     liftIO . putStrLn $ "------------------------"
--     liftIO . putStrLn $ "Registers: "
--     liftIO . mapM_ print $ dumpRegisters $ registers $ finalState
--     liftIO . putStrLn $ "------------------------"

--     liftIO . putStrLn $ "Flags: "
--     liftIO . mapM_ print $ dumpFlags     $ flags $ finalState
--     liftIO . putStrLn $ "------------------------"

--     liftIO . putStrLn $ "Memory: "
--     liftIO . mapM_ print $ dumpMemory [3,4] $ memory $ finalState
--     liftIO . putStrLn $ "------------------------"

--     return true
