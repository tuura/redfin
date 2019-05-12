{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Redfin.Examples.GCD where

import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (div, mod, read)
import Text.Pretty.Simple (pPrint)
import Data.SBV hiding ((%), (#))
import Redfin
import Redfin.Assembly hiding (abs)
import Redfin.Listing
import qualified Redfin.Assembly as Assembly
import Redfin.Simulate
import Redfin.Language.Expression
import Redfin.Examples.Common

-- gcdHighLevel :: Script
-- gcdHighLevel = do
--     let x    = read $ IntegerVariable 0
--         y    = read $ IntegerVariable 1
--         temp  = Temporary 4
--         stack = Stack 5
--     compile r0 stack temp (gcd x y)
--     halt

mod :: Register -> MemoryAddress -> Temporary -> Temporary-> Script
mod reg addr (Temporary tmp1) (Temporary tmp2) = do
    st reg tmp1
    div reg addr
    mul reg addr
    st  reg tmp2
    ld reg tmp1
    sub reg tmp2

gcdLowLevel :: Script
gcdLowLevel = do
    -- # Find the greatest common divisor of values in memory locations 0 and 1,
    -- # put result to the register R1
    -- ld_i r0 0
    -- st r0 255
    ld r0 1
    -- # Test register r0 for being zero by subtracting zero
    cmpeq r0 255
    -- -- # Halt if register r0 contains zero, loop otherwise
    jmpi_ct 6
    -- ld r0 0
    -- -- mod r0 1 (Temporary 254) (Temporary 253)
    -- ld r1 1
    -- st r0 1
    -- st r1 0
    -- jmpi (-8)
    -- halt

-- equivalence :: Symbolic SBool
-- equivalence = do
--     t1 <- forall "t1"
--     t2 <- forall "t2"
--     p1 <- forall "p1"
--     p2 <- forall "p2"
--     constrain $ t1 .>= 0 &&& t1 .<= toMilliSeconds (30 % Year)
--     constrain $ t2 .>= 0 &&& t2 .<= toMilliSeconds (30 % Year)
--     constrain $ p1 .>= 0 &&& p1 .<= toMilliWatts (1 % Watt)
--     constrain $ p2 .>= 0 &&& p2 .<= toMilliWatts (1 % Watt)
--     let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
--         steps = 100
--         finalStateLL = simulate steps $ boot energyEstimateLowLevel mem
--         finalStateHL = simulate steps $ boot energyEstimateHighLevel mem
--         resultLL = readArray (registers finalStateLL) 0
--         resultHL = readArray (registers finalStateHL) 0
--     pure $ resultLL .== resultHL

-- equivHaskell :: Script -> Symbolic SBool
-- equivHaskell src = do
--     t1 <- forall "t1"
--     t2 <- forall "t2"
--     p1 <- forall "p1"
--     p2 <- forall "p2"
--     constrain $ t1 .>= 0 &&& t1 .<= toMilliSeconds (30 % Year)
--     constrain $ t2 .>= 0 &&& t2 .<= toMilliSeconds (30 % Year)
--     constrain $ p1 .>= 0 &&& p1 .<= toMilliWatts (1 % Watt)
--     constrain $ p2 .>= 0 &&& p2 .<= toMilliWatts (1 % Watt)
--     let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
--         steps = 100
--         finalState = simulate steps $ boot src mem
--         result = readArray (registers finalState) 0
--     pure $ result .== energyEstimate t1 t2 p1 p2

faultyExample :: Script -> Symbolic SBool
faultyExample src = do
    x <- forall "x"
    y <- forall "y"
    -- constrain $ x .>= 0
    -- constrain $ y .>= 0
    let mem = initialiseMemory [(0, x), (1, y), (255, 0)]
        steps = 100
        finalState = simulate steps $ boot src mem
        -- result     = readArray (registers finalState) 0
        overflow   = readArray (flags finalState) (flagId Overflow)
    pure $   bnot overflow
        --  &&& result .>= 0

-- correct :: Script -> Symbolic SBool
-- correct src = do
--     t1 <- forall "t1"
--     t2 <- forall "t2"
--     p1 <- forall "p1"
--     p2 <- forall "p2"
--     constrain $ t1 .>= 0 &&& t1 .<= toMilliSeconds (30 % Year)
--     constrain $ t2 .>= 0 &&& t2 .<= toMilliSeconds (30 % Year)
--     constrain $ p1 .>= 0 &&& p1 .<= toMilliWatts (1 % Watt)
--     constrain $ p2 .>= 0 &&& p2 .<= toMilliWatts (1 % Watt)
--     let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
--         steps = 100
--         finalState = simulate steps $ boot src mem
--         result = readArray (registers finalState) 0
--         overflow = readArray (flags finalState) (flagId Overflow)
--     pure $   bnot overflow
--          &&& result .>= 0

-- lowLevelCorrect :: Symbolic SBool
-- lowLevelCorrect = do
--     t1 <- forall "t1"
--     t2 <- forall "t2"
--     p1 <- forall "p1"
--     p2 <- forall "p2"
--     constrain $ t1 .>= 0 &&& t1 .<= toMilliSeconds (30 % Year)
--     constrain $ t2 .>= 0 &&& t2 .<= toMilliSeconds (30 % Year)
--     constrain $ p1 .>= 0 &&& p1 .<= toMilliWatts (1 % Watt)
--     constrain $ p2 .>= 0 &&& p2 .<= toMilliWatts (1 % Watt)
--     let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
--         steps = 100
--         finalState = simulate steps $ boot energyEstimateLowLevel mem
--         result = readArray (registers finalState) 0
--         overflow = readArray (flags finalState) (flagId Overflow)
--     pure $   bnot overflow
--          &&& result .>= 0

-- highLevelCorrectFP :: Symbolic SBool
-- highLevelCorrectFP = do
--     t1 <- forall "t1"
--     t2 <- forall "t2"
--     p1 <- forall "p1"
--     p2 <- forall "p2"
--     constrain $ t1 .>= 0 &&& t1 .<= toMilliSeconds (30 % Year)
--     constrain $ t2 .>= 0 &&& t2 .<= toMilliSeconds (30 % Year)
--     constrain $ p1 .>= 0 &&& p1 .<= toMilliWatts (1 % Watt)
--     constrain $ p2 .>= 0 &&& p2 .<= toMilliWatts (1 % Watt)
--     let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
--         steps = 100
--         finalState = simulate steps $ boot energyEstimateHighLevel mem
--         result = readArray (registers finalState) 0
--         overflow = readArray (flags finalState) (flagId Overflow)
--     pure $
--         -- bnot overflow &&&
--         (Fixed $ result) .>= 0

-- simulateHighLevel :: IO ()
-- simulateHighLevel = do
--     let mem = initialiseMemory [ (0, 10)
--                                , (1, 5)
--                                , (2, 3)
--                                , (3, 5), (5, 100)]
--         finalState = simulate 100 $ boot energyEstimateHighLevel mem
--         memoryDump = dumpMemory 0 255 $ memory finalState
--     putStr "Memory Dump: "
--     pPrint memoryDump
--     putStrLn $ "Estimated energy: " ++ show (readArray (registers finalState) 0)
--     putStrLn $ "Clock: " ++ show (clock finalState)

-- simulateHighLevelFP :: IO ()
-- simulateHighLevelFP = do
--     let mem = initialiseMemory [ (0, (getFixed . toFixed) 10)
--                                , (1, (getFixed . toFixed) 5)
--                                , (2, (getFixed . toFixed) 3)
--                                , (3, (getFixed . toFixed) 5), (5, 100)]
--         finalState = simulate 100 $ boot energyEstimateHighLevelFP mem
--         memoryDump = dumpMemory 0 255 $ memory finalState
--     putStr "Memory Dump: "
--     pPrint memoryDump
--     putStrLn $ "Estimated energy: " ++ show (Fixed $ readArray (registers finalState) 0)
--     putStrLn $ "Clock: " ++ show (clock finalState)

-- -- An alternative to defining these orphan instances is to switch to SBV's type
-- -- class SDivisible instead. Even better is to fix Haskell's class hierarchy.
-- instance Integral (SBV Int64) where
--     div       = sDiv
--     quotRem   = error "quotRem is not implemented for SBV Int64"
--     toInteger = error "quotRem cannot be implemented for SBV Int64"

-- instance Ord (SBV Int64) where
--     compare = error "Ord cannot be implemented for SBV Int64"

-- instance Real (SBV Int64) where
--     toRational = error "Real cannot be implemented for SBV Int64"
