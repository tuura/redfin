{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Redfin.Examples.Energy (
  energyEstimate, energyEstimateHighLevel, energyEstimateLowLevel,
  faultyExample, correct, equivHaskell, equivalence
  ) where

import           Control.Monad.IO.Class       (liftIO)
import           Data.SBV                     hiding (( # ), (%))
import           Prelude                      hiding (read)
import           Redfin
import           Redfin.Assembly              hiding (abs, div)
import qualified Redfin.Assembly              as Assembly
import           Redfin.Data.Fixed
import           Redfin.Examples.Common
import           Redfin.Examples.Energy.Units
import           Redfin.Language.Expression
import           Redfin.Listing
import           Redfin.Simulate
import           Text.Pretty.Simple           (pPrint)

energyEstimate :: Integral a => a -> a -> a -> a -> a
energyEstimate t1 t2 p1 p2 = abs (t1 - t2) * (p1 + p2) `div` 2

energyEstimate' :: Fractional a => a -> a -> a -> a -> a
energyEstimate' t1 t2 p1 p2 = abs (t1 - t2) * (p1 + p2) / 2

energyEstimateHighLevel :: Script
energyEstimateHighLevel = do
    let t1    = read $ IntegerVariable 0
        t2    = read $ IntegerVariable 1
        p1    = read $ IntegerVariable 2
        p2    = read $ IntegerVariable 3
        temp  = Temporary 4
        stack = Stack 5
    compile r0 stack temp (energyEstimate t1 t2 p1 p2)
    halt

energyEstimateHighLevelFP :: Script
energyEstimateHighLevelFP = do
    let t1    = read $ FixedPointVariable 0
        t2    = read $ FixedPointVariable 1
        p1    = read $ FixedPointVariable 2
        p2    = read $ FixedPointVariable 3
        temp  = Temporary 4
        stack = Stack 5
    compile r0 stack temp (energyEstimate' t1 t2 p1 p2)
    halt

energyEstimateLowLevel :: Script
energyEstimateLowLevel = do
    let { t1 = 0; t2 = 1; p1 = 2; p2 = 3 }
    ld r0 t1
    sub r0 t2
    Assembly.abs r0
    ld r1 p1
    add r1 p2
    st r1 p2
    mul r0 p2
    sra_i r0 1
    halt

equivalence :: Symbolic SBool
equivalence = do
    t1 <- forall "t1"
    t2 <- forall "t2"
    p1 <- forall "p1"
    p2 <- forall "p2"
    constrain $ t1 .>= 0 .&& t1 .<= toMilliSeconds (30 % Year)
    constrain $ t2 .>= 0 .&& t2 .<= toMilliSeconds (30 % Year)
    constrain $ p1 .>= 0 .&& p1 .<= toMilliWatts (1 % Watt)
    constrain $ p2 .>= 0 .&& p2 .<= toMilliWatts (1 % Watt)
    mem <- mkMemory "memory" [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
    let steps = 100
    emptyRegs <- mkRegisters "registers" []
    emptyFlags <- mkFlags "flags" []
    progLowLevel <- assemble energyEstimateLowLevel
    progHighLevel <- assemble energyEstimateHighLevel
    let finalStateLL = simulate steps $ boot progLowLevel emptyRegs mem emptyFlags
        finalStateHL = simulate steps $ boot progHighLevel emptyRegs mem emptyFlags
    let resultLL = readArray (registers finalStateLL) 0
        resultHL = readArray (registers finalStateHL) 0
    pure $ resultLL .== resultLL

equivHaskell :: Script -> Symbolic SBool
equivHaskell src = do
    t1 <- forall "t1"
    t2 <- forall "t2"
    p1 <- forall "p1"
    p2 <- forall "p2"
    constrain $ t1 .>= 0 .&& t1 .<= toMilliSeconds (30 % Year)
    constrain $ t2 .>= 0 .&& t2 .<= toMilliSeconds (30 % Year)
    constrain $ p1 .>= 0 .&& p1 .<= toMilliWatts (1 % Watt)
    constrain $ p2 .>= 0 .&& p2 .<= toMilliWatts (1 % Watt)
    mem <- mkMemory "memory" [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
    let steps = 100
    emptyRegs <- mkRegisters "registers" []
    emptyFlags <- mkFlags "flags" []
    prog <- assemble src
    let finalState = simulate steps $ boot prog emptyRegs mem emptyFlags
        result = readArray (registers finalState) 0
    pure $ result .== energyEstimate t1 t2 p1 p2

faultyExample :: Script -> Symbolic SBool
faultyExample src = do
    t1 <- forall "t1"
    t2 <- forall "t2"
    p1 <- forall "p1"
    p2 <- forall "p2"
    constrain $ p1 .>= 0
    constrain $ p2 .>= 0

    mem <- mkMemory "memory" [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]

    let steps = 100
    emptyRegs <- mkRegisters "registers" []
    emptyFlags <- mkFlags "flags" []
    prog <- assemble src
    let finalState = simulate steps $ boot prog emptyRegs mem emptyFlags
        result = readArray (registers finalState) 0
        overflow = readArray (flags finalState) (flagId Overflow)
    pure $   sNot overflow
         .&& result .>= 0

correct :: Script -> Symbolic SBool
correct src = do
    t1 <- forall "t1"
    t2 <- forall "t2"
    p1 <- forall "p1"
    p2 <- forall "p2"
    constrain $ t1 .>= 0 .&& t1 .<= toMilliSeconds (30 % Year)
    constrain $ t2 .>= 0 .&& t2 .<= toMilliSeconds (30 % Year)
    constrain $ p1 .>= 0 .&& p1 .<= toMilliWatts (1 % Watt)
    constrain $ p2 .>= 0 .&& p2 .<= toMilliWatts (1 % Watt)
    mem <- mkMemory "memory" [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
    prog <- assemble src
    emptyRegs <- mkRegisters "registers" []
    emptyFlags <- mkFlags "flags" []
    let steps = 100
        finalState = simulate steps $ boot prog emptyRegs mem emptyFlags
        result = readArray (registers finalState) 0
        overflow = readArray (flags finalState) (flagId Overflow)
    pure $   sNot overflow
         .&& result .>= 0

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

-- An alternative to defining these orphan instances is to switch to SBV's type
-- class SDivisible instead. Even better is to fix Haskell's class hierarchy.
instance Integral (SBV Int64) where
    div       = sDiv
    quotRem   = error "quotRem is not implemented for SBV Int64"
    toInteger = error "quotRem cannot be implemented for SBV Int64"

instance Ord (SBV Int64) where
    compare = error "Ord cannot be implemented for SBV Int64"

instance Real (SBV Int64) where
    toRational = error "Real cannot be implemented for SBV Int64"
