{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Redfin.Examples.Energy (
        energyEstimate, energyEstimateHighLevel, energyEstimateLowLevel,
        equivalence, highLevelFaultyExample, highLevelCorrect,
        simulateHighLevel
    ) where

import Prelude hiding (read)
import Text.Pretty.Simple (pPrint)
import Data.SBV
import Redfin
import Redfin.Assembly hiding (div, abs)
import qualified Redfin.Assembly as Assembly
import Redfin.Verify
import Redfin.Language.Expression
import Redfin.Examples.Common

energyEstimate :: (Integral a, Num a) => a -> a -> a -> a -> a
energyEstimate t1 t2 p1 p2 = abs (t1 - t2) * (p1 + p2) `div` 2

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
    -- div_si r0 2
    halt

equivalence :: Symbolic SBool
equivalence = do
    t1 <- forall "t1"
    t2 <- forall "t2"
    p1 <- forall "p1"
    p2 <- forall "p2"
    constrain $ t1 .>= 0 &&& t1 .<= 948672000000
    constrain $ t2 .>= 0 &&& t2 .<= 948672000000
    constrain $ p1 .>= 0 &&& p1 .<= 1000
    constrain $ p2 .>= 0 &&& p2 .<= 1000
    let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
        steps = 100
        finalStateLL = verify steps $ templateState energyEstimateLowLevel mem
        finalStateHL = verify steps $ templateState energyEstimateHighLevel mem
        resultLL = readArray (registers finalStateLL) 0
        resultHL = readArray (registers finalStateHL) 0
    -- pure $ resultHL .== energyEstimate t1 t2 p1 p2 -- resultHL
    pure $ resultLL .== resultHL

highLevelFaultyExample :: Symbolic SBool
highLevelFaultyExample = do
    t1 <- forall "t1"
    t2 <- forall "t2"
    p1 <- forall "p1"
    p2 <- forall "p2"
    constrain $ p1 .>= 0
    constrain $ p2 .>= 0
    let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
        steps = 100
        finalState = verify steps $ templateState energyEstimateHighLevel mem
        result = readArray (registers finalState) 0
        overflow = readArray (flags finalState) (flagId Overflow)
    pure $   bnot overflow
         &&& result .>= 0

highLevelCorrect :: Symbolic SBool
highLevelCorrect = do
    t1 <- forall "t1"
    t2 <- forall "t2"
    p1 <- forall "p1"
    p2 <- forall "p2"
    constrain $ t1 .>= 0 &&& t1 .<= 948672000000
    constrain $ t2 .>= 0 &&& t2 .<= 948672000000
    constrain $ p1 .>= 0 &&& p1 .<= 1000
    constrain $ p2 .>= 0 &&& p2 .<= 1000
    let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
        steps = 100
        finalState = verify steps $ templateState energyEstimateHighLevel mem
        result = readArray (registers finalState) 0
        overflow = readArray (flags finalState) (flagId Overflow)
    pure $   bnot overflow
         &&& result .>= 0

simulateHighLevel :: IO ()
simulateHighLevel = do
    let mem = initialiseMemory [(0, 10), (1, 5), (2, 3), (3, 5), (5, 100)]
        finalState = verify 100 $ templateState energyEstimateHighLevel mem
        memoryDump = dumpMemory 0 255 $ memory finalState
    putStr "Memory Dump: "
    pPrint memoryDump
    putStrLn $ "Estimated energy: " ++ show (readArray (registers finalState) 0)
    putStrLn $ "Clock: " ++ show (clock finalState)

-------------- Energy Estimate WCET analysis -----------------------------------
-- worstCaseClock :: IO OptimizeResult
-- worstCaseClock = optimize Lexicographic $ do
--     t1 <- sInt64 "t1"
--     t2 <- sInt64 "t2"
--     p1 <- sInt64 "p1"
--     p2 <- sInt64 "p2"
--     constrain $ t1 .>= 0 &&& t1 .<= (1000 :: SInt64)
--     constrain $ t2 .>= 0 &&& t2 .<= (1000 :: SInt64)
--     constrain $ p1 .>= 0 &&& p1 .<= 1000
--     constrain $ p2 .>= 0 &&& p2 .<= 1000
--     -- constrain $ t1 .== 1
--     -- constrain $ t2 .== 2
--     -- constrain $ p1 .== 1
--     -- constrain $ p2 .== 1
--     let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
--         steps = 100
--         finalStateHL = verify steps $ templateState energyEstimate mem
--         finalStateLL = verify steps $ templateState energyEstimateLowLevel mem
--     -- maximize "Max clock HL" $ clock finalStateHL
--     -- maximize "Max clock LL" $ clock finalStateLL
--     -- minimize "Min clock HL" $ clock finalStateHL
--     minimize "Min clock LL" $ clock finalStateLL

worstCaseClock = optimize Independent $ do
    t1 <- sInt64 "t1"
    t2 <- sInt64 "t2"
    p1 <- sInt64 "p1"
    p2 <- sInt64 "p2"
    constrain $ t1 .>= 0 &&& t1 .<= 948672000000
    constrain $ t2 .>= 0 &&& t2 .<= 948672000000
    constrain $ p1 .>= 0 &&& p1 .<= 1000
    constrain $ p2 .>= 0 &&& p2 .<= 100
    let dataMemory = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
        finalState = verify 100 (templateState energyEstimateLowLevel dataMemory)
    maximize "Max clock" $ clock finalState
    minimize "Min clock" $ clock finalState

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
