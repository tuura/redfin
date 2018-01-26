{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Redfin.Language.Examples.Energy (equivalence, theorem, simulate) where

import Prelude hiding (read)
import Text.Pretty.Simple (pPrint)
import Data.SBV
import Redfin
import Redfin.Assembly hiding (div, abs)
import qualified Redfin.Assembly as Assembly
import Redfin.Verify
import Redfin.Language.Expression
import Redfin.Language.Examples.Common

energyEstimateFormula :: (Integral a, Num a) => a -> a -> a -> a -> a
energyEstimateFormula t1 t2 p1 p2 = abs (t1 - t2) * (p1 + p2) `div` 2

energyEstimate :: Script
energyEstimate = do
    let t1    = read $ IntegerVariable 0
        t2    = read $ IntegerVariable 1
        p1    = read $ IntegerVariable 2
        p2    = read $ IntegerVariable 3
        temp  = Temporary 4
        stack = Stack 5
    compile r0 stack temp (energyEstimateFormula t1 t2 p1 p2)
    halt

energyEstimateLowLevel :: Script
energyEstimateLowLevel = do
    let t1 = 0
        t2 = 1
        p1 = 2
        p2 = 3
    ld r0 t1
    sub r0 t2
    Assembly.abs r0
    ld r1 p1
    add r1 p2
    st r1 p2
    mul r0 p2
    sra_i r0 1
    halt

equivalence :: IO ThmResult
equivalence = proveWith prover $ do
    t1 <- forall "t1"
    t2 <- forall "t2"
    p1 <- forall "p1"
    p2 <- forall "p2"
    constrain $ t1 .>= 0 &&& t1 .<= 2 ^ (32 :: Int)
    constrain $ t2 .>= 0 &&& t2 .<= 2 ^ (32 :: Int)
    constrain $ p1 .>= 0 &&& p1 .<= 1000
    constrain $ p2 .>= 0 &&& p2 .<= 1000
    let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2)]
        steps = 100
        finalState = verify steps $ templateState energyEstimateLowLevel mem
        result = readArray (registers finalState) 0
    pure $ result .== energyEstimateFormula t2 t1 p1 p2

theorem :: IO ThmResult
theorem = proveWith prover $ do
    t1 <- forall "t1"
    t2 <- forall "t2"
    p1 <- forall "p1"
    p2 <- forall "p2"
    constrain $ t1 .>= 0 &&& t1 .<= 2 ^ (32 :: Int)
    constrain $ t2 .>= 0 &&& t2 .<= 2 ^ (32 :: Int)
    constrain $ p1 .>= 0 &&& p1 .<= 1000
    constrain $ p2 .>= 0 &&& p2 .<= 1000
    let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2)]
        steps = 100
        finalState = verify steps $ templateState energyEstimate mem
        result = readArray (registers finalState) 0
    pure $ result .>= 0

simulate :: IO ()
simulate = do
    let mem = initialiseMemory [(0, 10), (1, 5), (2, 3), (3, 5)]
        finalState = verify 100 $ templateState energyEstimate mem
        memoryDump = dumpMemory 0 255 $ memory finalState
    putStr "Memory Dump: "
    pPrint memoryDump
    putStrLn $ "Estimated energy: " ++ show (readArray (registers finalState) 0)
    putStrLn $ "Clock: " ++ show (clock finalState)

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
