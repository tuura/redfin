{-# LANGUAGE TypeApplications #-}
module Redfin.Examples.Energy.OverflowHunt where

import Prelude hiding (read)
import Data.SBV
import Redfin
import Redfin.Verify
import Redfin.Examples.Common
import Redfin.Examples.Energy

---------------- Overflow Hunt -------------------------------------------------
-- | 't1 - t2' causes an integer overflow
t1MinusT2overflow :: IO OptimizeResult
t1MinusT2overflow = optimize Lexicographic $ do
    t1 <- sInt64 "t1"
    t2 <- sInt64 "t2"
    p1 <- sInt64 "p1"
    p2 <- sInt64 "p2"
    constrain $ t2 .> 0
    constrain $ t1 .< minBound @Value + t2
    let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
        steps = 100
        finalStateHL = verify steps $ templateState energyEstimateHighLevel mem
        overflow = readArray (flags finalStateHL) (flagId Overflow)
    constrain $ overflow .== true
    minimize "t1_min" $ abs t1
    minimize "t2_min" $ abs t2
-- Optimal model:
--   t1     = -9223372036854775808 :: Int64 -- -(2^63)
--   t2     =                    1 :: Int64
--   p1     =                    0 :: Int64
--   p2     =                    0 :: Int64

-- | 'p1 + p2' causes an integer overflow
p1PlusP2overflow :: IO OptimizeResult
p1PlusP2overflow = optimize Lexicographic $ do
    t1 <- sInt64 "t1"
    t2 <- sInt64 "t2"
    p1 <- sInt64 "p1"
    p2 <- sInt64 "p2"
    constrain $ p2 .> 0
    constrain $ p1 .> maxBound @Value - p2
    let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
        steps = 100
        finalStateHL = verify steps $ templateState energyEstimateHighLevel mem
        overflow = readArray (flags finalStateHL) (flagId Overflow)
    constrain $ overflow .== true
    minimize "p1_min" $ abs p1
    minimize "p2_min" $ abs p2
-- Optimal model:
--   t1     =                   0 :: Int64
--   t2     =                   0 :: Int64
--   p1     =                   1 :: Int64
--   p2     = 9223372036854775807 :: Int64 -- 2^63 - 1

-- | 'abs (t1 - t2)' causes an integer overflow
absOverflow :: IO OptimizeResult
absOverflow = optimize Lexicographic $ do
    t1 <- sInt64 "t1"
    t2 <- sInt64 "t2"
    p1 <- sInt64 "p1"
    p2 <- sInt64 "p2"
    constrain $ t1 - t2 .== minBound @Value
    let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
        steps = 100
        finalStateHL = verify steps $ templateState energyEstimateHighLevel mem
        overflow = readArray (flags finalStateHL) (flagId Overflow)
    constrain $ overflow .== true
    minimize "t1_min" $ abs t1
    minimize "t2_min" $ abs t2
-- Optimal model:
--   t1     = -9223372036854775808 :: Int64 --  -(2^63)
--   t2     =                    0 :: Int64
--   p1     =                    0 :: Int64
--   p2     =                    0 :: Int64

-- | let x = abs (t1 - t2), y = p1 + p2; 'x * y' causes an integer overflow
mulOverflow:: IO OptimizeResult
mulOverflow = optimize Lexicographic $ do
    t1 <- sInt64 "t1"
    t2 <- sInt64 "t2"
    p1 <- sInt64 "p1"
    p2 <- sInt64 "p2"
    let x = abs (t1 - t2)
        y = p1 + p2
    constrain $ t1 .> 0
    constrain $ t2 .> 0
    constrain $ p1 .> 0
    constrain $ p2 .> 0
    constrain $ x .> 0
    constrain $ y .> 0
    constrain $ x .> (maxBound @Value) `sDiv` y
    let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
        steps = 100
        finalStateHL = verify steps $ templateState energyEstimateHighLevel mem
        overflow = readArray (flags finalStateHL) (flagId Overflow)
    constrain $ overflow .== true
    minimize "t1_min" $ abs t1
    minimize "t2_min" $ abs t2
    minimize "p1_min" $ abs p1
    minimize "p2_min" $ abs p2
-- Optimal model:
--   t1     =                   1 :: Int64
--   t2     =                   3 :: Int64
--   p1     =                   1 :: Int64
--   p2     = 4611686018427387903 :: Int64 -- 2 ^ 62

-- | let x = abs(t1 - t2) * p1 + p2, 'x `div` 2' causes an integer overflow
-- | If 'x' itself doesn't cause an overflow itself, there is no way
--   'x `div` 2' can.