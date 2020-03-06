module Redfin.Examples.ManhattanDistance where

import           Data.SBV
import           Prelude                    hiding (read)
import           Redfin
import           Redfin.Assembly            hiding (abs, div)
import qualified Redfin.Assembly            as Assembly
import           Redfin.Examples.Common
import           Redfin.Language.Expression
import           Redfin.Listing
import           Redfin.Simulate
import           Text.Pretty.Simple         (pPrint)

-- -- | Do we really need more than 4 dimensions?
-- pointsCount :: Int
-- pointsCount = 4

-- distance :: Num a => [a] -> [a] -> a
-- distance xs ys = sum $ zipWith (\x y -> abs (x - y)) xs ys

-- distanceHighLevel :: Script
-- distanceHighLevel = do
--     let xs = map (read . IntegerVariable) [2..(fromIntegral pointsCount) + 1]
--         ys = map (read . IntegerVariable)
--                  [(fromIntegral pointsCount) + 2..2 * (fromIntegral pointsCount) + 1]
--         temp = Temporary 0
--         stack = Stack 1
--     compile r0 stack temp (distance xs ys)
--     halt

-- faultyExample :: Script -> Symbolic SBool
-- faultyExample src = do
--     let xsNames = map (("x" ++) . show) [1..pointsCount]
--         ysNames = map (("y" ++) . show) [1..pointsCount]
--     xs <- symbolics xsNames
--     ys <- symbolics ysNames
--     let mem = initialiseMemory (zip [2..] (xs ++ ys) ++ [(1, 100)])
--         steps = 1000
--         finalState = simulate steps $ boot src mem
--         result = readArray (registers finalState) 0
--         halted = readArray (flags finalState) (flagId Halt)
--         overflow = readArray (flags finalState) (flagId Overflow)
--     pure $ bnot overflow

-- noOverflow :: Script -> Symbolic SBool
-- noOverflow src = do
--     let xsNames = map (("x" ++) . show) [1..pointsCount]
--         ysNames = map (("y" ++) . show) [1..pointsCount]
--     xs <- symbolics xsNames
--     ys <- symbolics ysNames
--     mapM_ (\x -> constrain (x .>= 0 &&& x .<= 1000)) (xs ++ ys)
--     let mem = initialiseMemory (zip [2..] (xs ++ ys) ++ [(1, 100)])
--         steps = 1000
--         finalState = simulate steps $ boot src mem
--         result = readArray (registers finalState) 0
--         overflow = readArray (flags finalState) (flagId Overflow)
--     pure $ bnot overflow

-- equivHaskell :: Script -> Symbolic SBool
-- equivHaskell src = do
--     let xsNames = map (("x" ++) . show) [1..pointsCount]
--         ysNames = map (("y" ++) . show) [1..pointsCount]
--     xs <- symbolics xsNames
--     ys <- symbolics ysNames
--     mapM_ (\x -> constrain (x .>= 0 &&& x .<= 1000)) (xs ++ ys)
--     let mem = initialiseMemory (zip [2..] (xs ++ ys) ++ [(1, 100)])
--         steps = 1000
--         finalState = simulate steps $ boot src mem
--         result = readArray (registers finalState) 0
--     pure $ result .== distance xs ys
