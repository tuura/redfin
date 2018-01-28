module Redfin.Examples.ManhattanDistance where

import Prelude hiding (read)
import Text.Pretty.Simple (pPrint)
import Data.SBV
import Redfin
import Redfin.Assembly hiding (div, abs)
import qualified Redfin.Assembly as Assembly
import Redfin.Listing
import Redfin.Verify
import Redfin.Language.Expression
import Redfin.Examples.Common

-- | Do we really need more than 4 dimensions?
pointsCount :: Int
pointsCount = 4

distance :: Num a => [a] -> [a] -> a
distance xs ys = sum $ zipWith (\x y -> abs (x - y)) xs ys

distanceHighLevel :: Script
distanceHighLevel = do
    let xs = map (read . IntegerVariable) [2..(fromIntegral pointsCount) + 1]
        ys = map (read . IntegerVariable)
                 [(fromIntegral pointsCount) + 2..2 * (fromIntegral pointsCount) + 1]
        temp = Temporary 0
        stack = Stack 1
    compile r0 stack temp (distance xs ys)
    halt

faultyExample :: Symbolic SBool
faultyExample = do
    let xsNames = map (("x" ++) . show) [1..pointsCount]
        ysNames = map (("y" ++) . show) [1..pointsCount]
    xs <- symbolics xsNames
    ys <- symbolics ysNames
    let mem = initialiseMemory (zip [2..] (xs ++ ys) ++ [(1, 100)])
        steps = 1000
        finalState = verify steps $ templateState distanceHighLevel mem
        result = readArray (registers finalState) 0
        halted = readArray (flags finalState) (flagId Halt)
        overflow = readArray (flags finalState) (flagId Overflow)
    pure $ halted &&& bnot overflow

noOverflow :: Symbolic SBool
noOverflow = do
    let xsNames = map (("x" ++) . show) [1..pointsCount]
        ysNames = map (("y" ++) . show) [1..pointsCount]
    xs <- symbolics xsNames
    ys <- symbolics ysNames
    mapM_ (\x -> constrain (x .>= 0 &&& x .<= 1000)) (xs ++ ys)
    let mem = initialiseMemory (zip [2..] (xs ++ ys) ++ [(1, 100)])
        steps = 1000
        finalState = verify steps $ templateState distanceHighLevel mem
        result = readArray (registers finalState) 0
    pure $ result .== distance xs ys

equivHaskell :: Symbolic SBool
equivHaskell = do
    let xsNames = map (("x" ++) . show) [1..pointsCount]
        ysNames = map (("y" ++) . show) [1..pointsCount]
    xs <- symbolics xsNames
    ys <- symbolics ysNames
    mapM_ (\x -> constrain (x .>= 0 &&& x .<= 1000)) (xs ++ ys)
    let mem = initialiseMemory (zip [2..] (xs ++ ys) ++ [(1, 100)])
        steps = 1000
        finalState = verify steps $ templateState distanceHighLevel mem
        result = readArray (registers finalState) 0
    pure $ result .== distance xs ys