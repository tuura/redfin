module Redfin.Examples.ManhattanDistance where

import           Data.SBV                   hiding (SFunArray, SymArray (..))
import           Prelude                    hiding (read)
import           Redfin.Assembly            hiding (abs, div)
import qualified Redfin.Assembly            as Assembly
import           Redfin.Examples.Common
import           Redfin.Language.Expression
import           Redfin.Listing
import           Redfin.SBV
import           Redfin.Simulate
import           Redfin.Types
import           Text.Pretty.Simple         (pPrint)

-- | Do we really need more than 4 dimensions?
pointsCount :: Int
pointsCount = 4

distance :: Num a => [a] -> [a] -> a
distance xs ys = sum $ zipWith (\x y -> abs (x - y)) xs ys

distanceHighLevel :: Script
distanceHighLevel = do
    let xs = map varAtAddress
          [2..(fromIntegral pointsCount) + 1]
        ys = map varAtAddress
             [(fromIntegral pointsCount) +
               2..2 * (fromIntegral pointsCount) + 1]
    compile (distance xs ys)
    halt

faultyExample :: Script -> Symbolic SBool
faultyExample src = do
    let xsNames = map (("x" ++) . show) [1..pointsCount]
        ysNames = map (("y" ++) . show) [1..pointsCount]
    xs <- symbolics xsNames
    ys <- symbolics ysNames
    let prog = assemble src
        mem = mkMemory (zip [2..] (xs ++ ys) ++ [(1, 100)])
    let steps = 1000
        initialState = boot prog defaultRegisters mem defaultFlags
        finalState = simulate steps initialState
        result = readArray (registers finalState) 0
        halted = readArray (flags finalState) (flagId Halt)
        overflow = readArray (flags finalState) (flagId Overflow)
    pure $ sNot overflow

noOverflow :: Script -> Symbolic SBool
noOverflow src = do
    let xsNames = map (("x" ++) . show) [1..pointsCount]
        ysNames = map (("y" ++) . show) [1..pointsCount]
    xs <- symbolics xsNames
    ys <- symbolics ysNames
    let prog = assemble src
        mem = mkMemory (zip [2..] (xs ++ ys) ++ [(1, 100)])
    mapM_ (\x -> constrain (x .>= 0 .&& x .<= 1000)) (xs ++ ys)
    let steps = 1000
        initialState = boot prog defaultRegisters mem defaultFlags
        finalState = simulate steps initialState
        result = readArray (registers finalState) 0
        overflow = readArray (flags finalState) (flagId Overflow)
    pure $ sNot overflow

equivHaskell :: Script -> Symbolic SBool
equivHaskell src = do
    let xsNames = map (("x" ++) . show) [1..pointsCount]
        ysNames = map (("y" ++) . show) [1..pointsCount]
    xs <- symbolics xsNames
    ys <- symbolics ysNames
    let prog = assemble src
    mapM_ (\x -> constrain (x .>= 0 .&& x .<= 1000)) (xs ++ ys)
    let mem = mkMemory (zip [2..] (xs ++ ys) ++ [(1, 100)])
    let steps = 1000
        initialState = boot prog defaultRegisters mem defaultFlags
        finalState = simulate steps initialState
        result = readArray (registers finalState) 0
    pure $ result .== distance xs ys
