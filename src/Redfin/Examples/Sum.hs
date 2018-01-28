module Redfin.Examples.Sum where

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

arraySize :: Int
arraySize = 10

sumArray :: Num a => [a] -> a
sumArray = sum

sumArrayHighLevel :: Script
sumArrayHighLevel = do
    let xs = map (read . IntegerVariable) [2..(fromIntegral arraySize) + 1]
        temp = Temporary 0
        stack = Stack 1
    compile r0 stack temp (sumArray xs)
    halt

type Constrain = Value -> Symbolic ()

type Statement = State -> SBool

sumArrayTheorem :: Constrain -> Statement -> IO ThmResult
sumArrayTheorem constr statement = proveWith prover $ do
    let names = map (("x" ++) . show) [1..arraySize]
    summands <- symbolics names
    -- constrain xs to be in [0, 1000]
    sequence_ (zipWith ($) (repeat constr) summands)
    let mem = initialiseMemory (zip [2..] summands ++ [(1, 100)])
        steps = 1000
        finalState = verify steps $ templateState sumArrayHighLevel mem
        result = readArray (registers finalState) 0
        halted = readArray (flags finalState) (flagId Halt)
        overflow = readArray (flags finalState) (flagId Overflow)
    pure $ statement finalState

faultyExample :: IO ThmResult
faultyExample =
    let constr    x     = constrain true
        statement state =
            let halted = readArray (flags state) (flagId Halt)
                overflow = readArray (flags state) (flagId Overflow)
            in halted &&& bnot overflow
    in sumArrayTheorem constr statement

noOverflow :: IO ThmResult
noOverflow =
    let constr    x     = constrain (x .>= 0 &&& x .<= 1000)
        statement state =
            let halted = readArray (flags state) (flagId Halt)
                overflow = readArray (flags state) (flagId Overflow)
            in halted &&& bnot overflow
    in sumArrayTheorem constr statement

equivHaskell :: IO ThmResult
equivHaskell = proveWith prover $ do
    let names = map (("x" ++) . show) [1..arraySize]
    summands <- symbolics names
    -- constrain xs to be in [0, 1000]
    mapM_ (\x -> constrain (x .>= 0 &&& x .<= 1000)) summands
    let mem = initialiseMemory (zip [2..] summands ++ [(1, 100)])
        steps = 1000
        finalState = verify steps $ templateState sumArrayHighLevel mem
        result = readArray (registers finalState) 0
    pure $ result .== sumArray summands