module Redfin.Examples.Sum where

import           Data.SBV                   hiding (label)
import           Prelude                    hiding (read)
import           Redfin.Assembly            hiding (abs, div)
import qualified Redfin.Assembly            as Assembly
import           Redfin.Examples.Common
import           Redfin.Language.Expression
import           Redfin.Listing
import           Redfin.Simulate
import           Redfin.Types
import           Text.Pretty.Simple         (pPrint)

-- -- arraySize :: Int
-- -- arraySize = 11

sumArray :: Num a => [a] -> a
sumArray = sum

sumArrayHighLevel :: Int -> Script
sumArrayHighLevel arraySize = do
    let xs = map (read . IntegerVariable) [2..(fromIntegral arraySize) + 1]
        temp = Temporary 0
        stack = Stack 1
    compile r0 stack temp (sumArray xs)
    halt

sumArrayLowLevel :: Script
sumArrayLowLevel = do
    let { pointer = 0; sum = 253; const_two = 255 } -- ; pointer_store
    ld_i r0 0
    st r0 sum
    ld r1 pointer
    add_si r1 1
    st r1 pointer

    -- compare the pointer variable to the constant 2 (stored in the cell 255)
    "loop" @@ cmplt r1 const_two
    -- if pointer == 2 then terminate
    goto_ct "end"
    -- jmpi_ct 7

    ldmi r2 pointer
    add r2 sum
    st r2 sum
    ld r1 pointer
    sub_si r1 1
    st r1 pointer

    goto "loop"
    "end" @@ ld r0 sum
    halt

type Constrain = Value -> Symbolic ()

type Statement = State -> SBool

sumArrayTheorem :: Script -> Int -> Constrain -> Statement -> Symbolic SBool
sumArrayTheorem src arraySize constr statement = do
    let names = map (("x" ++) . show) [1..arraySize]
    summands <- symbolics names
    -- constrain xs to be in [0, 1000]
    sequence_ (zipWith ($) (repeat constr) summands)
    mem <- mkMemory "memory"
                (zip [2..] summands ++
                 [(1, 100)] ++
                 [(0, literal . fromIntegral $ arraySize)] ++
                 [(255, 2)])
    emptyRegs <- mkRegisters "registers" []
    emptyFlags <- mkFlags "flags" []
    prog <- assemble sumArrayLowLevel
    let steps = 1000
        finalState = simulate steps $ boot prog emptyRegs mem emptyFlags
        result = readArray (registers finalState) 0
        halted = readArray (flags finalState) (flagId Halt)
        overflow = readArray (flags finalState) (flagId Overflow)
    pure $ statement finalState

faultyExample :: Script -> Int -> Symbolic SBool
faultyExample src arraySize =
    let constr    x     = constrain sTrue
        statement state =
            let halted = readArray (flags state) (flagId Halt)
                overflow = readArray (flags state) (flagId Overflow)
            in halted .&& sNot overflow
    in sumArrayTheorem src arraySize constr statement

noOverflow :: Script -> Int -> Symbolic SBool
noOverflow src arraySize =
    let constr    x     = constrain (x .>= 0 .&& x .<= 1000)
        statement state =
            let halted = readArray (flags state) (flagId Halt)
                overflow = readArray (flags state) (flagId Overflow)
            in halted .&& sNot overflow
    in sumArrayTheorem src arraySize constr statement

equivHaskell :: Script -> Int -> Symbolic SBool
equivHaskell src arraySize = do
    let names = map (("x" ++) . show) [1..arraySize]
    summands <- symbolics names
    -- summands <- map literal [1..10]
    -- constrain xs to be in [0, 1000]
    -- mapM_ (\x -> constrain (x .== 1)) summands
    mapM_ (\x -> constrain (x .>= 0 .&& x .<= 1000)) summands
    emptyRegs <- mkRegisters "registers" []
    emptyFlags <- mkFlags "flags" []
    prog <- assemble src
    mem <- mkMemory "memory" (zip [2..] summands ++ [(1, 100)] ++
                                [(0, literal . fromIntegral $ arraySize)] ++ [(255, 2)])
    let steps = 1000
        finalState = simulate steps $ boot prog emptyRegs mem emptyFlags
        result = readArray (registers finalState) 0
    pure $ result .== sumArray summands
