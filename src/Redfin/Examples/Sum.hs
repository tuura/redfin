module Redfin.Examples.Sum where

import           Data.SBV                   hiding (label)
import           Prelude                    hiding (read)
import           Redfin
import           Redfin.Assembly            hiding (abs, div)
import qualified Redfin.Assembly            as Assembly
import           Redfin.Examples.Common
import           Redfin.Language.Expression
import           Redfin.Listing
import           Redfin.Simulate
import           Text.Pretty.Simple         (pPrint)

-- -- arraySize :: Int
-- -- arraySize = 11

-- sumArray :: Num a => [a] -> a
-- sumArray = sum

-- sumArrayHighLevel :: Int -> Script
-- sumArrayHighLevel arraySize = do
--     let xs = map (read . IntegerVariable) [2..(fromIntegral arraySize) + 1]
--         temp = Temporary 0
--         stack = Stack 1
--     compile r0 stack temp (sumArray xs)
--     halt

-- -- arrayShiftLeft :: Script
-- -- arrayShiftLeft = do
-- --     -- i = 0, store i in cell 252
-- --     ld_i r0 0
-- --     st r0 252
-- --      -- j = 1, store j in cell 253
-- --     ld_i r1 1
-- --     st r1 253
-- --     -- while (j <= size of array)
-- --     loop <- label
-- --     cmplt r1 255 -- Is j greater than the size of array?
-- --     jmpi_cf 11 -- Jump to the instruction right after the loop body
-- --     -- loop body
-- --     swap (252, 253) 254 r2 -- this is 6 instructions
-- --     add_si r0 1
-- --     st r0 252
-- --     add_si r1 1
-- --     st r1 253
-- --     goto loop
-- --     -- end loop
-- --     halt

-- sumArrayLowLevel :: Script
-- sumArrayLowLevel = do
--     let { pointer = 0; sum = 253; const_two = 255 } -- ; pointer_store
--     ld_i r0 0
--     st r0 sum
--     ld r1 pointer
--     add_si r1 1
--     st r1 pointer

--     -- compare the pointer variable to the constant 2 (stored in the cell 255)
--     "loop" @@ cmplt r1 const_two
--     -- if pointer == 2 then terminate
--     goto_ct "end"
--     -- jmpi_ct 7

--     ldmi r2 pointer
--     add r2 sum
--     st r2 sum
--     ld r1 pointer
--     sub_si r1 1
--     st r1 pointer

--     goto "loop"
--     "end" @@ ld r0 sum
--     halt

-- type Constrain = Value -> Symbolic ()

-- type Statement = State -> SBool

-- sumArrayTheorem :: Script -> Int -> Constrain -> Statement -> Symbolic SBool
-- sumArrayTheorem src arraySize constr statement = do
--     let names = map (("x" ++) . show) [1..arraySize]
--     summands <- symbolics names
--     -- constrain xs to be in [0, 1000]
--     sequence_ (zipWith ($) (repeat constr) summands)
--     mem <- mkMemory "memory"
--                 (zip [2..] summands ++
--                  [(1, 100)] ++
--                  [(0, literal . fromIntegral $ arraySize)] ++
--                  [(255, 2)])
--     emptyRegs <- mkRegisters "registers" []
--     emptyFlags <- mkFlags "flags" []
--     prog <- assemble sumArrayLowLevel
--     let steps = 1000
--         finalState = simulate steps $ boot prog emptyRegs mem
--         result = readArray (registers finalState) 0
--         halted = readArray (flags finalState) (flagId Halt)
--         overflow = readArray (flags finalState) (flagId Overflow)
--     pure $ statement finalState

-- faultyExample :: Script -> Int -> Symbolic SBool
-- faultyExample src arraySize =
--     let constr    x     = constrain sTrue
--         statement state =
--             let halted = readArray (flags state) (flagId Halt)
--                 overflow = readArray (flags state) (flagId Overflow)
--             in halted .&& sNot overflow
--     in sumArrayTheorem src arraySize constr statement

-- noOverflow :: Script -> Int -> Symbolic SBool
-- noOverflow src arraySize =
--     let constr    x     = constrain (x .>= 0 .&& x .<= 1000)
--         statement state =
--             let halted = readArray (flags state) (flagId Halt)
--                 overflow = readArray (flags state) (flagId Overflow)
--             in halted .&& sNot overflow
--     in sumArrayTheorem src arraySize constr statement

-- -- equivHaskell :: Int -> Symbolic SBool
-- -- equivHaskell arraySize = do
-- --     let names = map (("x" ++) . show) [1..arraySize]
-- --     summands <- symbolics names
-- --     -- constrain xs to be in [0, 1000]
-- --     mapM_ (\x -> constrain (x .>= 0 .&& x .<= 1000)) summands
-- --     let mem = initialiseMemory (zip [2..] summands ++ [(1, 100)])
-- --         steps = 1000
-- --         finalState = simulate steps $ boot (sumArrayHighLevel arraySize) mem
-- --         result = readArray (registers finalState) 0
-- --     pure $ result .== sumArray summands

-- equivHaskell :: Script -> Int -> Symbolic SBool
-- equivHaskell src arraySize = do
--     let names = map (("x" ++) . show) [1..arraySize]
--     summands <- symbolics names
--     -- summands <- map literal [1..10]
--     -- constrain xs to be in [0, 1000]
--     -- mapM_ (\x -> constrain (x .== 1)) summands
--     mapM_ (\x -> constrain (x .>= 0 .&& x .<= 1000)) summands
--     mem <- mkMemory (zip [2..] summands ++ [(1, 100)] ++
--                                 [(0, literal . fromIntegral $ arraySize)] ++ [(255, 2)])
--     let steps = 1000
--         finalState = simulate steps $ boot src mem
--         result = readArray (registers finalState) 0
--     pure $ result .== sumArray summands
