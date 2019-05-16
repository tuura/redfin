module Redfin.Examples.CyclicShift where

import Control.Monad.IO.Class (liftIO)
import Prelude hiding (read)
import Text.Pretty.Simple (pPrint)
import Data.SBV hiding (label)
import Redfin
import Redfin.Assembly hiding (div, abs)
import qualified Redfin.Assembly as Assembly
import Redfin.Listing
import Redfin.Simulate
import Redfin.Language.Expression
import Redfin.Examples.Common
import Redfin.Listing

-- arraySize :: Int
-- arraySize = 11

swap :: (MemoryAddress, MemoryAddress) -> MemoryAddress -> Register -> Script
swap (x, y) temp r = do
    ldmi r x
    st r temp
    ldmi r y
    stmi r x
    ld r temp
    stmi r y

arrayShiftLeft :: Script
arrayShiftLeft = do
    -- i = 0, store i in cell 252
    ld_i r0 0
    st r0 252
     -- j = 1, store j in cell 253
    ld_i r1 1
    st r1 253
    -- while (j <= size of array)
    "loop" @@ cmplt r1 255 -- Is j greater than the size of array?
    goto_cf "end" -- jmpi_cf 11 -- Jump to the instruction right after the loop body
    -- loop body
    swap (252, 253) 254 r2 -- this is 6 instructions
    add_si r0 1
    st r0 252
    add_si r1 1
    st r1 253
    goto "loop"
    -- end loop
    "end" @@ halt

arrayShiftLeftMemory :: [Value] -> Memory
arrayShiftLeftMemory xs = initialiseMemory $
    zip [0..] xs ++ -- array
    [ (252, 0)      -- i
    , (253, 0)      -- j
    , (254, 0)      -- temporary variable
    , (255, fromIntegral $ length xs)]     -- array size
--------------------------------------------------------------------------------

shiftedLeft :: Int -> Symbolic SBool
shiftedLeft n = do
    -- n <- forall "n" -- pure 10
    -- constrain $ n .> 0 &&& n .< 100
    -- let xs = map (literal . snd) $ zip [0..n-1] [1,2..]
    xs <- symbolics (map ('x':) $ map show [0..n-1])
    let initialState = boot arrayShiftLeft (arrayShiftLeftMemory xs)
        finalState = simulate 100000 $ initialState
    let arr  = dumpMemory 0 (fromIntegral n - 1) . memory $ initialState
        arr' = dumpMemory 0 (fromIntegral n - 1) . memory $ finalState
        overflow = readArray (flags finalState) (flagId Overflow)
        halted   = readArray (flags finalState) (flagId Halt)
    -- liftIO $ mapM_ print arr
    -- liftIO $ mapM_ print arr'
    pure $ halted &&&
           (map snd arr') `isLeftCyclicShiftOf` (map snd arr)

isLeftCyclicShiftOf :: [Value] -> [Value] -> SBool
isLeftCyclicShiftOf [] [] = true
isLeftCyclicShiftOf xs (y:ys) = xs .== ys ++ [y]
isLeftCyclicShiftOf _ _ = false
