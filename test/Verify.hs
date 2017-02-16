import qualified Data.Map.Strict as Map
import Data.SBV

import Redfin.Verification hiding (memory, program)
import Redfin.Verification.Assembly
import Redfin.Verification.Verify

script :: Script
script = do
    ld_i 0 0    -- Set R0 to 0 (result).
    ld_i 1 1    -- Set R1 to 1 (counter).
    st 1 6      -- Set the pointer to R1.
    cmpgt 1 0   -- Are we done?
    jmpi_ct 5    -- If yes, go to halt.
    ldmi 2 6    -- Load the next number.
    st 2 7      -- Store it in a temporary variable.
    add 0 7     -- Accumulate result.
    add_si 1 1  -- Increment the counter.
    jmpi (-8)
    halt

initialiseMemory :: [(MemoryAddress, Value)] -> Memory
initialiseMemory = foldr (\(a, v) m -> writeArray m a v) (mkSFunArray $ const 0)

memory :: Memory
memory = initialiseMemory
    [ (0, 5)    -- How many numbers to add?
    , (1, 1)    -- First number
    , (2, 2)
    , (3, 0)
    , (4, 3)
    , (5, 8) ]  -- Last number
                -- 6: Pointer to the current number (used by the program).
                -- 7: Temporary variable for addition.

main :: IO ()
main = do
    let program      = assemble script
        initialState = State (mkSFunArray $ const 0) 0 0 Map.empty memory program 0
        finalState   = verify initialState
    putStrLn $ "Initial state:\n" ++ show initialState
    putStrLn $ "Final state:\n" ++ show finalState
