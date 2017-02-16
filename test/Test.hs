import qualified Data.Map.Strict as Map

import Redfin hiding (memory, program)
import Redfin.Assembly
import Redfin.Simulate

script :: Script
script = do
    ld_i R0 0    -- Set R0 to 0 (result).
    ld_i R1 1    -- Set R1 to 1 (counter).
    st R1 6      -- Set the pointer to R1.
    cmpgt R1 0   -- Are we done?
    jmpi_ct 5    -- If yes, go to halt.
    ldmi R2 6    -- Load the next number.
    st R2 7      -- Store it in a temporary variable.
    add R0 7     -- Accumulate result.
    add_si R1 1  -- Increment the counter.
    jmpi (-8)
    halt

memory :: Memory
memory = Map.fromList
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
        initialState = State Map.empty 0 0 Map.empty memory program 0
        finalState   = simulate initialState
    putStrLn $ "\nInitial state = " ++ show initialState
    putStrLn $ "\nFinal state = " ++ show finalState
