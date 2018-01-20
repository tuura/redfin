{-# LANGUAGE ScopedTypeVariables #-}
import Data.SBV hiding (label)

import Redfin hiding (memory, program)
import Redfin.Assembly
import Redfin.Verify

r0, r1, r2, r3 :: Register
[r0, r1, r2, r3] = [0..3]

emptyRegisters :: RegisterBank
emptyRegisters = mkSFunArray $ const 0

emptyFlags :: Flags
emptyFlags = mkSFunArray $ const false

initialiseMemory :: [(MemoryAddress, Value)] -> Memory
initialiseMemory = foldr (\(a, v) m -> writeArray m a v) (mkSFunArray $ const 0)

script1 :: UImm10 -> Script
script1 x = do
    ld_i r0 0    -- Set R0 to 0 (result).
    ld_i r1 1    -- Set R1 to 1 (counter).
    loop <- label
    st r1 6      -- Set the pointer to R1.
    cmpgt r1 0   -- Are we done?
    jmpi_ct 5    -- If yes, go to halt.
    ldmi r2 6    -- Load the next number.
    st r2 7      -- Store it in a temporary variable.
    add r0 7     -- Accumulate result.
    add_si r1 1  -- Increment the counter.
    goto loop
    wait x       -- Wait for x clock cycles
    halt

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

script2 :: Script
script2 = do
    mul_si r1 77 -- R1 *= 77
    div_si r2 5  -- R2 /= 5
    st r2 0      -- [0] <- R2
    add r1 0     -- R1 += [0]
    halt

script3 :: InstructionCode -> Script
script3 code = do
    ld_i r3 99 -- R3 <- 99
    asm code   -- ???
    halt

main :: IO ()
main = do
    let program = assemble $ script1 10
        result  = verify 50 $ State emptyRegisters 0 0 emptyFlags memory program 0
    -- Can it take more than 100 clock cycles?
    fasterThan100 <- prove $ clock result .< 100
    putStrLn $ "\nfasterThan100:\n" ++ show fasterThan100

    let program2 = assemble script2
    eq1234 <- sat $ do
        v1 :: Value <- exists "R1"
        v2 :: Value <- exists "R2"
        constrain $ v1 .< 16
        constrain $ v2 .> 10 &&& v2 .< 100
        let rs  = writeArray emptyRegisters r1 v1
            rs' = writeArray rs r2 v2
            result2 = verify 10 $ State rs' 0 0 emptyFlags memory program2 0
        return $ readArray (registers result2) r1 .== 1234
    putStrLn $ "\neq1234:\n" ++ show eq1234

    -- opcode of ld_i   = 101111
    -- opcode of div_si = 000111
    -- opcode of sr     = 011101
    synthesise49 <- sat $ do
        code <- exists "Instruction"
        constrain $ sTestBit code 15 .== false
        let program3 = assemble $ script3 code
            result3  = verify 2 $ State emptyRegisters 0 0 emptyFlags memory program3 0
        --return $ readArray (registers result3) r3 .== 49
        return $ readArray (registers result3) r3 .== 49 &&& clock result3 .< 20
    putStrLn $ "\nsynthesise49:\n" ++ show synthesise49
