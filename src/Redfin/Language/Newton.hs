module Redfin.Language.Newton where

import Text.Pretty.Simple (pPrint)
import Data.SBV
import Prelude hiding (read)
import Redfin
import Redfin.Assembly
import Redfin.Verify

r0, r1, r2, r3 :: Register
[r0, r1, r2, r3] = [0..3]

emptyRegisters :: RegisterBank
emptyRegisters = mkSFunArray $ const 0

emptyFlags :: Flags
emptyFlags = mkSFunArray $ const false

initialiseMemory :: [(MemoryAddress, Value)] -> Memory
initialiseMemory =
    foldr (\(a, v) m -> writeArray m a v) (mkSFunArray $ const 0)

-- dumpMemory :: Word8 -> Word8 -> Memory -> [SWord64]
dumpMemory from to m =
    filter ((/= 0) . snd) $
    zip [0..] $ map (readArray m) [literal from..literal to]

templateState :: Script -> Memory -> State
templateState src mem = State { registers = emptyRegisters
                               , instructionCounter = 0
                               , instructionRegister = 0
                               , program = assemble src
                               , flags = emptyFlags
                               , memory = mem
                               , clock = 0
                               }

sqr :: Num (Expression a) => Expression a -> Expression a
sqr x = x * x

example :: Register -> Script
example reg = do
    let x = FixedPointVariable 0
        y = FixedPointVariable 1
        z = FixedPointVariable 2
        t = Temporary 3
        s = Stack 100
    ld_i r0 200
    st r0 100
    evaluate reg s t $ read y / read x


-- newton :: Script
-- newton = do
--     let expr =
--     evaluate

-- newtonMemory

simulate :: IO ()
simulate = do
    putStrLn "Program: "
    -- pPrint $ zip [0..] $ snd $ runWriter (arraySum 5)
    -- let mem = initialiseMemory [(252, 0), (253, 3), (254, 2), (255, 1)]
    let mem = initialiseMemory [(0, 2), (1, 5), (2, 3), (3, 0)]
        finalState = verify 1000 $ templateState (example r0) mem
        memoryDump = dumpMemory 0 255 $ memory finalState
    putStrLn "Final state: "

    -- putStrLn $ "Instruction register: " ++ (show $ instructionRegister finalState)
    -- putStrLn "Flags register Dump: "
    print $ "*  Halted: " ++ show (readArray (flags finalState) (flagId Halt))
    print $ "*  Condition: " ++ show (readArray (flags finalState) (flagId Condition))
    print $ "*  OutOfMemory: " ++ show (readArray (flags finalState) (flagId OutOfMemory))
    -- -- print $ "*  OutOfProgram: " ++ show (readArray (flags finalState) (flagId OutOfProgram))

    putStr "Memory Dump: "
    pPrint memoryDump
    putStrLn $ "R0: " ++ show (readArray (registers finalState) 0)
    putStrLn $ "R1 = " ++ show (readArray (registers finalState) 1)
    putStrLn $ "R2 = " ++ show (readArray (registers finalState) 2)
    pPrint finalState