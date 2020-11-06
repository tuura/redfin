module Redfin.Examples.Common where

import           Data.SBV
import           Redfin.Assembly
import           Redfin.Types

r0, r1, r2, r3 :: Register
[r0, r1, r2, r3] = [0 .. 3]

mkRegisters :: String -> [(Register, Value)] -> Symbolic RegisterBank
mkRegisters name inits = do
    blanks <- newArray name (Just . literal $ 0)
    pure $ foldr (\(k, v) arr -> writeArray arr k v) blanks inits

mkFlags :: String -> [(Flag, Bool)] -> Symbolic Flags
mkFlags name inits = do
    blanks <- newArray name (Just sFalse)
    pure $ foldr (\(k, v) arr -> writeArray arr (flagId k) (literal v)) blanks inits

mkMemory :: String -> [(MemoryAddress, Value)] -> Symbolic Memory
mkMemory name inits = do
    blanks <- newArray name (Just . literal $ 0)
    pure $ foldr (\(k, v) arr -> writeArray arr k v) blanks inits

dumpMemory :: WordN 8 -> WordN 8 -> Memory -> [(Int, Value)]
dumpMemory from to m = filter ((/=0) . snd) $ zip [0 ..] $ map
    (readArray m)
    [literal from .. literal to]

boot :: Program -> RegisterBank -> Memory -> Flags -> State
boot prog regs mem flags =
    State
        { registers           = regs
        , instructionCounter  = 0
        , instructionRegister = 0
        , program             = prog
        , flags               = flags
        , memory              = mem
        , clock               = 0
        }

prover :: SMTConfig
prover = z3 { verbose = True
            , redirectVerbose = Just "example.smt2"
            , timing = PrintTiming
            , printBase = 10
            }
