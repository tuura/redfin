module Redfin.Examples.Common where

import Data.Maybe (fromJust)
import Data.SBV
import Redfin
import Redfin.Assembly

r0, r1, r2, r3 :: Register
[r0, r1, r2, r3] = [0 .. 3]

emptyRegisters :: RegisterBank
emptyRegisters = mkSFunArray $ const 0

emptyFlags :: Flags
emptyFlags = mkSFunArray $ const false

initialiseMemory :: [(MemoryAddress, Value)] -> Memory
initialiseMemory =
    foldr (\(a, v) m -> writeArray m a v) (mkSFunArray $ const 0)

-- | Dump the address fron the list 'addrs'
dumpMemory :: [Word8] -> Memory -> [(Word8, Int64)]
dumpMemory addrs m =
    zip [0 ..] $
    map (fromJust . unliteral) $
    map (readArray m) (map literal addrs)

dumpRegisters :: RegisterBank -> [(Word8, Int64)]
dumpRegisters regs =
    zip [0, 1, 2, 3]  $
    map (fromJust . unliteral) $
    map (readArray regs) [r0, r1, r2, r3]

dumpFlags :: Flags -> [(Flag, Bool)]
dumpFlags regs =
    let fs = [Halt, Overflow, Condition] in
    zip fs  $
    map (fromJust . unliteral) $
    map (readArray regs) (map flagId fs)

boot :: Script -> Memory -> State
boot src mem = State
    { registers           = emptyRegisters
    , instructionCounter  = 0
    , instructionRegister = 0
    , program             = assemble src
    , flags               = emptyFlags
    , memory              = mem
    , clock               = 0
    }

prover :: SMTConfig
prover = z3 { verbose = True
            , redirectVerbose = Just "example.smt2"
            , timing = PrintTiming
            , printBase = 10
            }