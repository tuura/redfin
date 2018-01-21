module Redfin.Language.Examples.Common where

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

dumpMemory :: Word8 -> Word8 -> Memory -> [(Int, Value)]
dumpMemory from to m = filter ((/=0) . snd) $ zip [0 ..] $ map
    (readArray m)
    [literal from .. literal to]

templateState :: Script -> Memory -> State
templateState src mem = State
    { registers           = emptyRegisters
    , instructionCounter  = 0
    , instructionRegister = 0
    , program             = assemble src
    , flags               = emptyFlags
    , memory              = mem
    , clock               = 0
    }

prover :: SMTConfig
prover = z3 { verbose   = True
            , smtFile   = Just "example.smt2"
            , timing    = True
            , printBase = 10
            }
