module Redfin.Examples.Common where

import           Data.Bifunctor
import           Data.SBV        hiding (SFunArray, SymArray (..))
import           Redfin.Assembly
import           Redfin.SBV
import           Redfin.Types

mkRegisters :: [(Register, Value)] -> RegisterBank
mkRegisters = sListArray 0

defaultRegisters :: RegisterBank
defaultRegisters = mkRegisters []

mkFlags :: [(Flag, Bool)] -> Flags
mkFlags inits = sListArray False (map (bimap flagId literal) inits)

defaultFlags :: Flags
defaultFlags = mkFlags []

mkMemory :: [(MemoryAddress, Value)] -> Memory
mkMemory inits = sListArray 0 inits

defaultMemory :: Memory
defaultMemory = mkMemory []

dumpMemory :: WordN 8 -> WordN 8 -> Memory -> [(Int, Value)]
dumpMemory from to m =
  filter ((/=0) . snd) . zip [0 ..] . map (readArray m) $
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
