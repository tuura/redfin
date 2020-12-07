{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Assembly
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2017-2020
--
-- Maintainer  :  mail@geo2a.info
-- Stability   :  experimental
--
-- An embedded REDFIN assembly language.
--
-----------------------------------------------------------------------------
-- module Redfin.Assembly where
module Redfin.Assembly (
    -- * Assembly scripts and assembler
    Script, machineCode, assemble, label, (@@), goto, goto_ct, goto_cf,

    -- * Arithmetic instructions
    add, add_si, sub, sub_si, mul, mul_si, div, div_si,
    fadd, fsub, fmul, fdiv, Redfin.Assembly.abs,

    -- * Logical bit-wise instructions
    Redfin.Assembly.and, Redfin.Assembly.or,
    Redfin.Assembly.xor, Redfin.Assembly.not,
    sl, sl_i, sr, sr_i, sra, sra_i,

    -- * Load/store instructions
    ld, ld_i, ld_si, ldmi, st, stmi,

    -- * Comparison instructions
    cmpeq, cmplt, cmpgt,

    -- * Jump instructions
    jmpi, jmpi_ct, jmpi_cf,

    -- * Miscellaneous instructions
    wait, halt,

    runWriter
    ) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromJust)
import           Data.SBV            hiding (SFunArray, SymArray (..), label)
import           Prelude             hiding (abs, and, div, not, or)
import           Redfin.SBV

import           Redfin.Types        hiding (State, instructionCounter, program)

-- | Table of labels is used for jumps
type Labels = Map.Map String InstructionAddress

data AssemblerState = MkAssemblerState
  { program            :: [(InstructionAddress, InstructionCode)]
  , labels             :: Labels
  , instructionCounter :: InstructionAddress
  }

type Script = State AssemblerState ()

-- | An assembly writer monad.
data Writer a = Writer
    { runWriter :: [InstructionCode] -> (a, [InstructionCode])
    } deriving Functor

instance Show a => Show (Writer a) where
    show s = show $ reverse $ snd $ runWriter s []

-- | Find all labels in a script
collectLabels :: Script -> Labels
collectLabels src =
    labels $ snd $ runState src (MkAssemblerState [] Map.empty 0)

-- | Translate an assembly script into binary machine codes
machineCode :: Script -> [(InstructionAddress, InstructionCode)]
machineCode src =
    let labels = collectLabels src
    in reverse . program . snd $
       runState src (MkAssemblerState [] labels 0)

-- | Assemble an assembly script into a program
assemble :: Script -> Program
assemble src = sListArray 0 (machineCode src)

-- | Declare a label in a program
label :: String -> Script
label name = do
    s <- get
    let ic = instructionCounter s
    put $ s {labels = Map.insert name ic $ labels s}

-- | Infix analog of label --- declare a label in the program
(@@) :: String -> Script -> Script
name @@ src = do
    label name
    src

-- | Unconditionally goto a label
goto :: String -> Script
goto name = do
    s <- get
    here <- instructionCounter <$> get
    case Map.lookup name (labels s) of
         Nothing -> jmpi 0
         Just there -> do
             let offset = there - here - 1
             jmpi (fromIntegral . fromJust . unliteral $ offset)

-- | Goto a label if the 'Condition' flag is set
goto_ct :: String -> Script
goto_ct name = do
    s <- get
    here <- instructionCounter <$> get
    case Map.lookup name (labels s) of
         Nothing -> jmpi 0
         Just there -> do
             let offset = there - here - 1
             jmpi_ct (fromIntegral . fromJust . unliteral $ offset)

-- | Goto a label if the 'Condition' flag is not set
goto_cf :: String -> Script
goto_cf name = do
    s <- get
    here <- instructionCounter <$> get
    case Map.lookup name (labels s) of
         Nothing -> jmpi 0
         Just there -> do
             let offset = there - here - 1
             jmpi_cf (fromIntegral . fromJust . unliteral $ offset)

-- -- | Add an instruction into a script
-- write :: InstructionCode -> Script
-- write c = do
--     s <- get
--     let ic = instructionCounter s
--     put $ s { program = (ic, c):program s
--             , instructionCounter = ic + 1}

-- | Add an instruction into a script
instruction :: InstructionCode -> Script
instruction c = do
    s <- get
    let ic = instructionCounter s
    put $ s { program = (ic, c):program s
            , instructionCounter = ic + 1}

opcode :: WordN 6 -> Opcode
opcode = literal

-- | Arithmetic instructions
add, sub, mul, div :: Register -> MemoryAddress -> Script
add   rX dmemaddr = instruction (opcode 0b000100 # rX # dmemaddr)
sub   rX dmemaddr = instruction (opcode 0b000101 # rX # dmemaddr)
mul   rX dmemaddr = instruction (opcode 0b000110 # rX # dmemaddr)
div   rX dmemaddr = instruction (opcode 0b000111 # rX # dmemaddr)

abs :: Register -> Script
abs rX = instruction (opcode 0b111001 # rX # 0)

-- | Arithmetic instructions with immediate arguments
add_si, sub_si, mul_si, div_si :: Register -> SImm8 -> Script
add_si rX simm = instruction (opcode 0b100000 # rX # fromSigned @8 simm)
sub_si rX simm = instruction (opcode 0b100001 # rX # fromSigned @8 simm)
mul_si rX simm = instruction (opcode 0b100010 # rX # fromSigned @8 simm)
div_si rX simm = instruction (opcode 0b100011 # rX # fromSigned @8 simm)

-- | Fixed-pint arithmetic instructions
fadd, fsub, fmul, fdiv :: Register -> MemoryAddress -> Script
fadd   rX dmemaddr = instruction (opcode 0b001100 # rX # dmemaddr)
fsub   rX dmemaddr = instruction (opcode 0b001101 # rX # dmemaddr)
fmul   rX dmemaddr = instruction (opcode 0b001110 # rX # dmemaddr)
fdiv   rX dmemaddr = instruction (opcode 0b001111 # rX # dmemaddr)

-- | Logical bit-wise instructions
and, or, xor :: Register -> MemoryAddress -> Script
and   rX dmemaddr = instruction (opcode 0b000001 # rX # dmemaddr)
or    rX dmemaddr = instruction (opcode 0b000010 # rX # dmemaddr)
xor   rX dmemaddr = instruction (opcode 0b000011 # rX # dmemaddr)

not :: Register -> Script
not rX = instruction (opcode 0b111000 # rX # 0)

-- | Shifts
sl, sr, sra :: Register -> MemoryAddress -> Script
sl    rX dmemaddr = instruction (opcode 0b011100 # rX # dmemaddr)
sr    rX dmemaddr = instruction (opcode 0b011101 # rX # dmemaddr)
sra   rX dmemaddr = instruction (opcode 0b011110 # rX # dmemaddr)

-- | Shifts with immediate arguments
sl_i, sr_i, sra_i :: Register -> UImm8 -> Script
sl_i   rX uimm = instruction (opcode 0b101100 # rX # uimm)
sr_i   rX uimm = instruction (opcode 0b101101 # rX # uimm)
sra_i  rX uimm = instruction (opcode 0b101110 # rX # uimm)

-- | Load/store instructions
ld, st, ldmi, stmi :: Register -> MemoryAddress -> Script
ld    rX dmemaddr = instruction (opcode 0b001000 # rX # dmemaddr)
st    rX dmemaddr = instruction (opcode 0b001001 # rX # dmemaddr)
ldmi  rX dmemaddr = instruction (opcode 0b001010 # rX # dmemaddr)
stmi  rX dmemaddr = instruction (opcode 0b001011 # rX # dmemaddr)

ld_i :: Register -> UImm8 -> Script
ld_i rX uimm = instruction (opcode 0b101111 # rX # uimm)

ld_si :: Register -> SImm8 -> Script
ld_si rX simm = instruction (opcode 0b100111 # rX # fromSigned @8 simm)

-- | Comparison instructions
cmpeq, cmplt, cmpgt :: Register -> MemoryAddress -> Script
cmpeq rX dmemaddr = instruction (opcode 0b010001 # rX # dmemaddr)
cmplt rX dmemaddr = instruction (opcode 0b010010 # rX # dmemaddr)
cmpgt rX dmemaddr = instruction (opcode 0b010011 # rX # dmemaddr)

-- | Jump instructions
jmpi, jmpi_ct, jmpi_cf :: SImm10 -> Script
jmpi    simm = instruction (opcode 0b110000 # fromSigned @10 simm)
jmpi_ct simm = instruction (opcode 0b110001 # fromSigned @10 simm)
jmpi_cf simm = instruction (opcode 0b110010 # fromSigned @10 simm)

-- | Miscellaneous instructions
wait :: UImm10 -> Script
wait uimm = instruction $ 0b110011 # uimm

halt :: Script
halt = instruction 0
