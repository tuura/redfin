{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Assembly
-- Copyright   :  (c) Andrey Mokhov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
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
-- import           Data.Bits           hiding (bit, xor)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromJust)
import           Data.SBV            hiding (label)
import           Prelude             hiding (abs, and, div, not, or)

import           Redfin.Types        hiding (State, instructionCounter, program)

type Labels = Map.Map String InstructionAddress

data AssemblerState =
    MkAssemblerState { program            :: [(InstructionAddress, InstructionCode)]
                     , labels             :: Labels
                     , instructionCounter :: InstructionAddress
                     }

type Script = State AssemblerState ()

type P = [InstructionCode]

-- | An assembly writer monad.
data Writer a = Writer
    { runWriter :: P -> (a, P)
    } deriving Functor

instance Show a => Show (Writer a) where
    show s = show $ reverse $ snd $ runWriter s []

collectLabels :: Script -> Labels
collectLabels src =
    labels $ snd $ runState src (MkAssemblerState [] Map.empty 0)

-- | Translate an assembly script into binary machine codes
machineCode :: Script -> [InstructionCode]
machineCode src =
    let labels = collectLabels src
    in map snd $ reverse $ program $ snd $ runState src (MkAssemblerState [] labels 0)

-- | Assemble an assembly script into a program
assemble :: Script -> Symbolic Program
assemble src = do
  a0 <- newArray_ (Just 0)
  return $ foldr (\(c, p) a -> writeArray a p c) a0 (zip prg [0..])
  where
    prg    = machineCode src

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

write :: Opcode -> InstructionCode -> Script
write o c = do
    s <- get
    let ic = instructionCounter s
    put $ s { program = (ic, (opcode o .|. c)):program s
            , instructionCounter = ic + 1}

-- Instructions
and   rX dmemaddr = write 0b000001 (register rX .|. address dmemaddr)
or    rX dmemaddr = write 0b000010 (register rX .|. address dmemaddr)
xor   rX dmemaddr = write 0b000011 (register rX .|. address dmemaddr)
add   rX dmemaddr = write 0b000100 (register rX .|. address dmemaddr)
sub   rX dmemaddr = write 0b000101 (register rX .|. address dmemaddr)
mul   rX dmemaddr = write 0b000110 (register rX .|. address dmemaddr)
div   rX dmemaddr = write 0b000111 (register rX .|. address dmemaddr)
ld    rX dmemaddr = write 0b001000 (register rX .|. address dmemaddr)
st    rX dmemaddr = write 0b001001 (register rX .|. address dmemaddr)
ldmi  rX dmemaddr = write 0b001010 (register rX .|. address dmemaddr)
stmi  rX dmemaddr = write 0b001011 (register rX .|. address dmemaddr)
cmpeq rX dmemaddr = write 0b010001 (register rX .|. address dmemaddr)
cmplt rX dmemaddr = write 0b010010 (register rX .|. address dmemaddr)
cmpgt rX dmemaddr = write 0b010011 (register rX .|. address dmemaddr)
sl    rX dmemaddr = write 0b011100 (register rX .|. address dmemaddr)
sr    rX dmemaddr = write 0b011101 (register rX .|. address dmemaddr)
sra   rX dmemaddr = write 0b011110 (register rX .|. address dmemaddr)

fadd   rX dmemaddr = write 0b001100 (register rX .|. address dmemaddr)
fsub   rX dmemaddr = write 0b001101 (register rX .|. address dmemaddr)
fmul   rX dmemaddr = write 0b001110 (register rX .|. address dmemaddr)
fdiv   rX dmemaddr = write 0b001111 (register rX .|. address dmemaddr)

add_si rX simm = write 0b100000 (register rX .|. simm8 simm)
sub_si rX simm = write 0b100001 (register rX .|. simm8 simm)
mul_si rX simm = write 0b100010 (register rX .|. simm8 simm)
div_si rX simm = write 0b100011 (register rX .|. simm8 simm)
ld_si  rX simm = write 0b100111 (register rX .|. simm8 simm)

sl_i   rX uimm = write 0b101100 (register rX .|. uimm8 uimm)
sr_i   rX uimm = write 0b101101 (register rX .|. uimm8 uimm)
sra_i  rX uimm = write 0b101110 (register rX .|. uimm8 uimm)
ld_i   rX uimm = write 0b101111 (register rX .|. uimm8 uimm)

jmpi    simm = write 0b110000 (simm10 simm)
jmpi_ct simm = write 0b110001 (simm10 simm)
jmpi_cf simm = write 0b110010 (simm10 simm)
wait    uimm = write 0b110011 (uimm10 uimm)

not rX = write 0b111000 (register rX)
abs rX = write 0b111001 (register rX)
halt   = write 0b000000 0

opcode :: Opcode -> InstructionCode
opcode o = fromBitsLE $ pad 10 ++ (take 6 $ blastLE o)

decodeOpcode :: InstructionCode -> Opcode
decodeOpcode c = fromBitsLE $ (drop 10 $ blastLE c) ++ pad 2

register :: Register -> InstructionCode
register r = fromBitsLE $ pad 8 ++ (take 2 $ blastLE r) ++ pad 6

address :: MemoryAddress -> InstructionCode
address a = fromBitsLE $ blastLE a ++ pad 8

simm8 :: SImm8 -> InstructionCode
simm8 s = fromBitsLE $ blastLE s ++ pad 8

simm10 :: SImm10 -> InstructionCode
simm10 s = fromBitsLE $ (take 10 $ blastLE s) ++ pad 6

uimm8 :: UImm8 -> InstructionCode
uimm8 u = fromBitsLE $ blastLE u ++ pad 8

uimm10 :: UImm10 -> InstructionCode
uimm10 = fromBitsLE . blastLE
