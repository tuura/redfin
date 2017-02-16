{-# LANGUAGE BinaryLiterals, DeriveFunctor #-}
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
module Redfin.Assembly (
    -- * Assembly scripts and assembler
    Script, assemble, topOpcode,

    -- * Arithmetic instructions
    add, add_si, sub, sub_si, mul, mul_si, div, div_si,

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
    wait, halt
    )where

import Control.Monad
import Data.Bits hiding (bit, xor)
import qualified Data.Map.Strict as Map
import Prelude hiding (and, div, not, or)

import Redfin

-- TODO: Get rid of ad-hoc fromIntegral.

-- | An assembly writer monad.
data Writer a = Writer
    { runWriter :: (Program -> (a, Program))
    , topOpcode :: Opcode
    } deriving Functor

type Script = Writer ()

assemble :: Script -> Program
assemble s = snd $ runWriter s Map.empty

instance Applicative Writer where
    pure  = return
    (<*>) = ap

instance Monad Writer where
    return a         = Writer (\p -> (a, p)) (error "Empty script")
    Writer w f >>= k = Writer (\p -> let (a, p') = w p in runWriter (k a) p') f

write :: Opcode -> InstructionCode -> Script
write o c = Writer
    (\p -> ((), Map.insert (fromIntegral $ Map.size p) (opcode o .|. c) p)) o

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
halt   = write 0b000000 0

opcode :: Opcode -> InstructionCode
opcode o = shiftL (fromIntegral o) 10

register :: Register -> InstructionCode
register r = fromIntegral $ shiftL (fromEnum r) 8

address :: MemoryAddress -> InstructionCode
address = fromIntegral

simm8 :: SImm8 -> InstructionCode
simm8 (SImm8 s) = fromIntegral s .&. 255

simm10 :: SImm10 -> InstructionCode
simm10 (SImm10 s) = fromIntegral s .&. 1023

uimm8 :: UImm8 -> InstructionCode
uimm8 (UImm8 u) = fromIntegral u

uimm10 :: UImm10 -> InstructionCode
uimm10 (UImm10 u) = fromIntegral u
