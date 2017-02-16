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
    Script (..), assemble, firstCode,

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
data Script a = Script { runScript :: (Program -> (a, Program)) } deriving Functor

assemble :: Script () -> Program
assemble s = snd $ runScript s Map.empty

instance Applicative Script where
    pure  = return
    (<*>) = ap

instance Monad Script where
    return a       = Script $ \p -> (a, p)
    Script w >>= f = Script $ \p -> let (a, p') = w p in runScript (f a) p'

write :: InstructionCode -> Script ()
write code = Script $ \p -> ((), Map.insert (fromIntegral $ Map.size p) code p)

-- | Extract the first 'InstrctionCode' from the given program 'Script'. Raise
-- an error if the script is empty.
firstCode :: Script () -> InstructionCode
firstCode s = Map.findWithDefault (error "Empty script") 0 (assemble s)

and   rX dmemaddr = write $ opcode 0b000001 + register rX + address dmemaddr
or    rX dmemaddr = write $ opcode 0b000010 + register rX + address dmemaddr
xor   rX dmemaddr = write $ opcode 0b000011 + register rX + address dmemaddr
add   rX dmemaddr = write $ opcode 0b000100 + register rX + address dmemaddr
sub   rX dmemaddr = write $ opcode 0b000101 + register rX + address dmemaddr
mul   rX dmemaddr = write $ opcode 0b000110 + register rX + address dmemaddr
div   rX dmemaddr = write $ opcode 0b000111 + register rX + address dmemaddr
ld    rX dmemaddr = write $ opcode 0b001000 + register rX + address dmemaddr
st    rX dmemaddr = write $ opcode 0b001001 + register rX + address dmemaddr
ldmi  rX dmemaddr = write $ opcode 0b001010 + register rX + address dmemaddr
stmi  rX dmemaddr = write $ opcode 0b001011 + register rX + address dmemaddr
cmpeq rX dmemaddr = write $ opcode 0b010001 + register rX + address dmemaddr
cmplt rX dmemaddr = write $ opcode 0b010010 + register rX + address dmemaddr
cmpgt rX dmemaddr = write $ opcode 0b010011 + register rX + address dmemaddr
sl    rX dmemaddr = write $ opcode 0b011100 + register rX + address dmemaddr
sr    rX dmemaddr = write $ opcode 0b011101 + register rX + address dmemaddr
sra   rX dmemaddr = write $ opcode 0b011110 + register rX + address dmemaddr

add_si rX simm = write $ opcode 0b100000 + register rX + simm8 simm
sub_si rX simm = write $ opcode 0b100001 + register rX + simm8 simm
mul_si rX simm = write $ opcode 0b100010 + register rX + simm8 simm
div_si rX simm = write $ opcode 0b100011 + register rX + simm8 simm
ld_si  rX simm = write $ opcode 0b100111 + register rX + simm8 simm

sl_i   rX uimm = write $ opcode 0b101100 + register rX + uimm8 uimm
sr_i   rX uimm = write $ opcode 0b101101 + register rX + uimm8 uimm
sra_i  rX uimm = write $ opcode 0b101110 + register rX + uimm8 uimm
ld_i   rX uimm = write $ opcode 0b101111 + register rX + uimm8 uimm

jmpi    simm = write $ opcode 0b110000 + simm10 simm
jmpi_ct simm = write $ opcode 0b110001 + simm10 simm
jmpi_cf simm = write $ opcode 0b110010 + simm10 simm
wait    uimm = write $ opcode 0b110011 + uimm10 uimm

not rX = write $ opcode 0b111000 + register rX
halt   = write $ opcode 0b000000

opcode :: Opcode -> InstructionCode
opcode o = shiftL (fromIntegral o) 10

register :: Register -> InstructionCode
register r = fromIntegral $ shiftL (fromEnum r) 8

address :: MemoryAddress -> InstructionCode
address = fromIntegral

simm8 :: SImm8 -> InstructionCode
simm8 (SImm8 s) = fromIntegral s .&. 255

uimm8 :: UImm8 -> InstructionCode
uimm8 (UImm8 u) = fromIntegral u

simm10 :: SImm10 -> InstructionCode
simm10 (SImm10 s) = fromIntegral s .&. 1023

uimm10 :: UImm10 -> InstructionCode
uimm10 (UImm10 u) = fromIntegral u
