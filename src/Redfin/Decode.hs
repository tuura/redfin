-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Decode
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2017-2020
--
-- Maintainer  :  mail@geo2a.info
-- Stability   :  experimental
--
-- REDFIN instruction set.
--
-----------------------------------------------------------------------------
{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE TypeApplications #-}
module Redfin.Decode (
    executeInstruction,

    decodeOpcode, decodeRegister, decodeMemoryAddress, decodeSImm8,
    decodeSImm10, decodeUImm8, decodeUImm10
    ) where
import           Data.Proxy
import           Data.SBV

import qualified Redfin.Semantics as S
import           Redfin.Types

-- | Execute the instruction pointed to by the instruction counter, performing
-- the following steps in sequence:
--
-- * Fetch the instruction from the program memory.
--
-- * Increment the instruction counter.
--
-- * Decode the instruction, extracting the opcode and immediate arguments.
--
-- * Execute the instruction by calling an appropriate implementation from the
--   "Redfin.Semantics" module.
executeInstruction :: Redfin ()
executeInstruction = do
    fetchInstruction
    incrementInstructionCounter
    decodeAndExecute =<< readInstructionRegister

-- | Decode an instruction code and translate it into it's semantics
decodeAndExecute :: InstructionCode -> Redfin ()
decodeAndExecute code =
    let opcode       = decodeOpcode code
        register     = decodeRegister code
        address      = decodeMemoryAddress code
        simm8        = decodeSImm8 code
        uimm8        = decodeUImm8 code
        simm10       = decodeSImm10 code
        uimm10       = decodeUImm10 code
    in case unliteral opcode of
         Just 0b000001 -> S.and register address
         Just 0b000010 -> S.or     register address
         Just 0b000011 -> S.xor    register address
         Just 0b000100 -> S.add    register address
         Just 0b000101 -> S.sub    register address
         Just 0b000110 -> S.mul    register address
         Just 0b000111 -> S.div    register address
         Just 0b001000 -> S.ld     register address
         Just 0b001001 -> S.st     register address
         Just 0b001010 -> S.ldmi   register address
         Just 0b001011 -> S.stmi   register address
         Just 0b010001 -> S.cmpeq  register address
         Just 0b010010 -> S.cmplt  register address
         Just 0b010011 -> S.cmpgt  register address
         Just 0b011100 -> S.sl     register address
         Just 0b011101 -> S.sr     register address
         Just 0b011110 -> S.sra    register address
         Just 0b001100 -> S.fadd   register address
         Just 0b001101 -> S.fsub   register address
         Just 0b001110 -> S.fmul   register address
         Just 0b001111 -> S.fdiv   register address
         Just 0b100000 -> S.add_si   register simm8
         Just 0b100001 -> S.sub_si   register simm8
         Just 0b100010 -> S.mul_si   register simm8
         Just 0b100011 -> S.div_si   register simm8
         Just 0b100111 -> S.ld_si   register simm8
         Just 0b101100 -> S.sl_i    register uimm8
         Just 0b101101 -> S.sr_i    register uimm8
         Just 0b101110 -> S.sra_i    register uimm8
         Just 0b101111 -> S.ld_i    register uimm8
         Just 0b110000 -> S.jmpi   simm10
         Just 0b110001 -> S.jmpi_ct   simm10
         Just 0b110010 -> S.jmpi_cf   simm10
         Just 0b110011 -> S.wait   uimm10
         Just 0b111000 -> S.not   register
         Just 0b111001 -> S.abs   register
         Just 0b000000 -> S.halt
         _             -> writeFlag IllegalInstruction sTrue

decodeOpcode :: InstructionCode -> Opcode
decodeOpcode = bvTake (Proxy @6)

decodeRegister :: InstructionCode -> Register
decodeRegister = bvTake (Proxy @2) . bvDrop (Proxy @6)

decodeMemoryAddress :: InstructionCode -> MemoryAddress
decodeMemoryAddress = bvDrop (Proxy @8)

decodeSImm8 :: InstructionCode -> SImm8
decodeSImm8 = toSigned . bvTake (Proxy @8) . bvDrop (Proxy @6)

decodeSImm10 :: InstructionCode -> SImm10
decodeSImm10 = toSigned . bvTake (Proxy @10) . bvDrop (Proxy @6)

decodeUImm8 :: InstructionCode -> UImm8
decodeUImm8 = bvTake (Proxy @8) . bvDrop (Proxy @6)

decodeUImm10 :: InstructionCode -> UImm10
decodeUImm10 = bvTake (Proxy @10) . bvDrop (Proxy @6)
