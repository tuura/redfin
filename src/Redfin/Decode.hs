-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Decode
-- Copyright   :  (c) Andrey Mokhov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- REDFIN instruction set.
--
-----------------------------------------------------------------------------
{-# LANGUAGE BinaryLiterals #-}
module Redfin.Decode (
    executeInstruction,

    decodeOpcode, decodeRegister, decodeMemoryAddress, decodeSImm8,
    decodeSImm10, decodeUImm8, decodeUImm10
    ) where
import Data.SBV

import Redfin
import qualified Redfin.Semantics as S

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

decodeAndExecute :: InstructionCode -> Redfin ()
decodeAndExecute code =
    let opcode       = decodeOpcode code
        register     = decodeRegister code
        address      = decodeMemoryAddress code
        simm8        = decodeSImm8 code
        uimm8        = decodeUImm8 code
        simm10       = decodeSImm10 code
        uimm10       = decodeUImm10 code
    in case opcode of
         0b000001 -> S.and register address
         0b000010 -> S.or     register address
         0b000011 -> S.xor    register address
         0b000100 -> S.add    register address
         0b000101 -> S.sub    register address
         0b000110 -> S.mul    register address
         0b000111 -> S.div    register address
         0b001000 -> S.ld     register address
         0b001001 -> S.st     register address
         0b001010 -> S.ldmi   register address
         0b001011 -> S.stmi   register address
         0b010001 -> S.cmpeq  register address
         0b010010 -> S.cmplt  register address
         0b010011 -> S.cmpgt  register address
         0b011100 -> S.sl     register address
         0b011101 -> S.sr     register address
         0b011110 -> S.sra    register address
         0b001100 -> S.fadd   register address
         0b001101 -> S.fsub   register address
         0b001110 -> S.fmul   register address
         0b001111 -> S.fdiv   register address
         0b100000 -> S.add_si   register simm8
         0b100001 -> S.sub_si   register simm8
         0b100010 -> S.mul_si   register simm8
         0b100011 -> S.div_si   register simm8
         0b100111 -> S.ld_si   register simm8
         0b101100 -> S.sl_i    register uimm8
         0b101101 -> S.sr_i    register uimm8
         0b101110 -> S.sra_i    register uimm8
         0b101111 -> S.ld_i    register uimm8
         0b110000 -> S.jmpi   simm10
         0b110001 -> S.jmpi_ct   simm10
         0b110010 -> S.jmpi_cf   simm10
         0b110011 -> S.wait   uimm10
         0b111000 -> S.not   register
         0b111001 -> S.abs   register
         0b000000 -> S.halt
         _        -> writeFlag IllegalInstruction true

pad :: Int -> [SBool]
pad k = replicate k false

decodeOpcode :: InstructionCode -> Opcode
decodeOpcode c = fromBitsLE $ (drop 10 $ blastLE c) ++ pad 2

decodeRegister :: InstructionCode -> Register
decodeRegister c = fromBitsLE $ (take 2 $ drop 8 $ blastLE c) ++ pad 6

decodeMemoryAddress :: InstructionCode -> MemoryAddress
decodeMemoryAddress c = fromBitsLE $ (take 8 $ blastLE c)

decodeSImm8 :: InstructionCode -> SImm8
decodeSImm8 c = fromBitsLE $ (take 8 $ blastLE c)

decodeSImm10 :: InstructionCode -> SImm10
decodeSImm10 c = fromBitsLE $ (take 10 $ blastLE c) ++ pad 6

decodeUImm8 :: InstructionCode -> UImm8
decodeUImm8 c = fromBitsLE $ (take 8 $ blastLE c)

decodeUImm10 :: InstructionCode -> UImm10
decodeUImm10 c = fromBitsLE $ (take 10 $ blastLE c) ++ pad 6
