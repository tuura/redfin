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
module Redfin.Decode (
    executeInstruction,

    decodeOpcode, decodeRegister, decodeMemoryAddress, decodeSImm8,
    decodeSImm10, decodeUImm8, decodeUImm10
    ) where

import Data.Foldable
import Data.SBV

import Redfin
import qualified Redfin.Assembly  as A
import qualified Redfin.Semantics as S

-- TODO: Add documentation.

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
decodeAndExecute code = Redfin $ \s -> ((), decode code s)

decode :: InstructionCode -> State -> State
decode code state = transformA
  where
    opcode       = decodeOpcode code
    register     = decodeRegister code
    address      = decodeMemoryAddress code
    simm8        = decodeSImm8 code
    uimm8        = decodeUImm8 code
    simm10       = decodeSImm10 code
    uimm10       = decodeUImm10 code
    illegal      = writeFlag IllegalInstruction true
    goA k (a, s) = ite (A.topOpcode a .== opcode) (snd $ redfin s state) k
    transformA   = foldl' goA transformB
        [(A.halt, S.halt)]
    goB k (a, s) = ite (A.topOpcode (a 0 0) .== opcode) (snd $ redfin (s register address) state) k
    transformB = foldl' goB transformC
        [ (A.and  , S.and  )
        , (A.or   , S.or   )
        , (A.xor  , S.xor  )
        , (A.add  , S.add  )
        , (A.sub  , S.sub  )
        , (A.mul  , S.mul  )
        , (A.div  , S.div  )
        , (A.fadd  , S.fadd  )
        , (A.fsub  , S.fsub  )
        , (A.fmul  , S.fmul  )
        , (A.fdiv  , S.fdiv  )
        , (A.ld   , S.ld   )
        , (A.st   , S.st   )
        , (A.ldmi , S.ldmi )
        , (A.stmi , S.stmi )
        , (A.cmpeq, S.cmpeq)
        , (A.cmplt, S.cmplt)
        , (A.cmpgt, S.cmpgt)
        , (A.sl   , S.sl   )
        , (A.sr   , S.sr   )
        , (A.sra  , S.sra  ) ]
    goC k (a, s) = ite (A.topOpcode (a 0 0) .== opcode) (snd $ redfin (s register simm8) state) k
    transformC = foldl' goC transformE
        [ (A.add_si, S.add_si)
        , (A.sub_si, S.sub_si)
        , (A.mul_si, S.mul_si)
        , (A.div_si, S.div_si)
        , (A.ld_si , S.ld_si ) ]
    goE k (a, s) = ite (A.topOpcode (a 0 0) .== opcode) (snd $ redfin (s register uimm8) state) k
    transformE = foldl' goE transformFS
        [ (A.sl_i , S.sl_i )
        , (A.sr_i , S.sr_i )
        , (A.sra_i, S.sra_i)
        , (A.ld_i , S.ld_i ) ]
    goFS k (a, s) = ite (A.topOpcode (a 0) .== opcode) (snd $ redfin (s simm10) state) k
    transformFS = foldl' goFS transformFU
        [ (A.jmpi   , S.jmpi   )
        , (A.jmpi_ct, S.jmpi_ct)
        , (A.jmpi_cf, S.jmpi_cf) ]
    goFU k (a, s) = ite (A.topOpcode (a 0) .== opcode) (snd $ redfin (s uimm10) state) k
    transformFU = foldl' goFU transformG
        [ (A.wait, S.wait) ]
    goG k (a, s) = ite (A.topOpcode (a 0) .== opcode) (snd $ redfin (s register) state) k
    transformG = foldl' goG (snd $ redfin illegal state)
        [ (A.not, S.not)
        , (A.abs, S.abs) ]

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
