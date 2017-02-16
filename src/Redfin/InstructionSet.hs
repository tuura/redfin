-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.InstructionSet
-- Copyright   :  (c) Andrey Mokhov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- REDFIN instruction set.
--
-----------------------------------------------------------------------------
module Redfin.InstructionSet (
    -- * Types of instructions
    typeA, typeB, typeC, typeD, typeE, typeF, typeG, typeH,

    -- * Instruction decoding primitives
    decodeOpcode, decodeRegister, decodeMemoryAddress, at,
    decodeSImm8, decodeUImm8, decodeSImm10, decodeUImm10, decodeUImm5s
    ) where

import Data.Bits hiding (bit, xor)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Redfin
import qualified Redfin.Assembly  as A
import qualified Redfin.Semantics as S

typeA :: Map Opcode (Redfin ())
typeA = Map.fromList $ map (\(m, s) -> (getOpcode m, s))
    [(A.halt, S.halt)]

typeB :: Map Opcode (Register -> MemoryAddress -> Redfin ())
typeB = Map.fromList $ map (\(m, s) -> (getOpcode $ m R0 0, s))
    [ (A.and  , S.and  )
    , (A.or   , S.or   )
    , (A.xor  , S.xor  )
    , (A.add  , S.add  )
    , (A.sub  , S.sub  )
    , (A.mul  , S.mul  )
    , (A.div  , S.div  )
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

typeC :: Map Opcode (Register -> SImm8 -> Redfin ())
typeC = Map.fromList $ map (\(m, s) -> (getOpcode $ m R0 0, s))
    [ (A.add_si, S.add_si)
    , (A.sub_si, S.sub_si)
    , (A.mul_si, S.mul_si)
    , (A.div_si, S.div_si)
    , (A.ld_si , S.ld_si ) ]

-- | TypeD instructions are currently not implemented.
typeD :: Map Opcode (Register -> SImm8 -> Redfin ())
typeD = Map.empty

typeE :: Map Opcode (Register -> UImm8 -> Redfin ())
typeE = Map.fromList $ map (\(m, s) -> (getOpcode $ m R0 0, s))
    [ (A.sl_i , S.sl_i )
    , (A.sr_i , S.sr_i )
    , (A.sra_i, S.sra_i)
    , (A.ld_i , S.ld_i ) ]

typeF :: Map Opcode (SImm10 -> Redfin ())
typeF = Map.fromList $ map (\(m, s) -> (getOpcode $ m 0, s))
    [ (A.jmpi   , S.jmpi   )
    , (A.jmpi_ct, S.jmpi_ct)
    , (A.jmpi_cf, S.jmpi_cf) ]

typeG :: Map Opcode (Register -> Redfin ())
typeG = Map.fromList $ map (\(m, s) -> (getOpcode $ m R0, s))
    [ (A.not, S.not) ]

-- | TypeH instruction 'pmac' is currently not implemented.
typeH :: Map Opcode (UImm5 -> UImm5 -> Redfin ())
typeH = Map.empty

at :: InstructionCode -> (Int, Int) -> Int
at code (high, low) =
    foldr (\bit res -> res * 2 + if testBit code bit then 1 else 0) 0 [low..high]

getOpcode :: A.Script -> Opcode
getOpcode = decodeOpcode . A.firstCode

decodeOpcode :: InstructionCode -> Opcode
decodeOpcode code = fromIntegral $ code `at` (15, 10)

decodeRegister :: InstructionCode -> Register
decodeRegister code = case (testBit code 9, testBit code 8) of
    (False, False) -> R0
    (False, True ) -> R1
    (True , False) -> R2
    (True , True ) -> R3

decodeMemoryAddress :: InstructionCode -> MemoryAddress
decodeMemoryAddress code = fromIntegral $ code `at` (7, 0)

decodeSImm8 :: InstructionCode -> SImm8
decodeSImm8 code = fromIntegral $ code `at` (7, 0)

decodeUImm8 :: InstructionCode -> UImm8
decodeUImm8 code = fromIntegral $ code `at` (7, 0)

decodeSImm10 :: InstructionCode -> SImm10
decodeSImm10 code = fromIntegral $ code `at` (9, 0)

decodeUImm10 :: InstructionCode -> UImm10
decodeUImm10 code = fromIntegral $ code `at` (9, 0)

decodeUImm5s :: InstructionCode -> (UImm5, UImm5)
decodeUImm5s code = (fromIntegral $ code `at` (9, 5), fromIntegral $ code `at` (4, 0))
