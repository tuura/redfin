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
import qualified Redfin.Mnemonics as M
import qualified Redfin.Semantics as S

typeA :: Map Opcode (Redfin ())
typeA = Map.fromList $ map (\(m, s) -> (getOpcode m, s))
    [(M.halt, S.halt)]

typeB :: Map Opcode (Register -> MemoryAddress -> Redfin ())
typeB = Map.fromList $ map (\(m, s) -> (getOpcode $ m R0 0, s))
    [ (M.and  , S.and  )
    , (M.or   , S.or   )
    , (M.xor  , S.xor  )
    , (M.add  , S.add  )
    , (M.sub  , S.sub  )
    , (M.mul  , S.mul  )
    , (M.div  , S.div  )
    , (M.ld   , S.ld   )
    , (M.st   , S.st   )
    , (M.ldmi , S.ldmi )
    , (M.stmi , S.stmi )
    , (M.cmpeq, S.cmpeq)
    , (M.cmplt, S.cmplt)
    , (M.cmpgt, S.cmpgt)
    , (M.sl   , S.sl   )
    , (M.sr   , S.sr   )
    , (M.sra  , S.sra  ) ]

typeC :: Map Opcode (Register -> SImm8 -> Redfin ())
typeC = Map.fromList $ map (\(m, s) -> (getOpcode $ m R0 0, s))
    [ (M.add_si, S.add_si)
    , (M.sub_si, S.sub_si)
    , (M.mul_si, S.mul_si)
    , (M.div_si, S.div_si)
    , (M.ld_si , S.ld_si ) ]

-- | TypeD instructions are currently not implemented.
typeD :: Map Opcode (Register -> SImm8 -> Redfin ())
typeD = Map.empty

typeE :: Map Opcode (Register -> UImm8 -> Redfin ())
typeE = Map.fromList $ map (\(m, s) -> (getOpcode $ m R0 0, s))
    [ (M.sl_i , S.sl_i )
    , (M.sr_i , S.sr_i )
    , (M.sra_i, S.sra_i)
    , (M.ld_i , S.ld_i ) ]

typeF :: Map Opcode (SImm10 -> Redfin ())
typeF = Map.fromList $ map (\(m, s) -> (getOpcode $ m 0, s))
    [ (M.jmpi   , S.jmpi   )
    , (M.jmpi_ct, S.jmpi_ct)
    , (M.jmpi_cf, S.jmpi_cf) ]

typeG :: Map Opcode (Register -> Redfin ())
typeG = Map.fromList $ map (\(m, s) -> (getOpcode $ m R0, s))
    [ (M.not, S.not) ]

-- | TypeH instruction 'pmac' is currently not implemented.
typeH :: Map Opcode (UImm5 -> UImm5 -> Redfin ())
typeH = Map.empty

at :: InstructionCode -> (Int, Int) -> Int
at code (high, low) = let bits = reverse [low..high] in
    foldr (\bit res -> res * 2 + if testBit code bit then 1 else 0) 0 bits

getOpcode :: M.Writer () -> Opcode
getOpcode = decodeOpcode . M.firstCode

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
