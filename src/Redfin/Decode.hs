{-# LANGUAGE BinaryLiterals #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Decode
-- Copyright   :  (c) Andrey Mokhov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- Decoding and executing REDFIN instructions.
--
-----------------------------------------------------------------------------
module Redfin.Decode (executeInstruction) where

import Data.Bits hiding (bit, xor)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Word
import Prelude hiding (and, div, not, or)

import Redfin
import Redfin.InstructionSet

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
--   "Redfin.InstructionSet" module.
executeInstruction :: Redfin ()
executeInstruction = do
    fetchInstruction
    incrementInstructionCounter
    decodeAndExecute =<< readInstructionRegister

newtype Opcode = Opcode Word8
    deriving (Bits, Enum, Eq, Integral, Num, Ord, Real, Show)

decodeAndExecute :: InstructionCode -> Redfin ()
decodeAndExecute code
    | code `at` (15, 10) == 0b000000 = halt
    | code `at` (15, 15) == 0b0      = run2 typeB register $ decodeMemoryAddress code
    | code `at` (15, 13) == 0b100    = run2 typeC register $ decodeSImm8 code
    | code `at` (15, 12) == 0b1010   = run2 typeD register $ decodeSImm8 code
    | code `at` (15, 12) == 0b1011   = run2 typeE register $ decodeUImm8 code
    | code `at` (15, 10) == 0b110011 = wait $ decodeUImm10 code -- TypeF
    | code `at` (15, 13) == 0b110    = run1 typeF $ decodeSImm10 code
    | code `at` (15, 12) == 0b1110   = run1 typeG register
    | code `at` (15, 10) == 0b111111 = run2 typeH uimm5a uimm5b
    | otherwise     = writeFlag IllegalInstruction True
  where
    run1 :: Map Opcode (a -> Redfin ()) -> a -> Redfin ()
    run1 is arg = case Map.lookup (decodeOpcode code) is of
        Just instruction -> instruction arg
        Nothing          -> writeFlag IllegalInstruction True
    run2 :: Map Opcode (a -> b -> Redfin ()) -> a -> b -> Redfin ()
    run2 is arg1 arg2 = case Map.lookup (decodeOpcode code) is of
        Just instruction -> instruction arg1 arg2
        Nothing          -> writeFlag IllegalInstruction True
    register = decodeRegister code
    (uimm5a, uimm5b) = decodeUImm5s code

at :: InstructionCode -> (Int, Int) -> Int
at code (high, low) = let bits = reverse [low..high] in
    foldr (\bit res -> res * 2 + if testBit code bit then 1 else 0) 0 bits

typeB :: Map Opcode (Register -> MemoryAddress -> Redfin ())
typeB = Map.fromList
    [ (0b000001, and  )
    , (0b000010, or   )
    , (0b000011, xor  )
    , (0b000100, add  )
    , (0b000101, sub  )
    , (0b000110, mul  )
    , (0b000111, div  )
    , (0b001000, ld   )
    , (0b001001, st   )
    , (0b001010, ldmi )
    , (0b001011, stmi )
    , (0b010001, cmpeq)
    , (0b010010, cmplt)
    , (0b010011, cmpgt)
    , (0b011100, sl   )
    , (0b011101, sr   )
    , (0b011110, sra  ) ]

typeC :: Map Opcode (Register -> SImm8 -> Redfin ())
typeC = Map.fromList
    [ (0b100000, add_si)
    , (0b100001, sub_si)
    , (0b100010, mul_si)
    , (0b100011, div_si)
    , (0b100111,  ld_si) ]

-- | TypeD instructions are currently not implemented.
typeD :: Map Opcode (Register -> SImm8 -> Redfin ())
typeD = Map.empty

typeE :: Map Opcode (Register -> UImm8 -> Redfin ())
typeE = Map.fromList
    [ (0b101100,  sl_i)
    , (0b101101,  sr_i)
    , (0b101110, sra_i)
    , (0b101111,  ld_i) ]

typeF :: Map Opcode (SImm10 -> Redfin ())
typeF = Map.fromList
    [ (0b110000, jmpi   )
    , (0b110001, jmpi_ct)
    , (0b110001, jmpi_cf) ]

typeG :: Map Opcode (Register -> Redfin ())
typeG = Map.fromList
    [ (0b111000, not) ]

-- | TypeH instruction 'pmac' is currently not implemented.
typeH :: Map Opcode (UImm5 -> UImm5 -> Redfin ())
typeH = Map.empty

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
