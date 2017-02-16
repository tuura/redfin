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

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Prelude hiding (and, div, not, or)

import Redfin
import Redfin.InstructionSet
import Redfin.Semantics

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
decodeAndExecute code
    | code `at` (15, 10) == 0b000000 = halt
    | code `at` (15, 15) == 0b0      = run2 typeB register $ decodeMemoryAddress code
    | code `at` (15, 13) == 0b100    = run2 typeC register $ decodeSImm8 code
    | code `at` (15, 12) == 0b1010   = run2 typeD register $ decodeSImm8 code
    | code `at` (15, 12) == 0b1011   = run2 typeE register $ decodeUImm8 code
    | code `at` (15, 10) == 0b110011 = run1 typeFU $ decodeUImm10 code
    | code `at` (15, 13) == 0b110    = run1 typeFS $ decodeSImm10 code
    | code `at` (15, 12) == 0b1110   = run1 typeG register
    | code `at` (15, 10) == 0b111111 = run2 typeH uimm5a uimm5b
    | otherwise                      = writeFlag IllegalInstruction True
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
