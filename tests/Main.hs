{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           Control.Arrow   (second)
import           Data.Proxy
import           Data.SBV
import qualified Data.SBV.List   as SBV
import           GHC.TypeNats


import qualified Redfin.Assembly as Assembler
import qualified Redfin.Decode   as Decoder
import qualified Redfin.Types    as Types

main :: IO ()
main = do
  xs <- mapM (prove . snd) theorems
  mapM_ print $ zip (map fst theorems) xs
  -- mapM_ print xs

theorems :: [(String, Symbolic SBool)]
theorems =
  [ ("SImm8 converts to Value correctly", fromSImm8Correct)
  , ("SImm10 converts to InstructionAddress correctly", fromSImm10Correct)
  , ("UImm8 converts to Value correctly", fromUImm8Correct)
  , ("UImm10 converts to Value correctly", fromUImm10Correct)
  , ("Arithmetic instructions are decoded correctly", decodeArithmCorrect)
  ]

{-------------------------------------
  Correctness of bitvector conversions
--------------------------------------}

fromSImm8Correct :: Symbolic SBool
fromSImm8Correct = do
  x <- forall "x"
  pure $ Types.fromSImm8 x `inRange` ( minBound :: Types.Value
                                     , maxBound :: Types.Value)
fromSImm10Correct :: Symbolic SBool
fromSImm10Correct = do
  x <- forall "x"
  pure $ Types.fromSImm10 x `inRange` ( minBound :: Types.InstructionAddress
                                      , maxBound :: Types.InstructionAddress)

fromUImm8Correct :: Symbolic SBool
fromUImm8Correct = do
  x <- forall "x"
  let y = Types.fromUImm8 (x :: Types.UImm8)
  pure $ y `inRange` (minBound :: Types.Value, maxBound :: Types.Value)

fromUImm10Correct :: Symbolic SBool
fromUImm10Correct = do
  x <- forall "x"
  let y = Types.fromUImm10 (x :: Types.UImm10)
  pure $ y `inRange` (minBound :: Types.Value, maxBound :: Types.Value)

fromSignedCorrect :: Symbolic SBool
fromSignedCorrect = do
  x <- forall "x"
  let y = Types.fromSigned (x :: Types.SImm8)
  pure $ Types.toSigned y .== x

{-------------------------------------
  Correctness of instruction decoding
--------------------------------------}

-- | Prove that for every arithmetic opcode, register and memory address,
--   the decoded opcode matches the initial one
decodeArithmCorrect :: Symbolic SBool
decodeArithmCorrect = do
  (op :: Types.Opcode)       <- forall "op"
  (r :: Types.Register)      <- forall "r"
  (a :: Types.MemoryAddress) <- forall "a"
  constrain $ op `SBV.elem` arithm
  let i = op # r # a
  pure $ -- "Opcode decoded correctly"
         Decoder.decodeOpcode i .== op
     .&& -- "Register decoded correctly"
         Decoder.decodeRegister i .== r
     .&& -- "Address decoded correctly"
         Decoder.decodeMemoryAddress i .== a
  where
    arithm :: SList (WordN 6)
    arithm = [0b000100, 0b000101, 0b000110, 0b000111]

-- | Prove that for every arithmetic with immediate argument opcode,
-- register and memory address, the decoded opcode matches the initial one
decodeArithmImmCorrect :: Symbolic SBool
decodeArithmImmCorrect = do
  (op :: Types.Opcode)  <- forall "op"
  (r :: Types.Register) <- forall "r"
  (imm :: Types.SImm8)  <- forall "imm"
  constrain $ op `SBV.elem` arithmImm
  let i = op # r # Types.fromSigned imm
  pure $ -- "Opcode decoded correctly"
         Decoder.decodeOpcode i .== op
     .&& -- "Register decoded correctly"
         Decoder.decodeRegister i .== r
     .&& -- "Immediate argument decoded correctly"
         Decoder.decodeSImm8 i .== imm
  where
    arithmImm :: SList (WordN 6)
    arithmImm = [0b100000, 0b100001, 0b100010, 0b100011]

decodeLoadSICorrect :: Symbolic SBool
decodeLoadSICorrect = do
  (r :: Types.Register) <- forall "r"
  (imm :: Types.SImm8)  <- forall "imm"
  let op = 0b100111
  let i = op # r # Types.fromSigned imm
  pure $ Decoder.decodeOpcode i .== op
     .&& -- "Register decoded correctly"
         Decoder.decodeRegister i .== r
     .&& -- "Immediate argument decoded correctly"
         Decoder.decodeSImm8 i .== imm
-----------------------------------------------------------------------------
