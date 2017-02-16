-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Verification.Decode
-- Copyright   :  (c) Andrey Mokhov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- REDFIN instruction set.
--
-----------------------------------------------------------------------------
module Redfin.Verification.Decode (executeInstruction) where

import Control.Applicative
import Data.List
import Data.SBV

import Redfin.Verification
import qualified Redfin.Verification.Assembly  as A
import qualified Redfin.Verification.Semantics as S

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
decodeAndExecute code = case decode code of
    Just f  -> transformState f
    Nothing -> writeFlag IllegalInstruction true

decode :: InstructionCode -> Maybe (State -> State)
decode code = transformA
          <|> transformB
          <|> transformC
          <|> transformE
          <|> transformFS
          <|> transformFU
          <|> transformG
  where
    opcode     = decodeOpcode code
    register   = decodeRegister code
    address    = decodeMemoryAddress code
    simm8      = decodeSImm8 code
    uimm8      = decodeUImm8 code
    simm10     = decodeSImm10 code
    uimm10     = decodeUImm10 code
    transformA = fmap (\s t -> snd $ redfin s t) semanticsA
    semanticsA = snd <$> find (\(a, _) -> A.topOpcode a == opcode)
        [(A.halt, S.halt)]
    transformB = fmap (\s t -> snd $ redfin (s register address) t) semanticsB
    semanticsB = snd <$> find (\(a, _) -> A.topOpcode (a 0 0) == opcode)
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
    transformC = fmap (\s t -> snd $ redfin (s register simm8) t) semanticsC
    semanticsC = snd <$> find (\(a, _) -> A.topOpcode (a 0 0) == opcode)
        [ (A.add_si, S.add_si)
        , (A.sub_si, S.sub_si)
        , (A.mul_si, S.mul_si)
        , (A.div_si, S.div_si)
        , (A.ld_si , S.ld_si ) ]
    transformE = fmap (\s t -> snd $ redfin (s register uimm8) t) semanticsE
    semanticsE = snd <$> find (\(a, _) -> A.topOpcode (a 0 0) == opcode)
        [ (A.sl_i , S.sl_i )
        , (A.sr_i , S.sr_i )
        , (A.sra_i, S.sra_i)
        , (A.ld_i , S.ld_i ) ]
    transformFS = fmap (\s t -> snd $ redfin (s simm10) t) semanticsFS
    semanticsFS = snd <$> find (\(a, _) -> A.topOpcode (a 0) == opcode)
        [ (A.jmpi   , S.jmpi   )
        , (A.jmpi_ct, S.jmpi_ct)
        , (A.jmpi_cf, S.jmpi_cf) ]
    transformFU = fmap (\s t -> snd $ redfin (s uimm10) t) semanticsFU
    semanticsFU = snd <$> find (\(a, _) -> A.topOpcode (a 0) == opcode)
        [ (A.wait, S.wait) ]
    transformG = fmap (\s t -> snd $ redfin (s register) t) semanticsG
    semanticsG = snd <$> find (\(a, _) -> A.topOpcode (a 0) == opcode)
        [ (A.not, S.not) ]

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
