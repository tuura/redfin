{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Listing
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2018
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- Pretty-print REDFIN assembly programs.
--
-----------------------------------------------------------------------------
module Redfin.Listing (
    showInstructionCode, showScript, prettyPrintScript
) where

import qualified Data.SBV           as SBV
import qualified Data.Text.Lazy     as T
import           Redfin.Assembly
import           Redfin.Decode
import           Redfin.Types
import           Text.Pretty.Simple (pPrint)

-- | Pretty-print a 'Script' to stdout.
prettyPrintScript :: Script -> IO ()
prettyPrintScript = pPrint . showScript

-- | Pretty-print a 'Script' to 'Text'.
showScript :: Script -> T.Text
showScript src =
    T.replace " :: SInt8"  "" .
    T.replace " :: SInt 8"  "" .
    T.replace " :: SInt 10"  "" .
    T.replace " :: SInt 16"  "" .
    T.replace " :: SWord 2" "" .
    T.replace " :: SWord 4" "" .
    T.replace " :: SWord 8" "" .
    T.replace " :: SWord8" "" .
    T.replace " :: SWord 10" "" .
    T.replace " :: SWord 16" "" .
    T.pack . unlines $
    map (showInstructionCode . snd) $ machineCode src

showInstructionCode :: InstructionCode -> String
showInstructionCode code =
    let opcode       = decodeOpcode code
        register     = decodeRegister code
        address      = decodeMemoryAddress code
        simm8        = decodeSImm8 code
        uimm8        = decodeUImm8 code
        simm10       = decodeSImm10 code
        uimm10       = decodeUImm10 code
    in case SBV.unliteral opcode of
         Just 0b000001 -> "and  "  ++ show register ++ " " ++ show address
         Just 0b000010 -> "or "    ++ show register ++ " " ++ show address
         Just 0b000011 -> "xor "   ++ show register ++ " " ++ show address
         Just 0b000100 -> "add "   ++ show register ++ " " ++ show address
         Just 0b000101 -> "sub "   ++ show register ++ " " ++ show address
         Just 0b000110 -> "mul "   ++ show register ++ " " ++ show address
         Just 0b000111 -> "div "   ++ show register ++ " " ++ show address
         Just 0b001000 -> "ld "    ++ show register ++ " " ++ show address
         Just 0b001001 -> "st "    ++ show register ++ " " ++ show address
         Just 0b001010 -> "ldmi "  ++ show register ++ " " ++ show address
         Just 0b001011 -> "stmi "  ++ show register ++ " " ++ show address
         Just 0b010001 -> "cmpeq " ++ show register ++ " " ++ show address
         Just 0b010010 -> "cmplt " ++ show register ++ " " ++ show address
         Just 0b010011 -> "cmpgt " ++ show register ++ " " ++ show address
         Just 0b011100 -> "sl "    ++ show register ++ " " ++ show address
         Just 0b011101 -> "sr "    ++ show register ++ " " ++ show address
         Just 0b011110 -> "sra "   ++ show register ++ " " ++ show address
         Just 0b001100 -> "fadd " ++ show register ++ " " ++ show address
         Just 0b001101 -> "fsub " ++ show register ++ " " ++ show address
         Just 0b001110 -> "fmul " ++ show register ++ " " ++ show address
         Just 0b001111 -> "fdiv " ++ show register ++ " " ++ show address
         Just 0b100000 -> "add_si " ++ show register ++ " " ++ show simm8
         Just 0b100001 -> "sub_si " ++ show register ++ " " ++ show simm8
         Just 0b100010 -> "mul_si " ++ show register ++ " " ++ show simm8
         Just 0b100011 -> "div_si " ++ show register ++ " " ++ show simm8
         Just 0b100111 -> "ld_si " ++ show register ++ " " ++ show simm8
         Just 0b101100 -> "sl_i " ++show register ++ " " ++ show uimm8
         Just 0b101101 -> "sr_i " ++show register ++ " " ++ show uimm8
         Just 0b101110 -> "sra_i " ++show register ++ " " ++ show uimm8
         Just 0b101111 -> "ld_i " ++show register ++ " " ++ show uimm8
         Just 0b110000 -> "jmpi " ++ show simm10
         Just 0b110001 -> "jmpi_ct " ++ show simm10
         Just 0b110010 -> "jmpi_cf " ++ show simm10
         Just 0b110011 -> "wait " ++ show uimm10
         Just 0b111000 -> "not " ++ show register
         Just 0b111001 -> "abs " ++ show register
         Just 0b000000 -> "halt "
         Just _        -> "illegal instruction"
         Nothing       -> show code
