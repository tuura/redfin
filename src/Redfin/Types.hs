-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2017-2020
--
-- Maintainer  :  mail@geo2a.info
-- Stability   :  experimental
--
-- Domain types describing the REDFIN sequencer
--
-----------------------------------------------------------------------------
module Redfin.Types (
    -- * Data types
    Value,
    UImm8, UImm10, SImm8, SImm10,
    Register, RegisterBank,
    MemoryAddress, Memory,
    InstructionAddress, InstructionCode, Opcode, Program,
    Flag (..), Flags, flagId,
    Clock,
    State (..),

    -- * Conversion between data types
    fromSImm8,
    fromSImm10,
    fromUImm8,
    fromUImm10,
    fromSigned,
    toSigned
    ) where

import           Data.Map        (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.SBV        hiding (SFunArray)
import           GHC.TypeNats
import           Redfin.SBV

type SymbolicValue = SBV

type SymbolicArray = SFunArray

-- | The 'Value' datatype represents data values in Redfin. The precise
-- bit-width is left unspecified, but it is assumed that it fits into 64 bits.
type Value = SymbolicValue (IntN 64)

-- | The 'UImm8' datatype represents 8-bit unsigned immediate arguments that are
-- used by many Redfin instructions with immediate addressing mode.
type UImm8 = SymbolicValue (WordN 8)

-- | The 'UImm10' datatype represents 10-bit unsigned immediate arguments that
-- are used by the 'Redfin.Semantics.wait' instruction.
type UImm10 = SymbolicValue (WordN 10)

-- | The 'SImm8' datatype represents 8-bit signed immediate arguments that are
-- used by many Redfin instructions with immediate addressing mode.
type SImm8 = SymbolicValue (IntN 8)

-- | The 'SImm10' datatype represents 10-bit signed immediate arguments that are
-- used for specifying the relative jump address, e.g. in
-- 'Redfin.Semantics.jmpi' instruction.
type SImm10 = SymbolicValue (IntN 10)

-- | Redfin has 4 general-purpose registers.
type Register = SymbolicValue (WordN 2)

-- | The register bank is represented by a map from registers to their values.
type RegisterBank = SymbolicArray (WordN 2) (IntN 64)

-- | Redfin memory can hold 256 values.
type MemoryAddress = SymbolicValue (WordN 8)

-- | The memory is represented by a map from memory addresses to their values.
type Memory = SymbolicArray (WordN 8) (IntN 64)

-- | Programs are stored in program memory (currently, up to 1024 instructions).
type InstructionAddress = SymbolicValue (WordN 10)

-- | Instructions have 16-bit codes.
type InstructionCode = SymbolicValue (WordN 16)

-- | 'Opcode' is the leading 6-bit part of the 'InstructionCode', which
-- determines the instruction. The remaining 10 bits of the 'InstructionCode'
-- are used to specify immediate instruction arguments.
type Opcode = SymbolicValue (WordN 6)

-- | The program is represented by a map from instruction addresses to codes.
--   We keep the program concrete.
type Program = SymbolicArray (WordN 10) (WordN 16)

-- | Boolean 'Flag's indicate the current status of Redfin.
data Flag = Condition
          -- ^ Set by comparison instructions.
          | IllegalInstruction
          -- ^ Set by the instruction decoder, see "Redfin.Decoder".
          | Halt
          -- ^ Set by the 'Redfin.Semantics.halt' instruction, indicating
          --   that the program execution must be terminated.
          | OutOfMemory
           -- ^ Set when the memory address exceeds the size of Redfin memory
           -- and needs to be truncated, e.g. see the
           -- 'Redfin.Semantics.ldmi' instruction.
          | UninitialisedRegisterRead
           -- ^ Set when the program reads from an uninitialised register.
          | UninitialisedMemoryRead
           -- ^ Set when the program reads from an uninitialised memory location.
          | OutOfProgram
          -- ^ Set when the instruction counter goes outside program memory,
          -- e.g. after the 'Redfin.Semantics.jmpi' instruction.
          | Overflow
          -- ^ Set when arithmetic overflow occurs.
          deriving (Bounded, Enum, Eq, Ord, Show)

flagId :: Flag -> SymbolicValue (WordN 4)
flagId = literal . fromIntegral . fromEnum

-- | The state of flags is represented by a map from flags to their values.
type Flags = SymbolicArray (WordN 4) Bool

-- | 'Clock' is the current time measured in clock cycles. It used to model the
-- effect of the 'Redfin.Semantics.wait' instruction.
type Clock = SymbolicValue (WordN 64)

-- | The 'State' of Redfin is fully characterised by the contents of the register
-- bank, instruction counter, flags, memory and program. The latter is assumed
-- to be unchanged throughout the program execution, however, it may technically
-- be possible to write to it, hence we make it part of the state to have a
-- faithful model.
data State = State
    { registers           :: RegisterBank
    , instructionCounter  :: InstructionAddress
    , instructionRegister :: InstructionCode
    , flags               :: Flags
    , memory              :: Memory
    , program             :: Program
    , clock               :: Clock
    } deriving Show

instance Mergeable State where
    symbolicMerge f t (State rs1 ic1 ir1 fs1 m1 p1 c1)
                      (State rs2 ic2 ir2 fs2 m2 p2 c2) =
                       State rs  ic  ir  fs  m  p  c
      where
         rs = symbolicMerge f t rs1 rs2
         ic = symbolicMerge f t  ic1 ic2
         ir = symbolicMerge f t ir1 ir2
         fs = symbolicMerge f t fs1 fs2
         m  = symbolicMerge f t m1 m2
         p  = symbolicMerge f t p1 p2
         c  = symbolicMerge f t c1 c2


--------------------------------------------------------------------------------
fromSImm8 :: SImm8 -> Value
fromSImm8 = sFromIntegral

fromSImm10 :: SImm10 -> InstructionAddress
fromSImm10 = sFromIntegral

fromUImm8 :: UImm8 -> Value
fromUImm8 = sFromIntegral

fromUImm10 :: UImm10 -> Value
fromUImm10 = sFromIntegral

-- | Convert a two's complement signed number into an unsigned word
--   by "forgetting" that it's signed
fromSigned :: (KnownNat n, IsNonZero n) => SBV (IntN n) -> SBV (WordN n)
fromSigned = sFromIntegral

-- | Interpret an unsigned word into a two's complement signed number
toSigned :: (KnownNat n, IsNonZero n) => SBV (WordN n) -> SBV (IntN n)
toSigned = sFromIntegral
