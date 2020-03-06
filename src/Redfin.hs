{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2017-2020
--
-- Maintainer  :  mail@geo2a.info
-- Stability   :  experimental
--
-- REDFIN sequencer (verification backend).
--
-----------------------------------------------------------------------------
module Redfin (
    -- * Data types
    Value,
    UImm5, UImm8, UImm10, SImm8, SImm10,
    Register, RegisterBank,
    MemoryAddress, Memory,
    InstructionAddress, InstructionCode, Opcode, Program,
    Flag (..), Flags, flagId,
    Clock,
    State (..),

    -- * Conversion between data types
    toMemoryAddress,

    -- * Redfin state transformer
    Redfin (..), transformState, readState, writeState,
    readRegister, writeRegister,
    readMemory, writeMemory,
    readFlag, writeFlag,
    readProgram, readInstructionRegister, writeInstructionRegister,
    fetchInstruction, incrementInstructionCounter,
    delay
    ) where

import Control.Monad
import Data.SBV

-- | The 'Value' datatype represents data values in Redfin. The precise
-- bit-width is left unspecified, but it is assumed that it fits into 64 bits.
type Value = SInt64

-- | The 'UImm5' datatype represents 5-bit unsigned immediate arguments that are
-- used by the 'Redfin.Semantics.pmac' instruction.
type UImm5 = SWord8

-- | The 'UImm8' datatype represents 8-bit unsigned immediate arguments that are
-- used by many Redfin instructions with immediate addressing mode.
type UImm8 = SWord8

-- | The 'UImm10' datatype represents 10-bit unsigned immediate arguments that
-- are used by the 'Redfin.Semantics.wait' instruction.
type UImm10 = SWord16

-- | The 'SImm8' datatype represents 8-bit signed immediate arguments that are
-- used by many Redfin instructions with immediate addressing mode.
type SImm8 = SInt8

-- | The 'SImm10' datatype represents 10-bit signed immediate arguments that are
-- used for specifying the relative jump address, e.g. in
-- 'Redfin.Semantics.jmpi' instruction.
type SImm10 = SInt16

-- | Redfin has 4 general-purpose registers.
type Register = SWord8

-- | The register bank is represented by a map from registers to their values.
type RegisterBank = SFunArray Word8 Int64

-- | Redfin memory can hold 256 values.
type MemoryAddress = SWord8

-- | The memory is represented by a map from memory addresses to their values.
type Memory = SFunArray Word8 Int64

-- | Programs are stored in program memory (currently, up to 1024 instructions).
type InstructionAddress = SWord16

-- | Instructions have 16-bit codes.
type InstructionCode = SWord16

-- | 'Opcode' is the leading 6-bit part of the 'InstructionCode', which
-- determines the instruction. The remaining 10 bits of the 'InstructionCode'
-- are used to specify immediate instruction arguments.
type Opcode = SWord8

-- | The program is represented by a map from instruction addresses to codes.
type Program = SFunArray Word16 Word16

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

flagId :: Flag -> SWord8
flagId = literal . fromIntegral . fromEnum

-- | The state of flags is represented by a map from flags to their values.
type Flags = SFunArray Word8 Bool

-- | 'Clock' is the current time measured in clock cycles. It used to model the
-- effect of the 'Redfin.Semantics.wait' instruction.
type Clock = SWord64

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
         ic = symbolicMerge f t ic1 ic2
         ir = symbolicMerge f t ir1 ir2
         fs = symbolicMerge f t fs1 fs2
         m  = symbolicMerge f t m1 m2
         p  = symbolicMerge f t p1 p2
         c  = symbolicMerge f t c1 c2

-- | The Redfin state transformer.
data Redfin a = Redfin { redfin :: (State -> (a, State)) } deriving Functor

-- | A standard 'Applicative' instance available for any 'Monad'.
instance Applicative Redfin where
    pure  = return
    (<*>) = ap

-- | A standard state 'Monad'.
instance Monad Redfin where
    return a       = Redfin $ \s -> (a, s)
    Redfin r >>= f = Redfin $ \s -> let (a, s') = r s in redfin (f a) s'

-- | Read the current 'State'.
readState :: Redfin State
readState = Redfin $ \s -> (s, s)

-- | Change the current 'State'.
writeState :: State -> Redfin ()
writeState s = Redfin $ \_ -> ((), s)

-- | Transform the current 'State' by applying a given transformation function.
transformState :: (State -> State) -> Redfin ()
transformState f = Redfin $ \s -> ((), f s)

-- | Advance the clock by a given number of clock cycles.
delay :: Clock -> Redfin ()
delay cycles = transformState $ \(State rs ic ir fs m p  c         )
                               -> State rs ic ir fs m p (c + cycles)

-- | Lookup the 'Value' in a given 'Register'. If the register has never been
-- initialised, this function returns 0, which is how the current hardware
-- implementation works. To handle more general settings, it may also be useful
-- to raise an error flag in this situation (future work).
readRegister :: Register -> Redfin Value
readRegister register = do
    state <- readState
    return $ readArray (registers state) register

-- | Write a new 'Value' to a given 'Register'.
writeRegister :: Register -> Value -> Redfin ()
writeRegister register value =
    transformState $ \(State             rs                 ic ir fs m p c)
                    -> State (writeArray rs register value) ic ir fs m p c

-- | Lookup the 'Value' at the given 'MemoryAddress'. If the value has never been
-- initialised, this function returns 0, which is how the current hardware
-- implementation works. To handle more general settings, it may also be useful
-- to raise an error flag in this situation (future work). We assume that it
-- takes 1 clock cycle to access the memory in hardware.
readMemory :: MemoryAddress -> Redfin Value
readMemory address = do
    state <- readState
    delay 1
    return $ readArray (memory state) address

-- | Write a new 'Value' to the given 'MemoryAddress'. We assume that it takes 1
-- clock cycle to access the memory in hardware.
writeMemory :: MemoryAddress -> Value -> Redfin ()
writeMemory address value = do
    delay 1
    transformState $ \(State rs ic ir fs             m                p c)
                    -> State rs ic ir fs (writeArray m address value) p c

-- | Convert a 'Value' to the symbolic memory address. If the value needs to be
-- truncated, the 'OutOfMemory' flag is set.
toMemoryAddress :: Value -> Redfin SWord8
toMemoryAddress value = do
    let valid = value .< 256
    transformState $ \s -> ite valid s (snd $ redfin (writeFlag OutOfMemory sTrue) s)
    return $ fromBitsLE (take 8 $ blastLE value)

-- | Lookup the value of a given 'Flag'. If the flag is not currently assigned
-- any value, it is assumed to be 'False'.
readFlag :: Flag -> Redfin SBool
readFlag flag = do
    state <- readState
    return $ readArray (flags state) (flagId flag)

-- | Set a given 'Flag' to the specified Boolean value.
writeFlag :: Flag -> SBool -> Redfin ()
writeFlag flag value =
    transformState $ \(State rs ic ir             fs                      m p c)
                    -> State rs ic ir (writeArray fs (flagId flag) value) m p c

-- | Lookup the 'InstructionCode' at the given 'InstructionAddress'. If the
-- program has no code associated with the address, the function returns 0 and
-- raises the 'OutOfProgram' error flag. We assume that it takes 1 clock cycle
-- to access the program memory in hardware.
readProgram :: InstructionAddress -> Redfin InstructionCode
readProgram address = do
    state <- readState
    delay 1
    return $ readArray (program state) address

-- | Fetch the instruction code pointed to by the instruction counter and store
-- it in the instruction register. We assume that instruction fetch takes one
-- clock cycle.
fetchInstruction :: Redfin ()
fetchInstruction = do
    state <- readState
    writeInstructionRegister =<< readProgram (instructionCounter state)

-- | Increment the instruction counter.
incrementInstructionCounter :: Redfin ()
incrementInstructionCounter = transformState $ \(State rs  ic      ir fs m p c)
                                              -> State rs (ic + 1) ir fs m p c

-- | Read the instruction register.
readInstructionRegister :: Redfin InstructionCode
readInstructionRegister = instructionRegister <$> readState

-- | Write a given 'InstructionCode' to the instruction register.
writeInstructionRegister :: InstructionCode -> Redfin ()
writeInstructionRegister instructionCode =
    transformState $ \(State rs ic _               fs m p c)
                    -> State rs ic instructionCode fs m p c
