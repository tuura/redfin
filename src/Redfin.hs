{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin
-- Copyright   :  (c) Andrey Mokhov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- REDFIN sequencer.
--
-----------------------------------------------------------------------------
module Redfin (
    -- * Data types
    Value (..),
    UImm5 (..), UImm8 (..), UImm10 (..), SImm8 (..), SImm10 (..),
    Register (..), RegisterBank,
    MemoryAddress (..), Memory,
    InstructionAddress (..), InstructionCode (..), Program,
    Flag (..), Flags,
    Clock (..),
    State (..),

    -- * Conversion between data types
    UnsignedValue (..), SignedValue (..), toMemoryAddress,

    -- * Redfin state transformer
    Redfin (..), transformState, readState, writeState,
    readRegister, writeRegister,
    readMemory, writeMemory,
    readFlag, writeFlag,
    readProgram, readInstructionRegister, writeInstructionRegister,
    fetchInstruction, incrementInstructionCounter,
    delay,
    ) where

import Control.Monad
import Data.Bits
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Word

-- | The 'Value' datatype represents data values in Redfin. The precise
-- bit-width is left unspecified, but it is assumed that it fits into 64 bits.
newtype Value = Value Int64
    deriving (Bits, Enum, Eq, Integral, Num, Ord, Real, Show)

-- | The 'UImm5' datatype represents 5-bit unsigned immediate arguments that are
-- used by the 'Redfin.InstructionSet.pmac' instruction.
newtype UImm5 = UImm5 Word8 deriving (Eq, Num, Ord, Show)

-- | The 'UImm8' datatype represents 8-bit unsigned immediate arguments that are
-- used by many Redfin instructions with immediate addressing mode.
newtype UImm8 = UImm8 Word8 deriving (Eq, Num, Ord, Show)

-- | The 'UImm10' datatype represents 10-bit unsigned immediate arguments that
-- are used by the 'Redfin.InstructionSet.wait' instruction.
newtype UImm10 = UImm10 Word16 deriving (Eq, Num, Ord, Show)

-- | Extend an unsigned integer to 'Value'. The latter must be wide enough to
-- represent the given integer with no loss of information.
class UnsignedValue a where
    unsignedValue :: a -> Value

instance UnsignedValue UImm5  where unsignedValue (UImm5  u) = fromIntegral u
instance UnsignedValue UImm8  where unsignedValue (UImm8  u) = fromIntegral u
instance UnsignedValue UImm10 where unsignedValue (UImm10 u) = fromIntegral u

-- | The 'SImm8' datatype represents 8-bit signed immediate arguments that are
-- used by many Redfin instructions with immediate addressing mode.
newtype SImm8 = SImm8 Word8 deriving (Eq, Num, Ord, Show)

-- | The 'SImm10' datatype represents 10-bit signed immediate arguments that are
-- used for specifying the relative jump address, e.g. in
-- 'Redfin.InstructionSet.jmpi' instruction.
newtype SImm10 = SImm10 Word16 deriving (Eq, Num, Ord, Show)

-- | Extend a signed integer to 'Value' applying sign extension. 'Value' must be
-- wide enough to represent the given integer with no loss of information.
class SignedValue a where
    signedValue :: a -> Value

instance SignedValue SImm8  where signedValue (SImm8  s) = fromIntegral s
instance SignedValue SImm10 where signedValue (SImm10 s) = fromIntegral s

-- | Redfin has 4 general-purpose registers 'R0' - 'R3'.
data Register = R0 | R1 | R2 | R3 deriving (Eq, Ord, Show)

-- | The register bank is represented by a map from registers to their values.
type RegisterBank = Map Register Value

-- | Redfin memory can hold 256 values.
newtype MemoryAddress = MemoryAddress Word8 deriving (Eq, Num, Ord, Show)

-- | The memory is represented by a map from memory addresses to their values.
type Memory = Map MemoryAddress Value

-- | Programs are stored in program memory (currently, up to 1024 instructions).
newtype InstructionAddress = InstructionAddress Word16
    deriving (Eq, Num, Ord, Show)

-- | Instructions have 16-bit codes.
newtype InstructionCode = InstructionCode Word16
    deriving (Bits, Enum, Eq, Integral, Num, Ord, Real, Show)

-- | The program is represented by a map from instruction addresses to codes.
type Program = Map InstructionAddress InstructionCode

-- | Boolean 'Flag's indicate the current status of Redfin.
data Flag = Condition
          -- ^ Set by comparison instructions.
          | IllegalInstruction
          -- ^ Set by the instruction decoder, see "Redfin.Decoder".
          | Halt
          -- ^ Set by the 'Redfin.InstructionSet.halt' instruction, indicating
          --   that the program execution must be terminated.
          | OutOfMemory
           -- ^ Set when the memory address exceeds the size of Redfin memory
           -- and needs to be truncated, e.g. see the
           -- 'Redfin.InstructionSet.ldmi' instruction.
          | OutOfProgram
          -- ^ Set when the instruction counter goes outside program memory,
          -- e.g. after the 'Redfin.InstructionSet.jmpi' instruction.
          | Overflow
          -- ^ Set when arithmetic overflow occurs.
          deriving (Eq, Ord, Show)

-- | The state of flags is represented by a map from flags to their values.
type Flags = Map Flag Bool

-- | 'Clock' is the current time measured in clock cycles. It used to model the
-- effect of the 'Redfin.InstructionSet.wait' instruction.
newtype Clock = Clock Int64 deriving (Eq, Num, Show)

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
    , clock               :: Clock }

-- | The Redfin state transformer.
data Redfin a = Redfin { execute :: (State -> (a, State)) } deriving Functor

-- | A standard 'Applicative' instance available for any 'Monad'.
instance Applicative Redfin where
    pure  = return
    (<*>) = ap

-- | A standard state 'Monad'.
instance Monad Redfin where
    return a       = Redfin $ \s -> (a, s)
    Redfin r >>= f = Redfin $ \s -> let (a, s') = r s in execute (f a) s'

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
    return $ Map.findWithDefault 0 register (registers state)

-- | Write a new 'Value' to a given 'Register'.
writeRegister :: Register -> Value -> Redfin ()
writeRegister register value =
    transformState $ \(State                            rs  ic ir fs m p c)
                    -> State (Map.insert register value rs) ic ir fs m p c

-- | Lookup the 'Value' at the given 'MemoryAddress'. If the value has never been
-- initialised, this function returns 0, which is how the current hardware
-- implementation works. To handle more general settings, it may also be useful
-- to raise an error flag in this situation (future work). We assume that it
-- takes 1 clock cycle to access the memory in hardware.
readMemory :: MemoryAddress -> Redfin Value
readMemory address = do
    state <- readState
    delay 1
    return $ Map.findWithDefault 0 address (memory state)

-- | Write a new 'Value' to the given 'MemoryAddress'. We assume that it takes 1
-- clock cycle to access the memory in hardware.
writeMemory :: MemoryAddress -> Value -> Redfin ()
writeMemory address value = do
    delay 1
    transformState $ \(State rs ic ir fs                           m  p c)
                    -> State rs ic ir fs (Map.insert address value m) p c

-- | Convert a 'Value' to the 'MemoryAddress'. If the value needs to be
-- truncated, the 'OutOfMemory' flag is set.
toMemoryAddress :: Value -> Redfin MemoryAddress
toMemoryAddress value
    | value < 256 = return $ fromIntegral value
    | otherwise   = do
        writeFlag OutOfMemory True
        return $ fromIntegral (value .&. 255)

-- | Lookup the value of a given 'Flag'. If the flag is not currently assigned
-- any value, it is assumed to be 'False'.
readFlag :: Flag -> Redfin Bool
readFlag flag = do
    state <- readState
    return $ Map.findWithDefault False flag (flags state)

-- | Set a given 'Flag' to the specified Boolean value.
writeFlag :: Flag -> Bool -> Redfin ()
writeFlag flag value =
    transformState $ \(State rs ic ir                        fs  m p c)
                    -> State rs ic ir (Map.insert flag value fs) m p c

-- | Lookup the 'InstructionCode' at the given 'InstructionAddress'. If the
-- program has no code associated with the address, the function returns 0 and
-- raises the 'OutOfProgram' error flag. We assume that it takes 1 clock cycle
-- to access the program memory in hardware.
readProgram :: InstructionAddress -> Redfin InstructionCode
readProgram address = do
    state <- readState
    delay 1
    case Map.lookup address (program state) of
        Just code -> return code
        Nothing -> do
            writeFlag OutOfProgram True
            return 0

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

