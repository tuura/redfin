{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin
-- Copyright   :  (c) Andrey Mokhov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- REDFIN sequencer and supported instructions.
--
-----------------------------------------------------------------------------
module Redfin where

import Control.Monad
import qualified Data.Bits as Std
import Data.Bits hiding (xor, shiftL, shiftR)
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Prelude hiding (div, not)
import qualified Prelude as Std
import Data.Word

-- | The 'Value' datatype represents data values in Redfin. The precise
-- bit-width is left unspecified, but it is assumed that it fits into 64 bits.
newtype Value = Value Int64
    deriving (Bits, Enum, Eq, Integral, Num, Ord, Real, Show)

-- | The 'UImm5' datatype represents 5-bit unsigned immediate arguments that are
-- used by the 'pmac' instruction.
newtype UImm5 = UImm5 Word8 deriving (Eq, Num, Ord, Show)

-- | The 'UImm8' datatype represents 8-bit unsigned immediate arguments that are
-- used by many Redfin instructions with immediate addressing mode.
newtype UImm8 = UImm8 Word8 deriving (Eq, Num, Ord, Show)

-- | The 'UImm10' datatype represents 10-bit unsigned immediate arguments that
-- are used by the 'wait' instruction.
newtype UImm10 = UImm10 Word16 deriving (Eq, Num, Ord, Show)

-- | Extend an unsigned integer to 'Value'. The latter must be wide enough to
-- represent the given integer with no loss of information.
class UnsignedValue a where
    unsignedValue :: a -> Value

instance UnsignedValue UImm5  where unsignedValue (UImm5  x) = fromIntegral x
instance UnsignedValue UImm8  where unsignedValue (UImm8  x) = fromIntegral x
instance UnsignedValue UImm10 where unsignedValue (UImm10 x) = fromIntegral x

-- | The 'SImm8' datatype represents 8-bit signed immediate arguments that are
-- used by many Redfin instructions with immediate addressing mode.
newtype SImm8 = SImm8 Word8 deriving (Eq, Num, Ord, Show)

-- | The 'SImm10' datatype represents 10-bit signed immediate arguments that are
-- used for specifying the relative jump address, e.g. in 'jmpi' instruction.
newtype SImm10 = SImm10 Word16 deriving (Eq, Num, Ord, Show)

-- | Extend a signed integer to 'Value' applying sign extension. 'Value' must be
-- wide enough to represent the given integer with no loss of information.
class SignedValue a where
    signedValue :: a -> Value

instance SignedValue SImm8  where signedValue (SImm8  x) = fromIntegral x
instance SignedValue SImm10 where signedValue (SImm10 x) = fromIntegral x

-- | Redfin has 4 general-purpose registers R0-R3.
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
newtype InstructionCode = InstructionCode Word16 deriving (Eq, Num, Show)

-- | The program is represented by a map from instruction addresses to codes.
type Program = Map InstructionAddress InstructionCode

-- | Boolean 'Flag's indicate the current status of Redfin.
data Flag = Condition    -- ^ Set by comparison instructions.
          | Overflow     -- ^ Set when arithmetic overflow occurs.
          | OutOfProgram -- ^ Set when the instruction counter goes outside program memory.
          deriving (Eq, Ord, Show)

-- | The state of flags is represented by a map from flags to their values.
type Flags = Map Flag Bool

-- | The 'State' of Redfin is fully characterised by the contents of the register
-- bank, instruction counter, flags, memory and program. The latter is assumed
-- to be unchanged throughout the program execution, however, it may technically
-- be possible to write to it, hence we make it part of the state to have a
-- faithful model.
data State = State
    { registers          :: RegisterBank
    , instructionCounter :: InstructionAddress
    , flags              :: Flags
    , memory             :: Memory
    , program            :: Program }

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
writeRegister register value = transformState $
    \(State rs ic fs m p) -> State (Map.insert register value rs) ic fs m p

-- | Lookup the 'Value' at the given 'MemoryAddress'. If the value has never been
-- initialised, this function returns 0, which is how the current hardware
-- implementation works. To handle more general settings, it may also be useful
-- to raise an error flag in this situation (future work).
readMemory :: MemoryAddress -> Redfin Value
readMemory address = do
    state <- readState
    return $ Map.findWithDefault 0 address (memory state)

-- | Write a new 'Value' to the given 'MemoryAddress'.
writeMemory :: MemoryAddress -> Value -> Redfin ()
writeMemory address value = transformState $
    \(State rs ic fs m p) -> State rs ic fs (Map.insert address value m) p

-- | Lookup the value of a given 'Flag'. Flags are initialised to 'False'.
readFlag :: Flag -> Redfin Bool
readFlag flag = do
    state <- readState
    return $ Map.findWithDefault False flag (flags state)

-- | Set a given 'Flag' to the specified Boolean value.
writeFlag :: Flag -> Bool -> Redfin ()
writeFlag flag value = transformState $
    \(State rs ic fs m p) -> State rs ic (Map.insert flag value fs) m p

-- | Lookup the 'InstructionCode' at the given 'InstructionAddress'. If the
-- program has no code associated with the address, the function returns 0 and
-- raises the 'OutOfProgram' error flag.
readProgram :: InstructionAddress -> Redfin InstructionCode
readProgram address = do
    state <- readState
    case Map.lookup address (program state) of
        Just code -> return code
        Nothing -> do
            writeFlag OutOfProgram True
            return 0

-- | Fetch the instruction code pointed to by the instruction counter.
fetchInstruction :: Redfin InstructionCode
fetchInstruction = do
    state <- readState
    readProgram $ instructionCounter state

-- | Increment the instruction counter.
incrementInstructionCounter :: Redfin ()
incrementInstructionCounter = transformState $
    \(State rs ic fs m p) -> State rs (ic + 1) fs m p

-- | A convenient combinator for defining instructions that fit the pattern
-- @res = arg1 op arg2@, e.g. addition is @rX = rX + [dmemaddr]@.
(<~) :: (c -> Redfin ()) -> (Redfin a, a -> b -> c, Redfin b) -> Redfin ()
(<~) res (arg1, op, arg2) = do
    x <- arg1
    y <- arg2
    res $ x `op` y

-- TODO: Set Overflow flag.
-- TODO: Implement fixed-point instructions.

-- | Instruction @add rX, dmemaddr@ is implemented as @rX = rX + [dmemaddr]@.
add :: Register -> MemoryAddress -> Redfin ()
add rX dmemaddr = writeRegister rX <~ (readRegister rX, (+), readMemory dmemaddr)

-- | Instruction @add_si rX, simm@ is implemented as @rX = rX + simm@.
add_si :: Register -> SImm8 -> Redfin ()
add_si rX simm = writeRegister rX <~ (readRegister rX, (+), pure $ signedValue simm)

-- | Instruction @sub rX, dmemaddr@ is implemented as @rX = rX - [dmemaddr]@.
sub :: Register -> MemoryAddress -> Redfin ()
sub rX dmemaddr = writeRegister rX <~ (readRegister rX, (-), readMemory dmemaddr)

-- | Instruction @sub_si rX, simm@ is implemented as @rX = rX - simm@.
sub_si :: Register -> SImm8 -> Redfin ()
sub_si rX simm = writeRegister rX <~ (readRegister rX, (-), pure $ signedValue simm)

-- | Instruction @mul rX, dmemaddr@ is implemented as @rX = rX * [dmemaddr]@.
mul :: Register -> MemoryAddress -> Redfin ()
mul rX dmemaddr = writeRegister rX <~ (readRegister rX, (*), readMemory dmemaddr)

-- | Instruction @mul_si rX, simm@ is implemented as @rX = rX * simm@.
mul_si :: Register -> SImm8 -> Redfin ()
mul_si rX simm = writeRegister rX <~ (readRegister rX, (*), pure $ signedValue simm)

-- | Instruction @div rX, dmemaddr@ is implemented as @rX = rX / [dmemaddr]@.
div :: Register -> MemoryAddress -> Redfin ()
div rX dmemaddr = writeRegister rX <~ (readRegister rX, Std.div, readMemory dmemaddr)

-- | Instruction @div_si rX, simm@ is implemented as @rX = rX / simm@.
div_si :: Register -> SImm8 -> Redfin ()
div_si rX simm = writeRegister rX <~ (readRegister rX, Std.div, pure $ signedValue simm)

-- | Instruction @and rX, dmemaddr@ is implemented as @rX = rX & [dmemaddr]@.
and :: Register -> MemoryAddress -> Redfin ()
and rX dmemaddr = writeRegister rX <~ (readRegister rX, (.&.), readMemory dmemaddr)

-- | Instruction @or rX, dmemaddr@ is implemented as @rX = rX | [dmemaddr]@.
or :: Register -> MemoryAddress -> Redfin ()
or rX dmemaddr = writeRegister rX <~ (readRegister rX, (.|.), readMemory dmemaddr)

-- | Instruction @xor rX, dmemaddr@ is implemented as @rX = rX xor [dmemaddr]@.
xor :: Register -> MemoryAddress -> Redfin ()
xor rX dmemaddr = writeRegister rX <~ (readRegister rX, Std.xor, readMemory dmemaddr)

-- | Instruction @not rX, dmemaddr@ is implemented as @rX = ~rX@.
not :: Register -> Redfin ()
not rX = writeRegister rX =<< (complement <$> readRegister rX)

-- | Logical left shift of a 'Value' by the number of bits given in another 'Value'.
shiftLL :: Value -> Value -> Value
shiftLL a b = a `Std.shiftL` (fromIntegral b)

-- | Logical right shift of a 'Value' by the number of bits given in another 'Value'.
shiftRL :: Value -> Value -> Value
shiftRL a b = fromIntegral $ ((fromIntegral a) :: Word64) `Std.shiftR` (fromIntegral b)

-- | Arithmetic right shift of a 'Value' by the number of bits given in another 'Value'.
shiftRA :: Value -> Value -> Value
shiftRA a b = a `Std.shiftR` (fromIntegral b)

-- | Instruction @sl rX, dmemaddr@ is implemented as @rX = rX << [dmemaddr]@.
sl :: Register -> MemoryAddress -> Redfin ()
sl rX dmemaddr = writeRegister rX <~ (readRegister rX, shiftLL, readMemory dmemaddr)

-- | Instruction @sl_i rX, uimm@ is implemented as @rX = rX << uimm@.
sl_i :: Register -> UImm8 -> Redfin ()
sl_i rX uimm = writeRegister rX <~ (readRegister rX, shiftLL, pure $ unsignedValue uimm)

-- | Instruction @sr rX, dmemaddr@ is implemented as @rX = rX >> [dmemaddr]@.
sr :: Register -> MemoryAddress -> Redfin ()
sr rX dmemaddr = writeRegister rX <~ (readRegister rX, shiftRL, readMemory dmemaddr)

-- | Instruction @sr_i rX, uimm@ is implemented as @rX = rX >> uimm@.
sr_i :: Register -> UImm8 -> Redfin ()
sr_i rX uimm = writeRegister rX <~ (readRegister rX, shiftRL, pure $ unsignedValue uimm)

-- | Instruction @sra rX, dmemaddr@ is implemented as @rX = (int)rX >> [dmemaddr]@.
sra :: Register -> MemoryAddress -> Redfin ()
sra rX dmemaddr = writeRegister rX <~ (readRegister rX, shiftRA, readMemory dmemaddr)

-- | Instruction @sra_i rX, uimm@ is implemented as @rX = (int)rX >> uimm@.
sra_i :: Register -> UImm8 -> Redfin ()
sra_i rX uimm = writeRegister rX <~ (readRegister rX, shiftRA, pure $ unsignedValue uimm)

-- | Instruction @cmpeq rX, dmemaddr@ is implemented as @cond = (rX == [dmemaddr])@.
cmpeq :: Register -> MemoryAddress -> Redfin ()
cmpeq rX dmemaddr = writeFlag Condition <~ (readRegister rX, (==), readMemory dmemaddr)

-- | Instruction @cmplt rX, dmemaddr@ is implemented as @cond = (rX < [dmemaddr])@.
cmplt :: Register -> MemoryAddress -> Redfin ()
cmplt rX dmemaddr =
    writeFlag Condition <~ (readRegister rX, (<), readMemory dmemaddr)

-- | Instruction @cmpgt rX, dmemaddr@ is implemented as @cond = (rX > [dmemaddr])@.
cmpgt :: Register -> MemoryAddress -> Redfin ()
cmpgt rX dmemaddr = writeFlag Condition <~ (readRegister rX, (>), readMemory dmemaddr)

-- | Instruction @ld_si rX, simm@ is implemented as @rx = (int)simm@.
ld_si :: Register -> SImm8 -> Redfin ()
ld_si rX simm = writeRegister rX $ signedValue simm

-- | A simple test program.
testProgram :: Redfin ()
testProgram = do
    add R0 10
