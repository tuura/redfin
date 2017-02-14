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

-- | 'Value' datatype represents data values in Redfin. The precise bit-width
-- is left unspecified, but it is assumed that it fits into 64 bits.
newtype Value = Value Int64
    deriving (Bits, Enum, Eq, Integral, Num, Ord, Real, Show)

-- | Redfin has 4 general-purpose registers R0-R3.
data Register = R0 | R1 | R2 | R3 deriving (Eq, Ord, Show)

-- | The register bank is represented by a map from registers to their values.
type RegisterBank = Map Register Value

-- | Redfin memory can hold 256 values.
newtype MemoryAddress = MemoryAddress Word8 deriving (Eq, Num, Ord, Show)

-- | Memory is represented by a map from memory addresses to their values.
type Memory = Map MemoryAddress Value

-- | Programs are stored in program memory (currently, up to 1024 instructions).
newtype InstructionAddress = InstructionAddress Word16
    deriving (Eq, Num, Ord, Show)

-- | Instructions have 16-bit codes.
newtype InstructionCode = InstructionCode Word16 deriving (Eq, Num, Show)

-- | Program is represented by a map from instruction addresses to codes.
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
    return $ fromMaybe ()
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
-- TODO: Add signed and unsigned datatypes.

-- | Instruction @add rX, dmemaddr@ is implemented as @rX = rX + [dmemaddr]@.
add :: Register -> MemoryAddress -> Redfin ()
add register address =
    writeRegister register <~ (readRegister register, (+), readMemory address)

-- | Instruction @add_si rX, simm@ is implemented as @rX = rX + simm@.
add_si :: Register -> Value -> Redfin ()
add_si register value =
    writeRegister register <~ (readRegister register, (+), pure value)

-- | Instruction @sub rX, dmemaddr@ is implemented as @rX = rX - [dmemaddr]@.
sub :: Register -> MemoryAddress -> Redfin ()
sub register address =
    writeRegister register <~ (readRegister register, (-), readMemory address)

-- | Instruction @sub_si rX, simm@ is implemented as @rX = rX - simm@.
sub_si :: Register -> Value -> Redfin ()
sub_si register value =
    writeRegister register <~ (readRegister register, (-), pure value)

-- | Instruction @mul rX, dmemaddr@ is implemented as @rX = rX * [dmemaddr]@.
mul :: Register -> MemoryAddress -> Redfin ()
mul register address =
    writeRegister register <~ (readRegister register, (*), readMemory address)

-- | Instruction @mul_si rX, simm@ is implemented as @rX = rX * simm@.
mul_si :: Register -> Value -> Redfin ()
mul_si register value =
    writeRegister register <~ (readRegister register, (*), pure value)

-- | Instruction @div rX, dmemaddr@ is implemented as @rX = rX / [dmemaddr]@.
div :: Register -> MemoryAddress -> Redfin ()
div register address =
    writeRegister register <~ (readRegister register, Std.div, readMemory address)

-- | Instruction @div_si rX, simm@ is implemented as @rX = rX / simm@.
div_si :: Register -> Value -> Redfin ()
div_si register value =
    writeRegister register <~ (readRegister register, Std.div, pure value)

-- | Instruction @and rX, dmemaddr@ is implemented as @rX = rX & [dmemaddr]@.
and :: Register -> MemoryAddress -> Redfin ()
and register address =
    writeRegister register <~ (readRegister register, (.&.), readMemory address)

-- | Instruction @or rX, dmemaddr@ is implemented as @rX = rX | [dmemaddr]@.
or :: Register -> MemoryAddress -> Redfin ()
or register address =
    writeRegister register <~ (readRegister register, (.|.), readMemory address)

-- | Instruction @xor rX, dmemaddr@ is implemented as @rX = rX xor [dmemaddr]@.
xor :: Register -> MemoryAddress -> Redfin ()
xor register address =
    writeRegister register <~ (readRegister register, Std.xor, readMemory address)

-- | Instruction @not rX, dmemaddr@ is implemented as @rX = ~rX@.
not :: Register -> Redfin ()
not register = do
    value <- readRegister register
    writeRegister register $ complement value

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
sl register address =
    writeRegister register <~ (readRegister register, shiftLL, readMemory address)

-- | Instruction @sl_i rX, uimm@ is implemented as @rX = rX << uimm@.
sl_i :: Register -> Value -> Redfin ()
sl_i register value =
    writeRegister register <~ (readRegister register, shiftLL, pure value)

-- | Instruction @sr rX, dmemaddr@ is implemented as @rX = rX >> [dmemaddr]@.
sr :: Register -> MemoryAddress -> Redfin ()
sr register address =
    writeRegister register <~ (readRegister register, shiftRL, readMemory address)

-- | Instruction @sr_i rX, uimm@ is implemented as @rX = rX >> uimm@.
sr_i :: Register -> Value -> Redfin ()
sr_i register value =
    writeRegister register <~ (readRegister register, shiftRL, pure value)

-- | Instruction @sra rX, dmemaddr@ is implemented as @rX = (int)rX >> [dmemaddr]@.
sra :: Register -> MemoryAddress -> Redfin ()
sra register address =
    writeRegister register <~ (readRegister register, shiftRA, readMemory address)

-- | Instruction @sra_i rX, uimm@ is implemented as @rX = (int)rX >> uimm@.
sra_i :: Register -> Value -> Redfin ()
sra_i register value =
    writeRegister register <~ (readRegister register, shiftRA, pure value)

-- | Instruction @cmpeq rX, dmemaddr@ is implemented as @cond = (rX == [dmemaddr])@.
cmpeq :: Register -> MemoryAddress -> Redfin ()
cmpeq register address =
    writeFlag Condition <~ (readRegister register, (==), readMemory address)

-- | Instruction @cmplt rX, dmemaddr@ is implemented as @cond = (rX < [dmemaddr])@.
cmplt :: Register -> MemoryAddress -> Redfin ()
cmplt register address =
    writeFlag Condition <~ (readRegister register, (<), readMemory address)

-- | Instruction @cmpgt rX, dmemaddr@ is implemented as @cond = (rX > [dmemaddr])@.
cmpgt :: Register -> MemoryAddress -> Redfin ()
cmpgt register address =
    writeFlag Condition <~ (readRegister register, (>), readMemory address)

-- | Instruction @ld_si rX, simm@ is implemented as @rx = (int)simm@.
ld_si :: Register -> Value -> Redfin ()
ld_si register value = writeRegister register value

-- | A simple test program.
testProgram :: Redfin ()
testProgram = do
    add R0 10
