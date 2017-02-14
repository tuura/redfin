-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.InstructionSet
-- Copyright   :  (c) Andrey Mokhov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- REDFIN instruction set.
--
-----------------------------------------------------------------------------
module Redfin.InstructionSet (
    -- * Arithmetic instructions
    add, add_si, sub, sub_si, mul, mul_si, div, div_si,

    -- * Logical bit-wise instructions
    Redfin.InstructionSet.and, Redfin.InstructionSet.or,
    Redfin.InstructionSet.xor, Redfin.InstructionSet.not,
    sl, sl_i, sr, sr_i, sra, sra_i,

    -- * Load/store instructions
    ld, ld_i, ld_si, ldmi, st, stmi,

    -- * Comparison instructions
    cmpeq, cmplt, cmpgt,

    -- * Jump instructions
    jmpi, jmpi_ct, jmpi_cf,

    -- * Silent instructions
    nop, wait, halt
    ) where

import Control.Monad.Extra
import qualified Data.Bits as Std
import Data.Bits hiding (xor, shiftL, shiftR)
import Prelude hiding (div, not)
import qualified Prelude as Std
import Data.Word

import Redfin

-- TODO: Switch to Fetch -> Decode -> Execute cycle.
-- TODO: Add DivisionByZero flag.
-- TODO: Set Overflow flag.
-- TODO: Implement bus and EEPROM I/O instructions.
-- TODO: Implement fixed-point instructions.
-- TODO: Model instruction delays accurately.

-- | Instruction @add rX, dmemaddr@ is implemented as @rX = rX + [dmemaddr]@.
add :: Register -> MemoryAddress -> Redfin InstructionCode
add rX dmemaddr = writeRegister rX <~ (readRegister rX, (+), readMemory dmemaddr)

-- | Instruction @add_si rX, simm@ is implemented as @rX = rX + simm@.
add_si :: Register -> SImm8 -> Redfin InstructionCode
add_si rX simm = writeRegister rX <~ (readRegister rX, (+), pure $ signedValue simm)

-- | Instruction @sub rX, dmemaddr@ is implemented as @rX = rX - [dmemaddr]@.
sub :: Register -> MemoryAddress -> Redfin InstructionCode
sub rX dmemaddr = writeRegister rX <~ (readRegister rX, (-), readMemory dmemaddr)

-- | Instruction @sub_si rX, simm@ is implemented as @rX = rX - simm@.
sub_si :: Register -> SImm8 -> Redfin InstructionCode
sub_si rX simm = writeRegister rX <~ (readRegister rX, (-), pure $ signedValue simm)

-- | Instruction @mul rX, dmemaddr@ is implemented as @rX = rX * [dmemaddr]@.
mul :: Register -> MemoryAddress -> Redfin InstructionCode
mul rX dmemaddr = writeRegister rX <~ (readRegister rX, (*), readMemory dmemaddr)

-- | Instruction @mul_si rX, simm@ is implemented as @rX = rX * simm@.
mul_si :: Register -> SImm8 -> Redfin InstructionCode
mul_si rX simm = writeRegister rX <~ (readRegister rX, (*), pure $ signedValue simm)

-- | Instruction @div rX, dmemaddr@ is implemented as @rX = rX / [dmemaddr]@.
div :: Register -> MemoryAddress -> Redfin InstructionCode
div rX dmemaddr = writeRegister rX <~ (readRegister rX, Std.div, readMemory dmemaddr)

-- | Instruction @div_si rX, simm@ is implemented as @rX = rX / simm@.
div_si :: Register -> SImm8 -> Redfin InstructionCode
div_si rX simm = writeRegister rX <~ (readRegister rX, Std.div, pure $ signedValue simm)

-- | Instruction @and rX, dmemaddr@ is implemented as @rX = rX & [dmemaddr]@.
and :: Register -> MemoryAddress -> Redfin InstructionCode
and rX dmemaddr = writeRegister rX <~ (readRegister rX, (.&.), readMemory dmemaddr)

-- | Instruction @or rX, dmemaddr@ is implemented as @rX = rX | [dmemaddr]@.
or :: Register -> MemoryAddress -> Redfin InstructionCode
or rX dmemaddr = writeRegister rX <~ (readRegister rX, (.|.), readMemory dmemaddr)

-- | Instruction @xor rX, dmemaddr@ is implemented as @rX = rX xor [dmemaddr]@.
xor :: Register -> MemoryAddress -> Redfin InstructionCode
xor rX dmemaddr = writeRegister rX <~ (readRegister rX, Std.xor, readMemory dmemaddr)

-- | Instruction @not rX, dmemaddr@ is implemented as @rX = ~rX@.
not :: Register -> Redfin InstructionCode
not rX = withFetch $ writeRegister rX =<< (complement <$> readRegister rX)

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
sl :: Register -> MemoryAddress -> Redfin InstructionCode
sl rX dmemaddr = writeRegister rX <~ (readRegister rX, shiftLL, readMemory dmemaddr)

-- | Instruction @sl_i rX, uimm@ is implemented as @rX = rX << uimm@.
sl_i :: Register -> UImm8 -> Redfin InstructionCode
sl_i rX uimm = writeRegister rX <~ (readRegister rX, shiftLL, pure $ unsignedValue uimm)

-- | Instruction @sr rX, dmemaddr@ is implemented as @rX = rX >> [dmemaddr]@.
sr :: Register -> MemoryAddress -> Redfin InstructionCode
sr rX dmemaddr = writeRegister rX <~ (readRegister rX, shiftRL, readMemory dmemaddr)

-- | Instruction @sr_i rX, uimm@ is implemented as @rX = rX >> uimm@.
sr_i :: Register -> UImm8 -> Redfin InstructionCode
sr_i rX uimm = writeRegister rX <~ (readRegister rX, shiftRL, pure $ unsignedValue uimm)

-- | Instruction @sra rX, dmemaddr@ is implemented as @rX = (int)rX >> [dmemaddr]@.
sra :: Register -> MemoryAddress -> Redfin InstructionCode
sra rX dmemaddr = writeRegister rX <~ (readRegister rX, shiftRA, readMemory dmemaddr)

-- | Instruction @sra_i rX, uimm@ is implemented as @rX = (int)rX >> uimm@.
sra_i :: Register -> UImm8 -> Redfin InstructionCode
sra_i rX uimm = writeRegister rX <~ (readRegister rX, shiftRA, pure $ unsignedValue uimm)

-- | Instruction @cmpeq rX, dmemaddr@ is implemented as @cond = (rX == [dmemaddr])@.
cmpeq :: Register -> MemoryAddress -> Redfin InstructionCode
cmpeq rX dmemaddr = writeFlag Condition <~ (readRegister rX, (==), readMemory dmemaddr)

-- | Instruction @cmplt rX, dmemaddr@ is implemented as @cond = (rX < [dmemaddr])@.
cmplt :: Register -> MemoryAddress -> Redfin InstructionCode
cmplt rX dmemaddr = writeFlag Condition <~ (readRegister rX, (<), readMemory dmemaddr)

-- | Instruction @cmpgt rX, dmemaddr@ is implemented as @cond = (rX > [dmemaddr])@.
cmpgt :: Register -> MemoryAddress -> Redfin InstructionCode
cmpgt rX dmemaddr = writeFlag Condition <~ (readRegister rX, (>), readMemory dmemaddr)

-- | Instruction @ld_si rX, simm@ is implemented as @rx = (int)simm@.
ld_si :: Register -> SImm8 -> Redfin InstructionCode
ld_si rX simm = withFetch $ writeRegister rX $ signedValue simm

-- | Instruction @ld_i rX, uimm@ is implemented as @rx = uimm@.
ld_i :: Register -> UImm8 -> Redfin InstructionCode
ld_i rX uimm = withFetch $ writeRegister rX $ unsignedValue uimm

-- | Instruction @ld rX, dmemaddr@ is implemented as @rx = [dmemaddr]@.
ld :: Register -> MemoryAddress -> Redfin InstructionCode
ld rX dmemaddr = withFetch $ writeRegister rX =<< readMemory dmemaddr

-- | Instruction @ldmi rX, dmemaddr@ is implemented as @rx = [[dmemaddr]]@.
ldmi :: Register -> MemoryAddress -> Redfin InstructionCode
ldmi rX dmemaddr = withFetch $
    writeRegister rX =<< readMemory =<< toMemoryAddress =<< readMemory dmemaddr

-- | Instruction @st rX, dmemaddr@ is implemented as @[dmemaddr] = rx@.
st :: Register -> MemoryAddress -> Redfin InstructionCode
st rX dmemaddr = withFetch $ writeMemory dmemaddr =<< readRegister rX

-- | Instruction @stmi rX, dmemaddr@ is implemented as @[[dmemaddr]] = rx@.
stmi :: Register -> MemoryAddress -> Redfin InstructionCode
stmi rX dmemaddr = withFetch $ do
    value   <- readRegister rX
    address <- toMemoryAddress =<< readMemory dmemaddr
    writeMemory address value

-- | Instruction @jmpi simm@ is implemented as
-- @InstructionCounter = InstructionCounter + simm + 1@.
jmpi :: SImm10 -> Redfin InstructionCode
jmpi (SImm10 simm) = withFetch $ transformState $
    \(State rs ic fs m p c) -> State rs (ic + fromIntegral simm) fs m p c

-- | Instruction @jmpi_ct simm@ is implemented as
-- @if Condition: InstructionCounter = InstructionCounter + simm + 1@.
jmpi_ct :: SImm10 -> Redfin InstructionCode
jmpi_ct (SImm10 simm) = withFetch $ whenM (readFlag Condition) $ transformState $
    \(State rs ic fs m p c) -> State rs (ic + fromIntegral simm) fs m p c

-- | Instruction @jmpi_cf simm@ is implemented as
-- @if ¬Condition: InstructionCounter = InstructionCounter + simm + 1@.
jmpi_cf :: SImm10 -> Redfin InstructionCode
jmpi_cf (SImm10 simm) = withFetch $ unlessM (readFlag Condition) $ transformState $
    \(State rs ic fs m p c) -> State rs (ic + fromIntegral simm) fs m p c

-- | Instruction @wait uimm@ does nothing for @uimm@ clock cycles.
wait :: UImm10 -> Redfin InstructionCode
wait (UImm10 uimm) = withFetch $ delay (fromIntegral uimm)

-- | Do nothing apart from fetching the next instruction.
nop :: Redfin InstructionCode
nop = withFetch $ return ()

-- | Instruction @halt@ is currently implemented as a no-op. TODO: Provide a
-- more meaningful implementation, for example, by raising the @Halt@ flag.
halt :: Redfin InstructionCode
halt = nop
