-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Semantics
-- Copyright   :  (c) Andrey Mokhov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- Semantics of REDFIN instructions.
--
-----------------------------------------------------------------------------
module Redfin.Semantics (
    -- * Arithmetic instructions
    add, add_si, sub, sub_si, mul, mul_si, div, div_si,
    fadd, fsub, fmul, fdiv,
    -- * Logical bit-wise instructions
    Redfin.Semantics.and, Redfin.Semantics.or,
    Redfin.Semantics.xor, Redfin.Semantics.not,
    sl, sl_i, sr, sr_i, sra, sra_i,

    -- * Load/store instructions
    ld, ld_i, ld_si, ldmi, st, stmi,

    -- * Comparison instructions
    cmpeq, cmplt, cmpgt,

    -- * Jump instructions
    jmpi, jmpi_ct, jmpi_cf,

    -- * Miscellaneous instructions
    wait, halt
    ) where

import Control.Monad.Extra
import qualified Data.Bits as Std
import Data.Bits hiding (xor)
import Prelude hiding (div, not)
import Data.SBV

import Redfin

-- | A convenient combinator for defining instructions that fit the pattern
-- @res = arg1 op arg2@, e.g. addition @rX = rX + [dmemaddr]@ can be defined as:
--
-- > add rX dmemaddr = writeRegister rX <~ (readRegister rX, (+), readMemory dmemaddr)
(<~) :: (c -> Redfin ()) -> (Redfin a, a -> b -> c, Redfin b) -> Redfin ()
(<~) res (arg1, op, arg2) = do
    x <- arg1
    y <- arg2
    res $ x `op` y

pad :: Int -> [SBool]
pad k = replicate k false

fromSImm8 :: SImm8 -> Value
fromSImm8 s = fromBitsLE $ blastLE s ++ replicate 56 (sTestBit s 7)

fromSImm10 :: SImm10 -> InstructionAddress
fromSImm10 s = fromBitsLE $ (take 10 $ blastLE s) ++ replicate 6 (sTestBit s 9)

fromUImm8 :: UImm8 -> Value
fromUImm8 u = fromBitsLE $ blastLE u ++ pad 56

fromUImm10 :: UImm10 -> Value
fromUImm10 u = fromBitsLE $ (take 10 $ blastLE u) ++ pad 54

-- | Instruction @add rX, dmemaddr@ is implemented as @rX = rX + [dmemaddr]@.
add :: Register -> MemoryAddress -> Redfin ()
add rX dmemaddr = writeRegister rX <~ (readRegister rX, (+), readMemory dmemaddr)

-- | Instruction @add_si rX, simm@ is implemented as @rX = rX + simm@.
add_si :: Register -> SImm8 -> Redfin ()
add_si rX simm = writeRegister rX <~ (readRegister rX, (+), pure $ fromSImm8 simm)

-- | Instruction @sub rX, dmemaddr@ is implemented as @rX = rX - [dmemaddr]@.
sub :: Register -> MemoryAddress -> Redfin ()
sub rX dmemaddr = writeRegister rX <~ (readRegister rX, (-), readMemory dmemaddr)

-- | Instruction @sub_si rX, simm@ is implemented as @rX = rX - simm@.
sub_si :: Register -> SImm8 -> Redfin ()
sub_si rX simm = writeRegister rX <~ (readRegister rX, (-), pure $ fromSImm8 simm)

-- | Instruction @mul rX, dmemaddr@ is implemented as @rX = rX * [dmemaddr]@.
mul :: Register -> MemoryAddress -> Redfin ()
mul rX dmemaddr = writeRegister rX <~ (readRegister rX, (*), readMemory dmemaddr)

-- | Instruction @mul_si rX, simm@ is implemented as @rX = rX * simm@.
mul_si :: Register -> SImm8 -> Redfin ()
mul_si rX simm = writeRegister rX <~ (readRegister rX, (*), pure $ fromSImm8 simm)

-- | Instruction @div rX, dmemaddr@ is implemented as @rX = rX / [dmemaddr]@.
div :: Register -> MemoryAddress -> Redfin ()
div rX dmemaddr = do
    delay 100
    writeRegister rX <~ (readRegister rX, sDiv, readMemory dmemaddr)

-- | Instruction @div_si rX, simm@ is implemented as @rX = rX / simm@.
div_si :: Register -> SImm8 -> Redfin ()
div_si rX simm = do
    delay 100
    writeRegister rX <~ (readRegister rX, sDiv, pure $ fromSImm8 simm)

-- | Instruction @add rX, dmemaddr@ is implemented as @rX = rX + [dmemaddr]@.
fadd :: Register -> MemoryAddress -> Redfin ()
fadd _rX _dmemaddr =
    error "Fixed precicion arithmetic unimplemented"
    -- writeRegister rX <~ (readRegister rX, (+), readMemory dmemaddr)

-- | Instruction @sub rX, dmemaddr@ is implemented as @rX = rX - [dmemaddr]@.
fsub :: Register -> MemoryAddress -> Redfin ()
fsub _rX _dmemaddr =
    error "Fixed precicion arithmetic unimplemented"
    -- writeRegister rX <~ (readRegister rX, (-), readMemory dmemaddr)

-- | Instruction @mul rX, dmemaddr@ is implemented as @rX = rX * [dmemaddr]@.
fmul :: Register -> MemoryAddress -> Redfin ()
fmul _rX _dmemaddr =
    error "Fixed precicion arithmetic unimplemented"
    -- writeRegister rX <~ (readRegister rX, (*), readMemory dmemaddr)

-- | Instruction @div rX, dmemaddr@ is implemented as @rX = rX / [dmemaddr]@.
fdiv :: Register -> MemoryAddress -> Redfin ()
fdiv _rX _dmemaddr = do
    error "Fixed precicion arithmetic unimplemented"
    -- delay 100
    -- writeRegister rX <~ (readRegister rX, sDiv, readMemory dmemaddr)

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

-- | Instruction @sl rX, dmemaddr@ is implemented as @rX = rX << [dmemaddr]@.
sl :: Register -> MemoryAddress -> Redfin ()
sl rX dmemaddr = writeRegister rX <~ (readRegister rX, sShiftLeft, readMemory dmemaddr)

-- | Instruction @sl_i rX, uimm@ is implemented as @rX = rX << uimm@.
sl_i :: Register -> UImm8 -> Redfin ()
sl_i rX uimm = writeRegister rX <~ (readRegister rX, sShiftLeft, pure $ fromUImm8 uimm)

-- | Instruction @sr rX, dmemaddr@ is implemented as @rX = rX >> [dmemaddr]@.
sr :: Register -> MemoryAddress -> Redfin ()
sr rX dmemaddr = writeRegister rX <~ (readRegister rX, sShiftRight, readMemory dmemaddr)

-- | Instruction @sr_i rX, uimm@ is implemented as @rX = rX >> uimm@.
sr_i :: Register -> UImm8 -> Redfin ()
sr_i rX uimm = writeRegister rX <~ (readRegister rX, sShiftRight, pure $ fromUImm8 uimm)

-- | Instruction @sra rX, dmemaddr@ is implemented as @rX = (int)rX >> [dmemaddr]@.
sra :: Register -> MemoryAddress -> Redfin ()
sra rX dmemaddr = writeRegister rX <~ (readRegister rX, sSignedShiftArithRight, readMemory dmemaddr)

-- | Instruction @sra_i rX, uimm@ is implemented as @rX = (int)rX >> uimm@.
sra_i :: Register -> UImm8 -> Redfin ()
sra_i rX uimm = writeRegister rX <~ (readRegister rX, sSignedShiftArithRight, pure $ fromUImm8 uimm)

-- | Instruction @cmpeq rX, dmemaddr@ is implemented as @cond = (rX == [dmemaddr])@.
cmpeq :: Register -> MemoryAddress -> Redfin ()
cmpeq rX dmemaddr = writeFlag Condition <~ (readRegister rX, (.==), readMemory dmemaddr)

-- | Instruction @cmplt rX, dmemaddr@ is implemented as @cond = (rX < [dmemaddr])@.
cmplt :: Register -> MemoryAddress -> Redfin ()
cmplt rX dmemaddr = writeFlag Condition <~ (readRegister rX, (.<), readMemory dmemaddr)

-- | Instruction @cmpgt rX, dmemaddr@ is implemented as @cond = (rX > [dmemaddr])@.
cmpgt :: Register -> MemoryAddress -> Redfin ()
cmpgt rX dmemaddr = writeFlag Condition <~ (readRegister rX, (.>), readMemory dmemaddr)

-- | Instruction @ld_si rX, simm@ is implemented as @rx = (int)simm@.
ld_si :: Register -> SImm8 -> Redfin ()
ld_si rX simm = writeRegister rX $ fromSImm8 simm

-- | Instruction @ld_i rX, uimm@ is implemented as @rx = uimm@.
ld_i :: Register -> UImm8 -> Redfin ()
ld_i rX uimm = writeRegister rX (fromUImm8 uimm)

-- | Instruction @ld rX, dmemaddr@ is implemented as @rx = [dmemaddr]@.
ld :: Register -> MemoryAddress -> Redfin ()
ld rX dmemaddr = writeRegister rX =<< readMemory dmemaddr

-- | Instruction @ldmi rX, dmemaddr@ is implemented as @rx = [[dmemaddr]]@.
ldmi :: Register -> MemoryAddress -> Redfin ()
ldmi rX dmemaddr = writeRegister rX =<< readMemory =<< toMemoryAddress =<< readMemory dmemaddr

-- | Instruction @st rX, dmemaddr@ is implemented as @[dmemaddr] = rx@.
st :: Register -> MemoryAddress -> Redfin ()
st rX dmemaddr = writeMemory dmemaddr =<< readRegister rX

-- | Instruction @stmi rX, dmemaddr@ is implemented as @[[dmemaddr]] = rx@.
stmi :: Register -> MemoryAddress -> Redfin ()
stmi rX dmemaddr = do
    value   <- readRegister rX
    address <- toMemoryAddress =<< readMemory dmemaddr
    writeMemory address value

-- | Instruction @jmpi simm@ is implemented as
-- @InstructionCounter = InstructionCounter + simm + 1@.
jmpi :: SImm10 -> Redfin ()
jmpi simm =
    transformState $ \(State rs  ic                    ir fs m p c)
                    -> State rs (ic + fromSImm10 simm) ir fs m p c


-- | Instruction @jmpi_ct simm@ is implemented as
-- @if Condition: InstructionCounter = InstructionCounter + simm + 1@.
jmpi_ct :: SImm10 -> Redfin ()
jmpi_ct simm = do
    condition <- readFlag Condition
    state <- readState
    let jumpState = snd $ redfin (jmpi simm) state
    writeState $ ite condition jumpState state

-- | Instruction @jmpi_cf simm@ is implemented as
-- @if ¬Condition: InstructionCounter = InstructionCounter + simm + 1@.
jmpi_cf :: SImm10 -> Redfin ()
jmpi_cf simm = do
    condition <- readFlag Condition
    state <- readState
    let jumpState = snd $ redfin (jmpi simm) state
    writeState $ ite condition state jumpState

-- | Instruction @wait uimm@ does nothing for @uimm@ clock cycles.
wait :: UImm10 -> Redfin ()
wait uimm = delay (fromUImm10 uimm)

-- | Instruction @halt@ is currently implemented as a no-op. TODO: Provide a
-- more meaningful implementation, for example, by raising the @Halt@ flag.
halt :: Redfin ()
halt = writeFlag Halt true
