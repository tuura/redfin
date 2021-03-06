-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Semantics
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2017-2020
--
-- Maintainer  :  mail@geo2a.info
-- Stability   :  experimental
--
-- Semantics of REDFIN instructions.
--
-----------------------------------------------------------------------------
module Redfin.Semantics (
    -- * Arithmetic instructions
    add, add_si, sub, sub_si, mul, mul_si, div, div_si,
    fadd, fsub, fmul, fdiv,
    abs,

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

import           Control.Monad.Extra
import           Data.Bits           hiding (xor)
import qualified Data.Bits           as Std
import           Data.SBV
import           Prelude             hiding (abs, div, not)
import qualified Prelude             (abs)

import qualified Debug.Trace         as Debugger

import           Redfin
import           Redfin.Data.Fixed
import           Redfin.Types

-- | A convenient combinator for defining instructions that fit the pattern
-- @res = arg1 op arg2@, e.g. addition @rX = rX + [dmemaddr]@ can be defined as:
--
-- > add rX dmemaddr = writeRegister rX <~ (readRegister rX, (+), readMemory dmemaddr)
(<~) :: (c -> Redfin ()) -> (Redfin a, a -> b -> c, Redfin b) -> Redfin ()
(<~) res (arg1, op, arg2) = do
    x <- arg1
    y <- arg2
    res $ x `op` y


-- | Instruction @add rX, dmemaddr@ is implemented as @rX = rX + [dmemaddr]@.
add :: Register -> MemoryAddress -> Redfin ()
add rX dmemaddr = do
    state <- readState
    arg1 <- readRegister rX
    arg2 <- readMemory dmemaddr
    let overflow = arg2 .> 0 .&& arg1 .> (maxBound @Value - arg2) .||
                   arg2 .< 0 .&& arg1 .< (minBound @Value - arg2)
    let overflowState = snd $ transform (writeFlag Overflow sTrue) state
    writeState $ ite overflow overflowState state
    writeRegister rX (arg1 + arg2)

-- | Instruction @add_si rX, simm@ is implemented as @rX = rX + simm@.
add_si :: Register -> SImm8 -> Redfin ()
add_si rX simm = do
    state <- readState
    arg1 <- readRegister rX
    arg2 <- pure $ fromSImm8 simm
    let overflow = arg2 .> 0 .&& arg1 .> (maxBound @Value - arg2) .||
                   arg2 .< 0 .&& arg1 .< (minBound @Value - arg2)
    let overflowState = snd $ transform (writeFlag Overflow sTrue) state
    writeState $ ite overflow overflowState state
    writeRegister rX (arg1 + arg2)

-- | Instruction @sub rX, dmemaddr@ is implemented as @rX = rX - [dmemaddr]@.
sub :: Register -> MemoryAddress -> Redfin ()
sub rX dmemaddr = do
    state <- readState
    arg1 <- readRegister rX
    arg2 <- readMemory dmemaddr
    let overflow = arg2 .> 0 .&& arg1 .< (minBound @Value + arg2) .||
                   arg2 .< 0 .&& arg1 .> (maxBound @Value + arg2)
    let overflowState = snd $ transform (writeFlag Overflow sTrue) state
    writeState $ ite overflow overflowState state
    writeRegister rX (arg1 - arg2)

-- | Instruction @sub_si rX, simm@ is implemented as @rX = rX - simm@.
sub_si :: Register -> SImm8 -> Redfin ()
sub_si rX simm = do --writeRegister rX <~~ (readRegister rX, (-), )
    state <- readState
    arg1 <- readRegister rX
    arg2 <- pure $ fromSImm8 simm
    let overflow = arg2 .> 0 .&& arg1 .< (minBound @Value + arg2) .||
                   arg2 .< 0 .&& arg1 .> (maxBound @Value + arg2)
    let overflowState = snd $ transform (writeFlag Overflow sTrue) state
    writeState $ ite overflow overflowState state
    writeRegister rX (arg1 - arg2)

-- | Instruction @mul rX, dmemaddr@ is implemented as @rX = rX * [dmemaddr]@.
mul :: Register -> MemoryAddress -> Redfin ()
mul rX dmemaddr = do -- writeRegister rX <~~ (readRegister rX, (*), readMemory dmemaddr)
    state <- readState
    arg1 <- readRegister rX
    arg2 <- readMemory dmemaddr
    let overflow = arg1 .>  0 .&& arg2 .>  0 .&& arg1 .> sDiv (maxBound @Value) arg2
               .|| arg1 .>  0 .&& arg2 .<= 0 .&& arg2 .< sDiv (minBound @Value) arg1
               .|| arg1 .<= 0 .&& arg2 .>  0 .&& arg1 .< sDiv (minBound @Value) arg2
               .|| arg1 .<= 0 .&& arg2 .<= 0 .&& arg1 ./= 0
                                             .&& arg2 .< sDiv (maxBound @Value) arg1
    let overflowState = snd $ transform (writeFlag Overflow sTrue) state
    writeState $ ite overflow overflowState state
    writeRegister rX (arg1 * arg2)

-- | Instruction @mul_si rX, simm@ is implemented as @rX = rX * simm@.
mul_si :: Register -> SImm8 -> Redfin ()
mul_si rX simm = do
    state <- readState
    arg1 <- readRegister rX
    arg2 <- pure $ fromSImm8 simm
    let overflow = arg1 .>  0 .&& arg2 .>  0 .&& arg1 .> sDiv (maxBound @Value) arg2
               .|| arg1 .>  0 .&& arg2 .<= 0 .&& arg2 .< sDiv (minBound @Value) arg1
               .|| arg1 .<= 0 .&& arg2 .>  0 .&& arg1 .< sDiv (minBound @Value) arg2
               .|| arg1 .<= 0 .&& arg2 .<= 0 .&& arg1 ./= 0
                                             .&& arg2 .< sDiv (maxBound @Value) arg1
    let overflowState = snd $ transform (writeFlag Overflow sTrue) state
    writeState $ ite overflow overflowState state
    writeRegister rX (arg1 * arg2)

-- | Instruction @div rX, dmemaddr@ is implemented as @rX = rX / [dmemaddr]@.
div :: Register -> MemoryAddress -> Redfin ()
div rX dmemaddr = do
    state <- readState
    arg1 <- readRegister rX
    arg2 <- readMemory dmemaddr
    let overflow = arg2 .== 0 .|| arg1 .== minBound @Value .&& arg2 .== -1
    let overflowState = snd $ transform (writeFlag Overflow sTrue) state
    writeState $ ite overflow overflowState state
    writeRegister rX (sDiv arg1 arg2)

-- | Instruction @div_si rX, simm@ is implemented as @rX = rX / simm@.
div_si :: Register -> SImm8 -> Redfin ()
div_si rX simm = do
    state <- readState
    arg1 <- readRegister rX
    arg2 <- pure $ fromSImm8 simm
    let overflow = arg2 .== 0 .|| arg1 .== minBound @Value .&& arg2 .== -1
    let overflowState = snd $ transform (writeFlag Overflow sTrue) state
    writeState $ ite overflow overflowState state
    writeRegister rX (sDiv arg1 arg2)

-- | Instruction @add rX, dmemaddr@ is implemented as @rX = rX + [dmemaddr]@.
--   TODO: overflow handling.
fadd :: Register -> MemoryAddress -> Redfin ()
fadd rX dmemaddr = do
    arg1 <- Fixed <$> readRegister rX
    arg2 <- Fixed <$> readMemory dmemaddr
    writeRegister rX (getFixed $ arg1 + arg2)

-- | Instruction @sub rX, dmemaddr@ is implemented as @rX = rX - [dmemaddr]@.
--   TODO: overflow handling.
fsub :: Register -> MemoryAddress -> Redfin ()
fsub rX dmemaddr = do
    arg1 <- Fixed <$> readRegister rX
    arg2 <- Fixed <$> readMemory dmemaddr
    writeRegister rX (getFixed $ arg1 - arg2)

-- | Instruction @mul rX, dmemaddr@ is implemented as @rX = rX * [dmemaddr]@.
--   TODO: overflow handling.
fmul :: Register -> MemoryAddress -> Redfin ()
fmul rX dmemaddr = do
    arg1 <- Fixed <$> readRegister rX
    arg2 <- Fixed <$> readMemory dmemaddr
    writeRegister rX (getFixed $ arg1 * arg2)

-- | Instruction @div rX, dmemaddr@ is implemented as @rX = rX / [dmemaddr]@.
--   TODO: overflow handling.
fdiv :: Register -> MemoryAddress -> Redfin ()
fdiv rX dmemaddr = do
    arg1 <- Fixed <$> readRegister rX
    arg2 <- Fixed <$> readMemory dmemaddr
    writeRegister rX (getFixed $ arg1 / arg2)

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

-- | Instruction @abd rX, dmemaddr@ is implemented as @rX = abs(rX)@.
abs :: Register -> Redfin ()
abs rX = do
    state <- readState
    result <- Prelude.abs <$> readRegister rX
    let overflowState = snd $ transform (writeFlag Overflow sTrue) state
    writeState $ ite (result .< 0) overflowState state
    writeRegister rX result

-- | Instruction @sl rX, dmemaddr@ is implemented as @rX = rX << [dmemaddr]@.
sl :: Register -> MemoryAddress -> Redfin ()
sl rX dmemaddr = writeRegister rX <~ (readRegister rX, sShiftLeft, readMemory dmemaddr)

-- | Instruction @sl_i rX, uimm@ is implemented as @rX = rX << uimm@.
sl_i :: Register -> UImm8 -> Redfin ()
sl_i rX uimm = writeRegister rX <~ (readRegister rX, sShiftLeft, pure uimm)

-- | Instruction @sr rX, dmemaddr@ is implemented as @rX = rX >> [dmemaddr]@.
sr :: Register -> MemoryAddress -> Redfin ()
sr rX dmemaddr = writeRegister rX <~ (readRegister rX, sShiftRight, readMemory dmemaddr)

-- | Instruction @sr_i rX, uimm@ is implemented as @rX = rX >> uimm@.
sr_i :: Register -> UImm8 -> Redfin ()
sr_i rX uimm = writeRegister rX <~ (readRegister rX, sShiftRight, pure uimm)

-- | Instruction @sra rX, dmemaddr@ is implemented as @rX = (int)rX >> [dmemaddr]@.
sra :: Register -> MemoryAddress -> Redfin ()
sra rX dmemaddr = writeRegister rX <~ (readRegister rX, sSignedShiftArithRight, readMemory dmemaddr)

-- | Instruction @sra_i rX, uimm@ is implemented as @rX = (int)rX >> uimm@.
sra_i :: Register -> UImm8 -> Redfin ()
sra_i rX uimm = writeRegister rX <~ (readRegister rX, sSignedShiftArithRight, pure uimm)

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
ld_si rX simm = writeRegister rX (fromSImm8 simm)

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
    let jumpState = snd $ transform (jmpi simm) state
    writeState $ ite condition jumpState state

-- | Instruction @jmpi_cf simm@ is implemented as
-- @if ¬Condition: InstructionCounter = InstructionCounter + simm + 1@.
jmpi_cf :: SImm10 -> Redfin ()
jmpi_cf simm = do
    condition <- readFlag Condition
    state <- readState
    let jumpState = snd $ transform (jmpi simm) state
    writeState $ ite condition state jumpState

-- | Instruction @wait uimm@ does nothing for @uimm@ clock cycles.
wait :: UImm10 -> Redfin ()
wait _uimm = pure () -- delay (fromUImm10 uimm)

-- | Instruction @halt@ raises the @Halt@ flag and stops the execution.
halt :: Redfin ()
halt = writeFlag Halt sTrue
