{-# LANGUAGE BinaryLiterals, DataKinds, DeriveFunctor, FlexibleInstances, GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Assembly
-- Copyright   :  (c) Andrey Mokhov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- An embedded REDFIN assembly language.
--
-----------------------------------------------------------------------------
module Redfin.Assembly (
    -- * Assembly scripts and assembler
    Script, assemble, topOpcode, asm, label, goto, scriptLength,

    -- * Stack
    Stack (..), push, pop,

    -- * Expressions
    Expression, evaluate, read,

    -- * Arithmetic instructions
    add, add_si, sub, sub_si, mul, mul_si, div, div_si,

    -- * Logical bit-wise instructions
    Redfin.Assembly.and, Redfin.Assembly.or,
    Redfin.Assembly.xor, Redfin.Assembly.not,
    sl, sl_i, sr, sr_i, sra, sra_i,

    -- * Load/store instructions
    ld, ld_i, ld_si, ldmi, st, stmi,

    -- * Comparison instructions
    cmpeq, cmplt, cmpgt,

    -- * Jump instructions
    jmpi, jmpi_ct, jmpi_cf,

    -- * Miscellaneous instructions
    wait, halt,
    )where

import Control.Monad
import Data.Bits hiding (bit, xor)
import Data.SBV hiding (label, literal)
import Prelude hiding (and, div, not, or, abs, read)

import qualified Prelude (abs, div)

import Redfin

-- TODO: Get rid of ad-hoc fromIntegral.

type P = [InstructionCode]

-- | An assembly writer monad.
data Writer a = Writer
    { runWriter :: P -> (a, P)
    , topOpcode :: Opcode
    } deriving Functor

type Script = Writer ()

assemble :: Script -> Program
assemble s = foldr (\(c, p) a -> writeArray a p c) a0 (zip prg [0..])
  where
    a0  = mkSFunArray (const 0)
    prg = reverse $ snd $ runWriter s []

instance Applicative Writer where
    pure  = return
    (<*>) = ap

instance Monad Writer where
    return a         = Writer (\p -> (a, p)) (error "Empty script")
    Writer w t >>= f = Writer (\p -> let (a, p') = w p in runWriter (f a) p') t

newtype Label = Label Int

label :: Writer Label
label = Writer (\p -> (Label (length p), p)) (error "Label script")

goto :: Label -> Script
goto (Label there) = do
    Label here <- label
    let offset = fromIntegral (there - here - 1)
    jmpi offset -- TODO: Add error handling if offset is too large

scriptLength :: Num a => Script -> a
scriptLength = fromIntegral . length . snd . flip runWriter []

write :: Opcode -> InstructionCode -> Script
write o c = Writer (\p -> ((), (opcode o .|. c):p)) o

asm :: InstructionCode -> Script
asm code = write (decodeOpcode code) code

-- We distinguish between integer and fixed-point values
data ValueType = IntType | FPType

-- These are all semantically different memory addresses
newtype Temporary                 = Temporary MemoryAddress
newtype Stack                     = Stack     MemoryAddress
data Variable (a :: ValueType) where
    IntegerVariable    :: MemoryAddress -> Variable IntType
    FixedPointVariable :: MemoryAddress -> Variable FPType

-- Pushes the value stored in the register to the stack, advances the stack
-- pointer, and destroys the value stored in the register.
push :: Register -> Stack -> Script
push reg (Stack pointer) = do
    stmi reg pointer
    ld reg pointer
    add_si reg 1
    st reg pointer

-- Decrements the stack pointer, and loads the value from the top of the stack
-- into the given register.
pop :: Register -> Stack -> Script
pop reg (Stack pointer) = do
    ld reg pointer
    sub_si reg 1
    st reg pointer
    ldmi reg pointer

type BinaryOperator = Register -> MemoryAddress -> Script

-- Applies a binary operation, such as add, to the two top values stored in
-- stack and returns the result in a register
applyBinary :: Register -> Stack -> Temporary -> BinaryOperator -> Script
applyBinary reg stack (Temporary tmp) op = do
    pop reg stack
    st reg tmp
    pop reg stack
    op reg tmp

data Expression (a :: ValueType) = Lit SImm8
                                 | Var (Variable a)
                                 | Bin BinaryOperator (Expression a) (Expression a)
                                 | Abs (Expression a)

instance Num (Expression IntType) where
    fromInteger = Lit . fromIntegral
    (+)         = Bin add
    (-)         = Bin sub
    (*)         = Bin mul
    abs         = Abs
    signum x    = x `Prelude.div` Prelude.abs x

instance Eq (Expression IntType) where
    (==) = error "Eq cannot be implemented for Expression IntType"

instance Ord (Expression IntType) where
    compare = error "Ord cannot be implemented for Expression IntType"

instance Real (Expression IntType) where
    toRational = error "Real cannot be implemented for Expression IntType"

instance Enum (Expression IntType) where
    toEnum   = error "Enum cannot be implemented for Expression IntType"
    fromEnum = error "Enum cannot be implemented for Expression IntType"

instance Integral (Expression IntType) where
    div       = Bin div
    quotRem   = error "quotRem is not implemented for Expression IntType"
    toInteger = error "quotRem cannot be implemented for Expression IntType"

instance Num (Expression FPType) where
    fromInteger = Lit . fromIntegral
    (+)         = Bin fadd
    (-)         = Bin fsub
    (*)         = Bin fmul
    abs         = Abs
    signum x    = x / Prelude.abs x

instance Fractional (Expression FPType) where
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
    (/)            = Bin fdiv

read :: Variable a -> Expression a
read = Var

evaluate :: Register -> Stack -> Temporary -> Expression a -> Script
evaluate reg stack tmp expr = case expr of
    Lit value -> ld_si reg value
    Var (IntegerVariable    var) -> ld reg var
    Var (FixedPointVariable var) -> ld reg var
    Bin op x y -> do
        evaluate reg stack tmp x
        push reg stack
        evaluate reg stack tmp y
        push reg stack
        applyBinary reg stack tmp op
    Abs x -> do
        evaluate reg stack tmp x
        abs reg

-- Instructions
and   rX dmemaddr = write 0b000001 (register rX .|. address dmemaddr)
or    rX dmemaddr = write 0b000010 (register rX .|. address dmemaddr)
xor   rX dmemaddr = write 0b000011 (register rX .|. address dmemaddr)
add   rX dmemaddr = write 0b000100 (register rX .|. address dmemaddr)
sub   rX dmemaddr = write 0b000101 (register rX .|. address dmemaddr)
mul   rX dmemaddr = write 0b000110 (register rX .|. address dmemaddr)
div   rX dmemaddr = write 0b000111 (register rX .|. address dmemaddr)
ld    rX dmemaddr = write 0b001000 (register rX .|. address dmemaddr)
st    rX dmemaddr = write 0b001001 (register rX .|. address dmemaddr)
ldmi  rX dmemaddr = write 0b001010 (register rX .|. address dmemaddr)
stmi  rX dmemaddr = write 0b001011 (register rX .|. address dmemaddr)
cmpeq rX dmemaddr = write 0b010001 (register rX .|. address dmemaddr)
cmplt rX dmemaddr = write 0b010010 (register rX .|. address dmemaddr)
cmpgt rX dmemaddr = write 0b010011 (register rX .|. address dmemaddr)
sl    rX dmemaddr = write 0b011100 (register rX .|. address dmemaddr)
sr    rX dmemaddr = write 0b011101 (register rX .|. address dmemaddr)
sra   rX dmemaddr = write 0b011110 (register rX .|. address dmemaddr)

fadd   rX dmemaddr = write 0b001100 (register rX .|. address dmemaddr)
fsub   rX dmemaddr = write 0b001101 (register rX .|. address dmemaddr)
fmul   rX dmemaddr = write 0b001110 (register rX .|. address dmemaddr)
fdiv   rX dmemaddr = write 0b001111 (register rX .|. address dmemaddr)

add_si rX simm = write 0b100000 (register rX .|. simm8 simm)
sub_si rX simm = write 0b100001 (register rX .|. simm8 simm)
mul_si rX simm = write 0b100010 (register rX .|. simm8 simm)
div_si rX simm = write 0b100011 (register rX .|. simm8 simm)
ld_si  rX simm = write 0b100111 (register rX .|. simm8 simm)

sl_i   rX uimm = write 0b101100 (register rX .|. uimm8 uimm)
sr_i   rX uimm = write 0b101101 (register rX .|. uimm8 uimm)
sra_i  rX uimm = write 0b101110 (register rX .|. uimm8 uimm)
ld_i   rX uimm = write 0b101111 (register rX .|. uimm8 uimm)

jmpi    simm = write 0b110000 (simm10 simm)
jmpi_ct simm = write 0b110001 (simm10 simm)
jmpi_cf simm = write 0b110010 (simm10 simm)
wait    uimm = write 0b110011 (uimm10 uimm)

not rX = write 0b111000 (register rX)
abs rX = write 0b111001 (register rX)
halt   = write 0b000000 0

pad :: Int -> [SBool]
pad k = replicate k false

opcode :: Opcode -> InstructionCode
opcode o = fromBitsLE $ pad 10 ++ (take 6 $ blastLE o)

decodeOpcode :: InstructionCode -> Opcode
decodeOpcode c = fromBitsLE $ (drop 10 $ blastLE c) ++ pad 2

register :: Register -> InstructionCode
register r = fromBitsLE $ pad 8 ++ (take 2 $ blastLE r) ++ pad 6

address :: MemoryAddress -> InstructionCode
address a = fromBitsLE $ blastLE a ++ pad 8

simm8 :: SImm8 -> InstructionCode
simm8 s = fromBitsLE $ blastLE s ++ pad 8

simm10 :: SImm10 -> InstructionCode
simm10 s = fromBitsLE $ (take 10 $ blastLE s) ++ pad 6

uimm8 :: UImm8 -> InstructionCode
uimm8 u = fromBitsLE $ blastLE u ++ pad 8

uimm10 :: UImm10 -> InstructionCode
uimm10 = fromBitsLE . blastLE
