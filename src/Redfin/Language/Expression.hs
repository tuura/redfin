{-# LANGUAGE DataKinds, FlexibleInstances, GADTs #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Language.Expression
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2018
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- A high-level expression language built on top of REDFIN assembly.
--
-----------------------------------------------------------------------------
module Redfin.Language.Expression (
    -- * Stack
    Stack (..), push, pop,

    -- * Typed memory locations
    Variable (..), Temporary (..), read,

    -- * Expressions
    Expression (..), compile
    )where

import Data.SBV
import Prelude hiding (and, div, not, or, abs, read)

import qualified Prelude (abs, div)

import Redfin
import Redfin.Assembly
import qualified Redfin.Data.Fixed as Fixed

-- | We distinguish between fixed-point and integer values
data ValueType = FPType | IntType

-- | Temporary, Stack and Variable are all semantically different memory addresses
newtype Temporary = Temporary MemoryAddress
newtype Stack     = Stack     MemoryAddress

data Variable :: ValueType -> * where
    IntegerVariable    :: MemoryAddress -> Variable IntType
    FixedPointVariable :: MemoryAddress -> Variable FPType

-- | Literal values get compiled to immediate arguments
data Literal :: ValueType -> * where
    IntegerLiteral    :: SImm8 -> Literal IntType
    FixedPointLiteral :: SImm8 -> Literal FPType

-- | Pushes the value stored in the register to the stack, advances the stack
--   pointer, and destroys the value stored in the register.
push :: Register -> Stack -> Script
push reg (Stack pointer) = do
    stmi reg pointer
    ld reg pointer
    add_si reg 1
    st reg pointer

-- | Decrements the stack pointer, and loads the value from the top of the stack
--   into the given register.
pop :: Register -> Stack -> Script
pop reg (Stack pointer) = do
    ld reg pointer
    sub_si reg 1
    st reg pointer
    ldmi reg pointer

type BinaryOperator = Register -> MemoryAddress -> Script

-- | Applies a binary operation, such as add, to the two top values stored in
--   stack and returns the result in a register
applyBinary :: Register -> Stack -> Temporary -> BinaryOperator -> Script
applyBinary reg stack (Temporary tmp) op = do
    pop reg stack
    st reg tmp
    pop reg stack
    op reg tmp

-- TODO: Switch from Lit to Imm :: Immediate a -> Expression a
-- TODO: Add support for register-immediate operators
-- TODO: Generalise Abs to arbitrary unary constructors
data Expression :: ValueType -> * where
    Lit :: Literal  a     -> Expression a
    Var :: Variable a     -> Expression a
    Bin :: BinaryOperator -> Expression a -> Expression a -> Expression a
    Abs :: Expression a   -> Expression a

instance Num (Expression IntType) where
    fromInteger = Lit . IntegerLiteral . fromIntegral
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
    fromInteger = Lit . FixedPointLiteral . fromInteger
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

-- | Compile high-level expression to assembly.
compile :: Register -> Stack -> Temporary -> Expression a -> Script
compile reg stack tmp expr = case expr of
    Lit (IntegerLiteral    value) -> ld_si reg value
    Lit (FixedPointLiteral value) -> do
        ld_si reg value
        sl_i  reg (literal Fixed.fracBits) -- interpret literal value as a fixed-point number
    Var (IntegerVariable    var) -> ld reg var
    Var (FixedPointVariable var) -> ld reg var
    Bin op x y -> do
        compile reg stack tmp x
        push reg stack
        compile reg stack tmp y
        push reg stack
        applyBinary reg stack tmp op
    Abs x -> do
        compile reg stack tmp x
        abs reg
