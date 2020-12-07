{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Language.Expression
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2018-2020
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- A high-level expression language built on top of REDFIN assembly.
--
-----------------------------------------------------------------------------
module Redfin.Language.Expression (
  -- * Stack and Temporary are typed memory locations
  stack, temporary,

  -- * Expressions
  Expression (..), varAtAddress,

  -- * Compiler
  initCompiler, compile
  ) where

import           Control.Monad.State.Class
import           Data.List                 (intersect)
import           Data.Maybe                (fromJust)
import           Data.SBV
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Prelude                   hiding (abs, and, div, not, or, read)

import qualified Prelude                   (abs, div, not)

import           Redfin.Assembly
import qualified Redfin.Data.Fixed         as Fixed
import           Redfin.Types

-- | Temporary, Stack and Variable are all semantically different memory addresses
newtype Temporary = MkTemporary { fromTemporary :: MemoryAddress }

temporary :: MemoryAddress -> Temporary
temporary = MkTemporary

data Stack = MkStack { _pointer :: MemoryAddress
                     , _size    :: WordN 4
                     }

stack :: MemoryAddress -> Stack
stack addr = MkStack addr maxBound

data Variable where
    MkVariable :: MemoryAddress -> Variable

-- | Literal values get compiled to immediate arguments
data Literal where
    MkLiteral :: SImm8 -> Literal

-- | State of the compiler:
--   * the compiled assembly script
--   * a stack
--   * a list of temporary memory locations
data CompilerEnv =
  MkCompilerEnv { _reg   :: Register
                , _tmp   :: Temporary
                , _stack :: Stack
                }

-- | Initialise the compiler
--   The compiler needs one register for data manipulation, a
initCompiler :: Register -> Temporary -> Stack-> CompilerEnv
initCompiler = MkCompilerEnv

-- | Check if the compiler state is valid, considering REDFIN's restrictions
validate :: CompilerEnv -> Bool
validate s =
  let -- stack is in bounds
      stackValid = fromJust . unliteral $
        _pointer (_stack s) + (literal $ fromIntegral $_size (_stack s)) .<= literal addressSpace
      -- all heap addresses are in bound
      heapValid = heapSpace <= addressSpace
      noOverlap = Prelude.not (heapSpace `elem` stackSpace)
  in all id [stackValid, heapValid, noOverlap]
  where addressSpace = fromJust . unliteral $ maxBound @MemoryAddress
        programSpace = fromIntegral . fromJust $ unliteral (maxBound @InstructionAddress)
        heapSpace = (fromJust . unliteral) $ fromTemporary (_tmp s)
        stackSpace = map (fromJust . unliteral)
          [_pointer (_stack s)..
           _pointer (_stack s) + (literal . fromIntegral $ _size (_stack s))]

-- | Pushes the value stored in the register to the stack, advances the stack
--   pointer, and destroys the value stored in the register.
push :: Register -> Stack -> Script
push reg (MkStack pointer _) = do
    stmi reg pointer
    ld reg pointer
    add_si reg 1
    st reg pointer

-- | Decrements the stack pointer, and loads the value from the top of the stack
--   into the given register.
pop :: Register -> Stack -> Script
pop reg (MkStack pointer _) = do
    ld reg pointer
    sub_si reg 1
    st reg pointer
    ldmi reg pointer

type BinaryOperator = Register -> MemoryAddress -> Script

-- | Applies a binary operation, such as add, to the two top values stored in
--   stack and returns the result in a register
applyBinary :: CompilerEnv -> BinaryOperator -> Script
applyBinary (MkCompilerEnv reg (MkTemporary tmp) stack) op = do
    pop reg stack
    st reg tmp
    pop reg stack
    op reg tmp

data Expression where
    Lit :: Literal     -> Expression
    Var :: Variable     -> Expression
    Bin :: BinaryOperator -> Expression -> Expression -> Expression
    Abs :: Expression   -> Expression

instance Num Expression where
    fromInteger = Lit . MkLiteral . fromIntegral
    (+)         = Bin add
    (-)         = Bin sub
    (*)         = Bin mul
    abs         = Abs
    signum x    = x `Prelude.div` Prelude.abs x

instance Eq Expression where
    (==) = error "Eq cannot be implemented for Expression"

instance Ord Expression where
    compare = error "Ord cannot be implemented for Expression"

instance Real Expression where
    toRational = error "Real cannot be implemented for Expression"

instance Enum Expression where
    toEnum   = error "Enum cannot be implemented for Expression"
    fromEnum = error "Enum cannot be implemented for Expression"

instance Integral Expression where
    div       = Bin div
    quotRem   = error "quotRem is not implemented for Expression"
    toInteger = error "toInteger cannot be implemented for Expression"

varAtAddress :: MemoryAddress -> Expression
varAtAddress = Var . MkVariable

-- | Compile high-level expression to assembly.
compile :: CompilerEnv -> Expression -> Script
compile env expr = do
  compileExpr env expr
  halt
  where
    compileExpr :: CompilerEnv -> Expression -> Script
    compileExpr env@(MkCompilerEnv reg tmp stack) expr =
      case expr of
        Lit (MkLiteral value) -> ld_si reg value
        Var (MkVariable  var) -> ld reg var
        Bin op x y -> do
            compileExpr env x
            push reg stack
            compileExpr env y
            push reg stack
            applyBinary env op
        Abs x -> do
            compileExpr env x
            abs reg
