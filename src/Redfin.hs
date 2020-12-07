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
    -- * Redfin state transformer
    Redfin (..), transformState, readState, writeState,
    readRegister, writeRegister,
    readMemory, writeMemory,
    readFlag, writeFlag,
    readProgram, readInstructionRegister, writeInstructionRegister,
    fetchInstruction, incrementInstructionCounter,
    delay, toMemoryAddress
) where

import           Control.Monad
import           Data.Map        (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.SBV        hiding (SFunArray, SymArray (..))
import           Redfin.SBV


import           Redfin.Types

-- | The Redfin state transformer.
newtype Redfin a = Redfin { transform :: State -> (a, State) } deriving Functor

-- | A standard 'Applicative' instance available for any 'Monad'.
instance Applicative Redfin where
    pure  = return
    (<*>) = ap

-- | A standard state 'Monad'.
instance Monad Redfin where
    return a       = Redfin $ \s -> (a, s)
    Redfin r >>= f = Redfin $ \s -> let (a, s') = r s in transform (f a) s'

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

-- | Lookup the 'Value' at the given 'MemoryAddress'.
-- If the value has never been
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
toMemoryAddress :: Value -> Redfin MemoryAddress
toMemoryAddress value = do
    let valid = value .< 256
    transformState $ \s ->
      ite valid s (snd $ transform (writeFlag OutOfMemory sTrue) s)
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
    pure $ readArray (program state) address
    -- case Map.lookup a (program state) of
    --   Just i  -> return (literal i)
    --   Nothing -> do writeFlag OutOfProgram sTrue
    --                 return 0

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
