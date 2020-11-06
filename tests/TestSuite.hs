{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.SBV

import qualified Redfin.Types as Types

main :: IO ()
main = sequence_
  [ fromSImm8Correct
  , fromSImm10Correct
  , fromUImm8Correct
  , fromUImm10Correct
  ]

fromSImm8Correct = prove $ \x ->
  let y = Types.fromSImm8 (x :: Types.SImm8)
  in  y `inRange` (minBound :: Types.Value
                  , maxBound :: Types.Value)

fromSImm10Correct = prove $ \x ->
  let y = Types.fromSImm10 (x :: Types.SImm10)
  in  y `inRange` ( minBound :: Types.InstructionAddress
                  , maxBound :: Types.InstructionAddress)

fromUImm8Correct = prove $ \x ->
  let y = Types.fromUImm8 (x :: Types.UImm8)
  in  y `inRange` (minBound :: Types.Value, maxBound :: Types.Value)

fromUImm10Correct = prove $ \x ->
  let y = Types.fromUImm10 (x :: Types.UImm10)
  in  y `inRange` (minBound :: Types.Value, maxBound :: Types.Value)


-----------------------------------------------------------------------------
