-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Data.Fixed
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- Fixed-point numbers and arithmetic.
-- The number format is currently fixed to Q56.8,
-- signed two's complement format with 56 integer and 8 fractional bits.
-----------------------------------------------------------------------------
module Redfin.Data.Fixed (
  -- * Fixed-point number data type
  Fixed (..),

  -- * Conversion to/from Double for printing/initialisation
  toFixed, fromFixed, unsafeValueToFixed, unsafeFixedToValue, fracBits
  ) where

import Test.QuickCheck.Arbitrary
import Data.SBV
import Redfin

newtype Fixed = Fixed { getFixed :: Value }
    deriving (EqSymbolic, Mergeable, OrdSymbolic, Arbitrary)

fracBits :: Word8
fracBits = 8

instance Num Fixed where
  Fixed a + Fixed b = Fixed (a + b)
  Fixed a - Fixed b = Fixed (a - b)
  negate = negate
  abs = abs
  signum (Fixed a)  = Fixed $ sShiftLeft (signum a) (literal fracBits)
  Fixed a * Fixed b =
    Fixed . sFromIntegral $
        (sShiftRight (sFromIntegral a * sFromIntegral b) (literal fracBits) :: SInteger)
  fromInteger i     = Fixed $ sShiftLeft (fromInteger i) (literal fracBits)

instance Fractional Fixed where
  Fixed a / Fixed b  = Fixed $
    sFromIntegral $
        (sShiftLeft (sFromIntegral a) (literal fracBits) `sDiv` sFromIntegral b :: SInteger)
  fromRational a = Fixed $
    (sShiftLeft (fromInteger $ numerator a) (literal fracBits) `sDiv` fromInteger (denominator a))

--------------------------- Show-related ---------------------------------------
-- | Convert from a 'Fixed' precision value to a 'Double'
fromFixed :: Fixed -> Double
fromFixed (Fixed f) =
    case unliteral f of
        Just x -> fromIntegral x / (2 ^ fracBits)
        Nothing -> error "fromFixed: non-literal value."

-- | Convert from a 'Double' to a 'Fixed' precision value
toFixed :: Double -> Fixed
toFixed x = Fixed . literal $ floor (x * (2 ^ fracBits) + 0.5)

unsafeValueToFixed :: Value -> Fixed
unsafeValueToFixed x = Fixed $ sShiftLeft x (literal fracBits)

unsafeFixedToValue :: Fixed -> Value
unsafeFixedToValue (Fixed x) =
  sShiftRight x (literal fracBits)

instance Show Fixed where
  showsPrec d = showsPrec d . fromFixed