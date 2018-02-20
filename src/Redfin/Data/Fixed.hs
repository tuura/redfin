-----------------------------------------------------------------------------
-- |
-- Module      :  Redfin.Data.Fixed
-- Copyright   :  (c) Andrey Mokhov, Georgy Lukyanov 2017
--
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- Symbolic fixed-point numbers and arithmetic.
-- The number format is currently fixed to Q56.8,
-- signed two's complement format with 56 integer and 8 fractional bits.
-- It is capable of storing numbers from (approx.) segment [-2 ^ 55, 2 ^ 55 - 1]
-- Implemented on top of 'Data.SBV.SInt64' --- signed two's complement integers.
-----------------------------------------------------------------------------
module Redfin.Data.Fixed (
  -- * Fixed-point number data type
  Fixed (..), fracBits,

  -- * Conversion to/from 'Double' for printing/initialisation
  toFixed, fromFixed,

  -- * Conversion to/from 'Redfin.Value'. make sure not to lose any information!
  unsafeValueToFixed, unsafeFixedToValue
  ) where

import Test.QuickCheck.Arbitrary (Arbitrary)
import Data.SBV
import Redfin (Value)

-- | Symbolic fixed-point numbers implemented on top of 'type Value = Data.SBV.SInt64'.
newtype Fixed = Fixed { getFixed :: Value }
    deriving (Bounded, EqSymbolic, Mergeable, OrdSymbolic, Arbitrary)

fracBits :: Word8
fracBits = 8

-- | Num instance is strait-forward, the only tricky part is multiplication,
--   which is performed via unbounded symbolic integer 'Data.SBV.SInteger' data type.
--   The product later gets shifted to fit into a 64-bit value.
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

-- | Unsafe conversion from integer 'Redfin.Value' to a fixed-point number by shifting the
--   integer part. Use with caution.
unsafeValueToFixed :: Value -> Fixed
unsafeValueToFixed x = Fixed $ sShiftLeft x (literal fracBits)

-- | Unsafe conversion from fixed-point to integer 'Redfin.Value' by shifting the
--   fractional part out. Use with caution.
unsafeFixedToValue :: Fixed -> Value
unsafeFixedToValue (Fixed x) =
  sShiftRight x (literal fracBits)

--------------------------- Show-related ---------------------------------------
-- | Convert from a 'Fixed' precision value to a 'Double'.
--   Used in the 'Show' instance.
fromFixed :: Fixed -> Double
fromFixed (Fixed f) =
    case unliteral f of
        Just x -> fromIntegral x / (2 ^ fracBits)
        Nothing -> error "fromFixed: non-literal value."

-- | Convert from a 'Double' to a 'Fixed' precision value
--   Used for literal initialisation.
toFixed :: Double -> Fixed
toFixed x = Fixed . literal $ floor (x * (2 ^ fracBits) + 0.5)

instance Show Fixed where
  showsPrec d = showsPrec d . fromFixed