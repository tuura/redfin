module Main where

import Test.QuickCheck (quickCheck)
import Redfin.Data.Fixed

eps :: Double
eps = 0.01

-- | Check the adequacy of conversion from/to 'Double'.
conversion_reversible :: Double -> Bool
conversion_reversible x =
    let x' = (fromFixed . toFixed) x
    in abs (x - x') < eps

main = quickCheck conversion_reversible