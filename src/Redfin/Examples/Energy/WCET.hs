module Redfin.Examples.Energy.WCET where

import Prelude hiding (read)
import Text.Pretty.Simple (pPrint)
import Data.SBV
import Redfin
import Redfin.Assembly hiding (div, abs)
import qualified Redfin.Assembly as Assembly
import Redfin.Simulate
import Redfin.Language.Expression
import Redfin.Examples.Common


-- adder_ld_si :: Script
-- adder_ld_si = do
--     let x = 0
--     ld_si r0 20
--     add r0 x

-- adder_ld :: Script
-- adder_ld = do
--     let x = 0
--         y = 1
--     ld r0 1
--     add r0 x

-- worstCaseClock :: IO OptimizeResult
-- worstCaseClock = optimize Lexicographic $ do
--     x <- sInt64 "x"
--     y <- sInt64 "y"
--     let mem = initialiseMemory [(0, x), (1, y)]
--         steps = 100
--         finalStateHL = simulate steps $ boot energyEstimate mem
--         finalStateLL = simulate steps $ boot energyEstimateLowLevel mem
--     -- maximize "Max clock HL" $ clock finalStateHL
--     -- maximize "Max clock LL" $ clock finalStateLL
--     -- minimize "Min clock HL" $ clock finalStateHL
--     minimize "Min clock LL" $ clock finalStateLL