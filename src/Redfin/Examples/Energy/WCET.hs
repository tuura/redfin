module Redfin.Examples.Energy.WCET where

import           Data.SBV
import           Prelude                    hiding (read)
import           Text.Pretty.Simple         (pPrint)

import           Redfin.Assembly            hiding (abs, div)
import qualified Redfin.Assembly            as Assembly
import           Redfin.Examples.Common
import           Redfin.Examples.Energy
import           Redfin.Language.Expression
import           Redfin.Simulate
import           Redfin.Types

worstCaseClock :: IO OptimizeResult
worstCaseClock = optimize Lexicographic $ do
    t1 <- exists "t1"
    t2 <- exists "t2"
    p1 <- exists "p1"
    p2 <- exists "p2"
    mem <- mkMemory "memory" [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
    let steps = 100
    emptyRegs <- mkRegisters "registers" []
    emptyFlags <- mkFlags "flags" []
    progLowLevel <- assemble energyEstimateLowLevel
    progHighLevel <- assemble energyEstimateHighLevel
    let finalStateLL = simulate steps $ boot progLowLevel emptyRegs mem emptyFlags
        finalStateHL = simulate steps $ boot progHighLevel emptyRegs mem emptyFlags
    maximize "Max clock HL" $ clock finalStateHL
    maximize "Max clock LL" $ clock finalStateLL
    minimize "Min clock HL" $ clock finalStateHL
    minimize "Min clock LL" $ clock finalStateLL
