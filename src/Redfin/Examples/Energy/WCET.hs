module Redfin.Examples.Energy.WCET where

import           Data.SBV                   hiding (SFunArray, SymArray (..),
                                             ( # ), (%))
import           Prelude                    hiding (read)
import           Redfin.SBV
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
    let mem = mkMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, 100)]
        steps = 100
        progLL = assemble energyEstimateLowLevel
        progHL = assemble energyEstimateHighLevel
        initialStateLL = boot progLL defaultRegisters mem defaultFlags
        initialStateHL = boot progHL defaultRegisters mem defaultFlags
        finalStateLL = simulate steps initialStateLL
        finalStateHL = simulate steps initialStateHL
    maximize "Max clock HL" $ clock finalStateHL
    maximize "Max clock LL" $ clock finalStateLL
    minimize "Min clock HL" $ clock finalStateHL
    minimize "Min clock LL" $ clock finalStateLL
