module Main where

import System.Directory (withCurrentDirectory)
import Data.SBV
import qualified Redfin.Examples.Energy            as Energy
import qualified Redfin.Examples.Sum               as Sum
import qualified Redfin.Examples.ManhattanDistance as Distance

prover :: FilePath -> SMTConfig
prover logFile =
        z3 { verbose = True
            , redirectVerbose = Just logFile
            , timing = PrintTiming
            , printBase = 10
            }

main :: IO ()
main =
    withCurrentDirectory "./smt_logs" $ do
        benchmarkEnergyEstimate
        benchmarkArraySum
        benchmarkManhattanDistance

benchmarkEnergyEstimate :: IO ()
benchmarkEnergyEstimate = do
    putStrLn "---------------------------------"
    putStrLn "Energy estimate theorems"
    putStrLn "---------------------------------"
    putStrLn "HighLevel overflows with unbounded t_k and p_k"
    print =<< proveWith (prover "energyEstimate_Faulty.smt2")
                        Energy.highLevelFaultyExample
    putStrLn ""
    putStrLn "HighLevel doesn't overflows with bounded t_k and p_k"
    print =<< proveWith (prover "energyEstimate_Correct.smt2")
                        Energy.highLevelCorrect
    putStrLn ""
    putStrLn "LowLevel is equivalent to HighLevel"
    print =<< proveWith (prover "energyEstimate_LLtoHLEquivalence.smt2")
                        Energy.equivalence
    putStrLn ""

benchmarkArraySum :: IO ()
benchmarkArraySum = do
    putStrLn "---------------------------------"
    putStrLn "Array sum theorems, n = 10"
    putStrLn "---------------------------------"
    putStrLn "Overflows with unbounded x_k"
    print =<< proveWith (prover "sum_Faulty.smt2")
                        Sum.faultyExample
    putStrLn ""
    putStrLn "Doesn't overflows with x_k in [0, 1000]"
    print =<< proveWith (prover "sum_Correct.smt2")
                        Sum.noOverflow
    putStrLn ""
    putStrLn "The program is is equivalent to the Haskell function"
    print =<< proveWith (prover "sum_HLtoHaskellEquivalence.smt2")
                        Sum.equivHaskell
    putStrLn ""

benchmarkManhattanDistance :: IO ()
benchmarkManhattanDistance = do
    putStrLn "---------------------------------"
    putStrLn "Manhattan distance theorems, n = 4"
    putStrLn "---------------------------------"
    putStrLn "Overflows with unbounded x_k, y_k"
    print =<< proveWith (prover "distance_Faulty.smt2")
                        Distance.faultyExample
    putStrLn ""
    putStrLn "Doesn't overflows with x_k, y_k in [0, 1000]"
    print =<< proveWith (prover "distance_Correct.smt2")
                        Distance.noOverflow
    putStrLn ""
    putStrLn "The program is is equivalent to the Haskell function"
    print =<< proveWith (prover "distance_HLtoHaskellEquivalence.smt2")
                        Distance.equivHaskell
    putStrLn ""