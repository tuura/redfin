module Main where

import           Data.Functor                      (void)
import           Data.SBV
import           Data.Time.Clock
import           Redfin.Assembly                   hiding (abs, div)
import           System.Directory                  (createDirectory,
                                                    withCurrentDirectory)

import qualified Redfin.Examples.Energy            as Energy
import qualified Redfin.Examples.ManhattanDistance as Distance
import qualified Redfin.Examples.Sum               as Sum

prover :: FilePath -> SMTConfig
prover logFile =
        z3 { verbose = True
           , redirectVerbose = Just logFile
           , timing = PrintTiming
           , printBase = 10
           }

main :: IO ()
main = do
    timestamp <- getCurrentTime
    let dirname = "smt_logs_" ++ show timestamp
    createDirectory dirname
    withCurrentDirectory dirname $ do
        benchmarkEnergyEstimate Energy.energyEstimateHighLevel
        mapM_ (\n -> benchmarkArraySum (Sum.sumArrayHighLevel n) n)
                     [6, 9, 12, 15, 18]
        mapM_ (benchmarkArraySum (Sum.sumArrayLowLevel)) [6, 9, 12, 15, 18]
        benchmarkManhattanDistance

discardModel :: String -> String
discardModel = unlines . take 1 . lines

fileLength :: FilePath -> IO Int
fileLength = (length . lines <$>) . readFile

benchmarkEnergyEstimate :: Script -> IO ()
benchmarkEnergyEstimate src = do
    putStrLn "--------------------------------------------------------------------------------------"
    putStr $ "energy_find_overflow , " ++ show 0 ++ ", "
    void $ proveWith (prover "energy_find_overflow.smt2")
          (Energy.faultyExample src)
    putStrLn . show =<< fileLength "energy_find_overflow.smt2"

    putStr $ "energy_correct , " ++ show 0 ++ ", "
    void $ proveWith (prover "energy_correct.smt2")
          (Energy.correct src)
    putStrLn . show =<< fileLength "energy_correct.smt2"

    putStr $ "energy_equiv_hs_spec, " ++ show 0 ++ ", "
    void $ proveWith (prover "energy_equiv_hs_spec.smt2")
          (Energy.equivHaskell src)
    putStrLn . show =<< fileLength "energy_equiv_hs_spec.smt2"

benchmarkArraySum :: Script -> Int -> IO ()
benchmarkArraySum src arraySize = do
    putStrLn "--------------------------------------------------------------------------------------"
    putStr $ "find_overflow, " ++ show arraySize ++ ", "
    void $ proveWith (prover "sum_Faulty.smt2")
          (Sum.faultyExample src arraySize)
    putStrLn . show =<< fileLength "sum_Faulty.smt2"
    putStrLn "--------------------------------------------------------------------------------------"
    putStr $ "prove_no_overflow, " ++ show arraySize ++ ", "
    void $ proveWith (prover "sum_Correct.smt2")
          (Sum.noOverflow src arraySize)
    putStrLn . show =<< fileLength "sum_Correct.smt2"
    putStrLn "--------------------------------------------------------------------------------------"
    putStr $ "equiv_hs_spec, " ++ show arraySize ++ ", "
    void $ proveWith (prover "sum_HLtoHaskellEquivalence.smt2")
          (Sum.equivHaskell src arraySize)
    putStrLn . show =<< fileLength "sum_HLtoHaskellEquivalence.smt2"

benchmarkManhattanDistance :: IO ()
benchmarkManhattanDistance = do
    putStrLn "--------------------------------------------------------------------------------------"
    putStrLn "Manhattan distance theorems, n = 4"
    putStrLn "--------------------------------------------------------------------------------------"
    putStrLn "Overflows with unbounded x_k, y_k"
    putStrLn . discardModel . show =<< proveWith (prover "distance_Faulty.smt2")
            (Distance.faultyExample Distance.distanceHighLevel)
    print =<< fileLength "distance_Faulty.smt2"
    putStrLn ""

    putStrLn "Doesn't overflow with x_k, y_k in [0, 1000]"
    putStrLn . discardModel . show =<< proveWith (prover "distance_Correct.smt2")
                        (Distance.noOverflow Distance.distanceHighLevel)
    print =<< fileLength "distance_Correct.smt2"
    putStrLn ""

    putStrLn "The program is is equivalent to the Haskell function"
    putStrLn . discardModel . show =<< proveWith (prover "distance_HLtoHaskellEquivalence.smt2")
                        (Distance.equivHaskell Distance.distanceHighLevel)
    print =<< fileLength "distance_HLtoHaskellEquivalence.smt2"
    putStrLn ""
