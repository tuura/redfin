module Main where

import qualified Redfin.Examples.Energy            as Energy
import qualified Redfin.Examples.Sum               as Sum
import qualified Redfin.Examples.ManhattanDistance as Distance

main :: IO ()
main = do
    benchmarkEnergyEstimate
    benchmarkArraySum
    benchmarkManhattanDistance

benchmarkEnergyEstimate :: IO ()
benchmarkEnergyEstimate = do
    putStrLn "---------------------------------"
    putStrLn "Energy estimate theorems"
    putStrLn "---------------------------------"
    putStrLn "HighLevel overflows with unbounded t_k and p_k"
    print =<< Energy.highLevelFaultyExample
    putStrLn ""
    putStrLn "HighLevel doesn't overflows with bounded t_k and p_k"
    print =<< Energy.highLevelCorrect
    putStrLn ""
    putStrLn "LowLevel is equivalent to HighLevel"
    print =<< Energy.equivalence
    putStrLn ""

benchmarkArraySum :: IO ()
benchmarkArraySum = do
    putStrLn "---------------------------------"
    putStrLn "Array sum theorems, n = 10"
    putStrLn "---------------------------------"
    putStrLn "Overflows with unbounded x_k"
    print =<< Sum.faultyExample
    putStrLn ""
    putStrLn "Doesn't overflows with x_k in [0, 1000]"
    print =<< Sum.noOverflow
    putStrLn ""
    putStrLn "The program is is equivalent to the Haskell function"
    print =<< Sum.equivHaskell
    putStrLn ""

benchmarkManhattanDistance :: IO ()
benchmarkManhattanDistance = do
    putStrLn "---------------------------------"
    putStrLn "Manhattan distance theorems, n = 4"
    putStrLn "---------------------------------"
    putStrLn "Overflows with unbounded x_k, y_k"
    print =<< Distance.faultyExample
    putStrLn ""
    putStrLn "Doesn't overflows with x_k, y_k in [0, 1000]"
    print =<< Distance.noOverflow
    putStrLn ""
    putStrLn "The program is is equivalent to the Haskell function"
    print =<< Distance.equivHaskell
    putStrLn ""