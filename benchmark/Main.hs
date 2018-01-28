module Main where

import Redfin.Examples.Energy

main :: IO ()
main = do
    benchmarkEnergyEstimate


benchmarkEnergyEstimate :: IO ()
benchmarkEnergyEstimate = do
    putStrLn "Energy estimate theorems"
    putStrLn "HighLevel overflows with unbounded t_k and p_k"
    print =<< highLevelFaultyExample
    putStrLn ""
    putStrLn "HighLevel doesn't overflows with bounded t_k and p_k"
    print =<< highLevelCorrect
    putStrLn ""
    putStrLn "LowLevel is equivalent to HighLevel"
    print =<< equivalence
    putStrLn ""
