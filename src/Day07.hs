module Day07
    ( day07,
      maxThrust,
      thrust,
      maxThrustWithFeedbackLoop,
      thrustWithFeedbackLoop,
      setupAmplifiers,
      runAmplifiers
    ) where

import Utils.Parse
import Data.List
import Data.Maybe
import Data.Function
import Data.List.Split
import Day05

-- |https://adventofcode.com/2019/day/7
day07 :: IO ()
day07 = interact $ show . maxThrustWithFeedbackLoop . readCSList

-- Part 02

maxThrustWithFeedbackLoop :: Program -> Int
maxThrustWithFeedbackLoop program = maximum $ map (thrustWithFeedbackLoop program) (permutations [5..9])

thrustWithFeedbackLoop :: Program -> [Int] -> Int
thrustWithFeedbackLoop program settings = getThrust $ head $ dropWhile isntFeedbackEnded iterations
    where getThrust (execs, output) = head output
          iterations = iterate runAmplifiers (setupAmplifiers program settings, [0])
          isntFeedbackEnded (execs, output) = not $ isEnded (last execs)
    
runAmplifiers :: ([Execution], Input) -> ([Execution], Output)
runAmplifiers (execs, input) = (executions, lastOutput)
    where executions = map fst executionsWithOutputs
          lastOutput = snd $ last executionsWithOutputs
          executionsWithOutputs = tail $ scanl runAmplifier (NilExecution, input) execs
          runAmplifier (previous, output) = consumeOutput . intcodeRun output

setupAmplifiers :: Program -> [Int] -> [Execution]
setupAmplifiers program = map (initialState program . return)

-- Part 01

maxThrust :: Program -> Int
maxThrust program = maximum $ map (thrust program 0) (permutations [0..4])

thrust :: Program -> Int -> [Int] -> Int
thrust program input settings = foldl' (&) input $ map (singleInputAmplifier program) settings

singleInputAmplifier :: Program -> Int -> Int -> Int
singleInputAmplifier program setting input = head . output $ intcode [setting, input] program