module Day07
    ( day07,
      maxThrust,
      amplifiersThrust
    ) where

import Utils.Parse
import Data.List
import Data.Maybe
import Data.Function
import Data.List.Split
import Day05

-- |https://adventofcode.com/2019/day/7
day07 :: IO ()
day07 = interact $ show . maxThrust . readCSList

maxThrust :: Program -> Int
maxThrust program = maximum $ map (amplifiersThrust program) (permutations [0..4])

amplifiersThrust :: Program -> [Int] -> Int
amplifiersThrust program settings = foldl' (&) 0 $ map (amplifier program) settings

amplifier :: Program -> Int -> Int -> Int
amplifier program setting input = head . output $ intcode [setting, input] program