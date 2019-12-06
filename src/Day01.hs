module Day01
    ( day01,
      fuel,
      totalfuel,
      sumfuel
    ) where

import Utils.Parse

-- |https://adventofcode.com/2019/day/1
day01 :: IO ()
day01 = interact $ show . (sumfuel fuelfn) . (map toInt) . lines
      where fuelfn = totalfuel

fuel :: Int -> Int
fuel = (max 0) . (subtract 2) . floor . (/ 3) . fromIntegral

totalfuel :: Int -> Int
totalfuel = sum . tail . takeWhile (> 0) . iterate fuel

sumfuel :: (Int -> Int) -> [Int] -> Int
sumfuel f = sum . map f