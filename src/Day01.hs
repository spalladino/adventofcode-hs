module Day01
    ( day01,
      fuel,
      totalfuel,
      sumfuel
    ) where

import Utils.Parse

-- |https://adventofcode.com/2019/day/1

fuelfn :: Integer -> Integer
fuelfn = totalfuel

day01 :: IO ()
day01 = interact $ show . (sumfuel fuelfn) . (map toInt) . lines

fuel :: Integer -> Integer
fuel = (max 0) . (subtract 2) . floor . (/ 3) . fromIntegral

totalfuel :: Integer -> Integer
totalfuel = sum . tail . takeWhile (> 0) . iterate fuel

sumfuel :: (Integer -> Integer) -> [Integer] -> Integer
sumfuel f = sum . map f