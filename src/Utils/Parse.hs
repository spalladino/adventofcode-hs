module Utils.Parse 
  ( toInt,
    readIntegerCSList,
    readCSList,
    readCharAsInt
  ) where

import Data.List.Split

toInt :: String -> Int
toInt s = read s :: Int

readIntegerCSList :: String -> [Integer]
readIntegerCSList = map (\x -> read x :: Integer) . splitOn ","

readCSList :: (Read a) => String -> [a]
readCSList = map read . splitOn ","

readCharAsInt :: Char -> Int
readCharAsInt '0' = 0
readCharAsInt '1' = 1
readCharAsInt '2' = 2
readCharAsInt '3' = 3
readCharAsInt '4' = 4
readCharAsInt '5' = 5
readCharAsInt '6' = 6
readCharAsInt '7' = 7
readCharAsInt '8' = 8
readCharAsInt '9' = 9
