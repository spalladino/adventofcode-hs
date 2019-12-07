module Utils.Parse 
  ( toInt,
    readIntegerList,
    readCSList
  ) where

import Data.List.Split

toInt :: String -> Int
toInt s = read s :: Int

readIntegerList :: String -> [Integer]
readIntegerList = map (\x -> read x :: Integer) . splitOn ","

readCSList :: (Read a) => String -> [a]
readCSList = map read . splitOn ","