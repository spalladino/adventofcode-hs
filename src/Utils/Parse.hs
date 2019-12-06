module Utils.Parse 
  ( toInt,
    readIntegerList
  ) where

import Data.List.Split

toInt :: String -> Int
toInt s = read s :: Int

readIntegerList :: String -> [Integer]
readIntegerList = map (\x -> read x :: Integer) . splitOn ","