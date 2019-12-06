module Utils.Parse 
  ( toInt
  ) where

toInt :: String -> Int
toInt s = read s :: Int