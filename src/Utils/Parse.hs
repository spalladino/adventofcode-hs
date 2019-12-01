module Utils.Parse 
  ( toInt
  ) where

toInt :: String -> Integer
toInt s = read s :: Integer