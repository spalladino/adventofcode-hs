module Day02
    ( day02,
      calculateChecksum
    ) where

import Data.List

day02 :: IO ()
day02 = interact calculateChecksum

calculateChecksum :: String -> String
calculateChecksum xs = show $ (countRepeating 2 ids) * (countRepeating 3 ids)
      where ids = (lines xs)

countRepeating :: Int -> [String] -> Int
countRepeating n xs = length $ filter (hasRepeating n) xs

hasRepeating :: Int -> String -> Bool
hasRepeating n = not . null . (filter ((==n) . length)) . group . sort