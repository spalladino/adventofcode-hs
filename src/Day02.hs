module Day02
    ( day02,
      calculateChecksum,
      findCommonLetters
    ) where

import Data.List

day02 :: IO ()
day02 = interact findCommonLetters

calculateChecksum :: String -> String
calculateChecksum xs = show $ (countRepeating 2 ids) * (countRepeating 3 ids)
      where ids = (lines xs)

countRepeating :: Int -> [String] -> Int
countRepeating n xs = length $ filter (hasRepeating n) xs

hasRepeating :: Int -> String -> Bool
hasRepeating n = not . null . (filter ((==n) . length)) . group . sort

findCommonLetters :: String -> String
findCommonLetters = getCommonLetters . findDiffByOne . lines

getCommonLetters :: (String, String) -> String
getCommonLetters (a, b) = map fst $ filter areEq (zip a b)
      where areEq (x, y) = x == y

findDiffByOne :: [String] -> (String, String)
findDiffByOne xs = head [(x,y) | x <- xs, y <- xs, x /= y, 1 == (countDiff x y)]

countDiff :: String -> String -> Int
countDiff a b = length $ filter areNeq (zip a b)
      where areNeq (x, y) = x /= y