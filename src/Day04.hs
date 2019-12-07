module Day04
    ( day04,
      hasDoubleDigits,
      hasExactlyDoubleDigits
    ) where

import Utils.Parse
import Utils.Data
import Data.List
import Data.List.Split

-- |https://adventofcode.com/2019/day/4
day04 :: IO ()
day04 = interact $ show . length . inRange ([1,7,8,4,1,6], [6,7,6,4,6,1]) . const validPasswords

inRange :: Ord a => (a, a) -> [a] -> [a]
inRange (a, b) = filter (> a) . filter (< b)

validPasswords :: [[Int]]
validPasswords = filter hasExactlyDoubleDigits [
  [a,b,c,d,e,f] |
      a <- [1..6],
      b <- [a..9],
      c <- [b..9],
      d <- [c..9],
      e <- [d..9],
      f <- [e..9]]

hasDoubleDigits :: [Int] -> Bool
hasDoubleDigits (x:y:xs) = x == y || hasDoubleDigits (y:xs)
hasDoubleDigits _ = False

hasExactlyDoubleDigits :: [Int] -> Bool
hasExactlyDoubleDigits = hasExactlyDoubleDigits' 0

hasExactlyDoubleDigits' :: Int -> [Int] -> Bool
hasExactlyDoubleDigits' m (x:y:xs)
    | m == x = hasExactlyDoubleDigits' m (y:xs)
    | x /= y = hasExactlyDoubleDigits' x (y:xs)
    | x == y = (null xs || head xs > x) || hasExactlyDoubleDigits' x xs
hasExactlyDoubleDigits' _ _ = False