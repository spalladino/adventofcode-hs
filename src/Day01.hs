module Day01
    ( day01,
      calculateFrequency,
      findRepeatedFrequency
    ) where

day01 :: IO ()
day01 = interact findRepeatedFrequency

calculateFrequency :: String -> String
calculateFrequency = show . sum . toIntegers

findRepeatedFrequency :: String -> String
findRepeatedFrequency = show . findRepeated . frequencies
  where frequencies = (scanl (+) 0) . cycle . toIntegers

findRepeated :: [Integer] -> Integer
findRepeated (x:xs) = findRepeatedAux [] x xs

findRepeatedAux :: [Integer] -> Integer -> [Integer] -> Integer
findRepeatedAux ps x (r:rs)
      | elem x ps = x
      | otherwise = findRepeatedAux (x:ps) r rs

toIntegers :: String -> [Integer]
toIntegers = (map toSignedInteger) . lines

toSignedInteger :: String -> Integer
toSignedInteger ('+':num) = read num :: Integer
toSignedInteger ('-':num) = - (read num :: Integer)
toSignedInteger _ = 0
