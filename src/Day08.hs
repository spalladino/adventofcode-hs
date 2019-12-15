module Day08
    ( day08,
      decodeImage,
      parseImage,
      imageCheck,
      digitCount
    ) where

import Utils.Parse
import Data.List
import Data.Maybe
import Data.Function
import Data.List.Split

type Row = [Int]
type Layer = [Row]
type Image = [Layer]
type Width = Int
type Height = Int

-- |https://adventofcode.com/2019/day/8
day08 :: IO ()
day08 = interact $ showLayer . decodeImage . parseImage 25 6 . map readCharAsInt

showLayer :: Layer -> String
showLayer = intercalate "\n" . map showRow

showRow :: Row -> String
showRow = map toChar
  where toChar 0 = ' '
        toChar 1 = '\x2588'

decodeImage :: Image -> Layer
decodeImage = foldl1 mergeLayers
    where mergeLayers = zipWith mergeRows
          mergeRows = zipWith mergePixels
          mergePixels 2 p2 = p2
          mergePixels p1 _ = p1

parseImage :: Width -> Height -> [Int] -> Image
parseImage w h = map parseLayer . chunksOf (w * h)
    where parseLayer = chunksOf w

imageCheck :: Image -> Int
imageCheck img = (digitCount 1 layer) * (digitCount 2 layer)
    where layer = fewestZeros img

fewestZeros :: Image -> Layer
fewestZeros = minimumBy (compare `on` (digitCount 0))

digitCount :: Int -> Layer -> Int
digitCount digit = sum . map (length . filter (== digit))