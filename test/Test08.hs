module Test08 (
  test08
) where

import Test.HUnit
import Day08

simpleImage = [[[1,2,3],[4,5,6]], [[7,8,9],[0,1,2]]]

test08 = test [
  "parseImage" ~: parseImage 3 2 [1,2,3,4,5,6,7,8,9,0,1,2] ~?= simpleImage,
  "digitsCount-0-0" ~: digitCount 0 (head simpleImage) ~?= 0,
  "digitsCount-0-1" ~: digitCount 0 (last simpleImage) ~?= 1,
  "digitsCount-1-0" ~: digitCount 1 (head simpleImage) ~?= 1,
  "digitsCount-1-1" ~: digitCount 1 (last simpleImage) ~?= 1,
  "decodeImage" ~: (decodeImage . parseImage 2 2) [0,2,2,2,1,1,2,2,2,2,1,2,0,0,0,0] ~?= [[0,1],[1,0]]
  ]