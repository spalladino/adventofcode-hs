module Test04 (
  day04tests
) where

import Test.HUnit

import qualified Day04

import Utils.Data

day04tests = test [
  "hasDoubleDigits-1" ~: Day04.hasDoubleDigits [1,1,1,1,1,1] ~?= True,
  "hasDoubleDigits-2" ~: Day04.hasDoubleDigits [1,2,3,4,5,6] ~?= False,
  "hasDoubleDigits-3" ~: Day04.hasDoubleDigits [1,2,3,4,5,5] ~?= True,
  "hasDoubleDigits-4" ~: Day04.hasDoubleDigits [1,1,3,4,5,5] ~?= True,
  "hasExactlyDoubleDigits-1" ~: Day04.hasExactlyDoubleDigits [1,1,3,4,5,6] ~?= True,
  "hasExactlyDoubleDigits-2" ~: Day04.hasExactlyDoubleDigits [1,2,3,4,5,6] ~?= False,
  "hasExactlyDoubleDigits-3" ~: Day04.hasExactlyDoubleDigits [1,2,3,4,5,5] ~?= True,
  "hasExactlyDoubleDigits-4" ~: Day04.hasExactlyDoubleDigits [1,1,1,4,5,5] ~?= True,
  "hasExactlyDoubleDigits-5" ~: Day04.hasExactlyDoubleDigits [1,1,1,4,5,6] ~?= False,
  "hasExactlyDoubleDigits-6" ~: Day04.hasExactlyDoubleDigits [1,1,2,2,3,3] ~?= True,
  "hasExactlyDoubleDigits-7" ~: Day04.hasExactlyDoubleDigits [1,2,3,4,4,4] ~?= False,
  "hasExactlyDoubleDigits-8" ~: Day04.hasExactlyDoubleDigits [1,1,1,1,2,2] ~?= True,
  "hasExactlyDoubleDigits-9" ~: Day04.hasExactlyDoubleDigits [1,1,1,1,1,2] ~?= False
  ]  
