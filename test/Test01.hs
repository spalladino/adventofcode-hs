module Test01 (
  day01tests
) where

import Test.HUnit

import qualified Day01
import Utils.Data

day01tests = test [
  "fuel-12" ~: (Day01.fuel 12) ~?= 2,
  "fuel-14" ~: (Day01.fuel 14) ~?= 2,
  "fuel-1969" ~: (Day01.fuel 1969) ~?= 654,
  "fuel-100756" ~: (Day01.fuel 100756) ~?= 33583,
  "totalfuel-14" ~: (Day01.totalfuel 14) ~?= 2,
  "totalfuel-1969" ~: (Day01.totalfuel 1969) ~?= 966,
  "totalfuel-100756" ~: (Day01.totalfuel 100756) ~?= 50346,
  "sumfuel" ~: (Day01.sumfuel Day01.fuel [12,14,1969,100756]) ~?= (2 + 2 + 654 + 33583)
  ]
