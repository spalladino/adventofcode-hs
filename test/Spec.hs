import Test.HUnit
import Day01

day01tests = test [
  "fuel-12" ~: (fuel 12) ~?= 2,
  "fuel-14" ~: (fuel 14) ~?= 2,
  "fuel-1969" ~: (fuel 1969) ~?= 654,
  "fuel-100756" ~: (fuel 100756) ~?= 33583,
  
  "totalfuel-14" ~: (totalfuel 14) ~?= 2,
  "totalfuel-1969" ~: (totalfuel 1969) ~?= 966,
  "totalfuel-100756" ~: (totalfuel 100756) ~?= 50346,

  "sumfuel" ~: (sumfuel fuel [12,14,1969,100756]) ~?= (2 + 2 + 654 + 33583)
  ]


main :: IO ()
main = do
  runTestTT day01tests
  return ()
