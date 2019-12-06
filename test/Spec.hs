import Test.HUnit
import Day01
import Day02
import Utils.Data

utilsTests = test [
  "assocIn-list" ~: (assocIn 2 60 [10,20,30,40]) ~?= [10,20,60,40]
  ]

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

day02tests = test [
  "prog-1" ~: (intcode [1,9,10,3,2,3,11,0,99,30,40,50]) ~?= [3500,9,10,70,2,3,11,0,99,30,40,50],
  "prog-2" ~: (intcode [1,0,0,0,99]) ~?= [2,0,0,0,99],
  "prog-3" ~: (intcode [2,3,0,3,99]) ~?= [2,3,0,6,99],
  "prog-4" ~: (intcode [2,4,4,5,99,0]) ~?= [2,4,4,5,99,9801],
  "prog-5" ~: (intcode [1,1,1,4,99,5,6,0,99]) ~?= [30,1,1,4,2,5,6,0,99]
  ]

main :: IO ()
main = do
  runTestTT day02tests
  return ()


