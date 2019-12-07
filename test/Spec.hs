import Test.HUnit
import qualified Day01
import qualified Day02
import Day03
import Utils.Data

utilsTests = test [
  "assocIn-list" ~: (assocIn 2 60 [10,20,30,40]) ~?= [10,20,60,40]
  ]

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

day02tests = test [
  "prog-1" ~: (Day02.intcode [1,9,10,3,2,3,11,0,99,30,40,50]) ~?= [3500,9,10,70,2,3,11,0,99,30,40,50],
  "prog-2" ~: (Day02.intcode [1,0,0,0,99]) ~?= [2,0,0,0,99],
  "prog-3" ~: (Day02.intcode [2,3,0,3,99]) ~?= [2,3,0,6,99],
  "prog-4" ~: (Day02.intcode [2,4,4,5,99,0]) ~?= [2,4,4,5,99,9801],
  "prog-5" ~: (Day02.intcode [1,1,1,4,99,5,6,0,99]) ~?= [30,1,1,4,2,5,6,0,99]
  ]

day03tests = test [
  "wireCrossing-1" ~: wireCrossing ([R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72], [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83]) ~?= 159,
  "wireCrossing-2" ~: wireCrossing ([R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51], [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]) ~?= 135,
  "wireCrossingSteps-1" ~: wireCrossingSteps ([R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72], [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83]) ~?= 610,
  "wireCrossingSteps-2" ~: wireCrossingSteps ([R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51], [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]) ~?= 410,
  "resolveInstructions-1" ~: resolveInstructions [R 2,U 5,L 1,D 3] ~?= [(0,0),(0,1),(0,2),(1,2),(2,2),(3,2),(4,2),(5,2),(5,1),(4,1),(3,1),(2,1)]
  ]

main :: IO ()
main = do
  runTestTT day03tests
  return ()


