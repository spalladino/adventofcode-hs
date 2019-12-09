import Test.HUnit

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05

import Utils.Data

import Day03 (PathInstruction(..))

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
  "wireCrossing-1" ~: Day03.wireCrossing ([R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72], [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83]) ~?= 159,
  "wireCrossing-2" ~: Day03.wireCrossing ([R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51], [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]) ~?= 135,
  "wireCrossingSteps-1" ~: Day03.wireCrossingSteps ([R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72], [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83]) ~?= 610,
  "wireCrossingSteps-2" ~: Day03.wireCrossingSteps ([R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51], [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]) ~?= 410,
  "resolveInstructions-1" ~: Day03.resolveInstructions [R 2,U 5,L 1,D 3] ~?= [(0,0),(0,1),(0,2),(1,2),(2,2),(3,2),(4,2),(5,2),(5,1),(4,1),(3,1),(2,1)]
  ]

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

day05tests = test [
  "prog-1" ~: (Day05.intcode [] [1,9,10,3,2,3,11,0,99,30,40,50]) ~?= Day05.End [3500,9,10,70,2,3,11,0,99,30,40,50] [],
  "prog-2" ~: (Day05.intcode [] [1,0,0,0,99]) ~?= Day05.End [2,0,0,0,99] [],
  "prog-3" ~: (Day05.intcode [] [2,3,0,3,99]) ~?= Day05.End [2,3,0,6,99] [],
  "prog-4" ~: (Day05.intcode [] [2,4,4,5,99,0]) ~?= Day05.End [2,4,4,5,99,9801] [],
  "prog-5" ~: (Day05.intcode [] [1,1,1,4,99,5,6,0,99]) ~?= Day05.End [30,1,1,4,2,5,6,0,99] [],
  "prog-compare8-1t" ~: (Day05.output $ Day05.intcode [8] [3,9,8,9,10,9,4,9,99,-1,8]) ~?= [1],
  "prog-compare8-1f" ~: (Day05.output $ Day05.intcode [5] [3,9,8,9,10,9,4,9,99,-1,8]) ~?= [0],
  "prog-compare8-2t" ~: (Day05.output $ Day05.intcode [5] [3,9,7,9,10,9,4,9,99,-1,8]) ~?= [1],
  "prog-compare8-2feq" ~: (Day05.output $ Day05.intcode [8] [3,9,7,9,10,9,4,9,99,-1,8]) ~?= [0],
  "prog-compare8-2fgt" ~: (Day05.output $ Day05.intcode [9] [3,9,7,9,10,9,4,9,99,-1,8]) ~?= [0],
  "prog-compare8-3t" ~: (Day05.output $ Day05.intcode [8] [3,3,1108,-1,8,3,4,3,99]) ~?= [1],
  "prog-compare8-3f" ~: (Day05.output $ Day05.intcode [5] [3,3,1108,-1,8,3,4,3,99]) ~?= [0],
  "prog-compare8-4t" ~: (Day05.output $ Day05.intcode [5] [3,3,1107,-1,8,3,4,3,99]) ~?= [1],
  "prog-compare8-4feq" ~: (Day05.output $ Day05.intcode [8] [3,3,1107,-1,8,3,4,3,99]) ~?= [0],
  "prog-compare8-4fgt" ~: (Day05.output $ Day05.intcode [9] [3,3,1107,-1,8,3,4,3,99]) ~?= [0],
  "prog-jump-1t" ~: (Day05.output $ Day05.intcode [0] [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) ~?= [0],
  "prog-jump-1f" ~: (Day05.output $ Day05.intcode [8] [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) ~?= [1],
  "prog-jump-2t" ~: (Day05.output $ Day05.intcode [0] [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) ~?= [0],
  "prog-jump-2f" ~: (Day05.output $ Day05.intcode [8] [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) ~?= [1],
  "prog-complex-lt" ~: (Day05.output $ Day05.intcode [7] [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]) ~?= [999],
  "prog-complex-eq" ~: (Day05.output $ Day05.intcode [8] [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]) ~?= [1000],
  "prog-complex-gt" ~: (Day05.output $ Day05.intcode [9] [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]) ~?= [1001]
  ]

main :: IO ()
main = do
  runTestTT day05tests
  return ()


