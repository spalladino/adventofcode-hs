module Test03 (
  day03tests
) where

import Test.HUnit

import qualified Day03
import Day03 (PathInstruction(..))
import Utils.Data

day03tests = test [
  "wireCrossing-1" ~: Day03.wireCrossing ([R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72], [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83]) ~?= 159,
  "wireCrossing-2" ~: Day03.wireCrossing ([R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51], [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]) ~?= 135,
  "wireCrossingSteps-1" ~: Day03.wireCrossingSteps ([R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72], [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83]) ~?= 610,
  "wireCrossingSteps-2" ~: Day03.wireCrossingSteps ([R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51], [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]) ~?= 410,
  "resolveInstructions-1" ~: Day03.resolveInstructions [R 2,U 5,L 1,D 3] ~?= [(0,0),(0,1),(0,2),(1,2),(2,2),(3,2),(4,2),(5,2),(5,1),(4,1),(3,1),(2,1)]
  ]