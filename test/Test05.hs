module Test05 (
  day05tests
) where

import Test.HUnit

import qualified Day05

import Utils.Data

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
