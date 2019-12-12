module Test02 (
  day02tests
) where

import Test.HUnit

import qualified Day02

import Utils.Data

day02tests = test [
  "prog-1" ~: (Day02.intcode [1,9,10,3,2,3,11,0,99,30,40,50]) ~?= [3500,9,10,70,2,3,11,0,99,30,40,50],
  "prog-2" ~: (Day02.intcode [1,0,0,0,99]) ~?= [2,0,0,0,99],
  "prog-3" ~: (Day02.intcode [2,3,0,3,99]) ~?= [2,3,0,6,99],
  "prog-4" ~: (Day02.intcode [2,4,4,5,99,0]) ~?= [2,4,4,5,99,9801],
  "prog-5" ~: (Day02.intcode [1,1,1,4,99,5,6,0,99]) ~?= [30,1,1,4,2,5,6,0,99]
  ]
