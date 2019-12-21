module Test09 (
  test09
) where

import Test.HUnit
import Day09

quine = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

test09 = test [
  "quine" ~: output (intcode [] quine) ~?= quine,
  "16-bits" ~: output (intcode [] [1102,34915192,34915192,7,4,7,99,0]) ~?= [1219070632396864],
  "middle-number" ~: output (intcode [] [104,1125899906842624,99]) ~?= [1125899906842624]
  ]