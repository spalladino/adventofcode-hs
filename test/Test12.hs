module Test12 (
  test12
) where

import Test.HUnit
import Day12

map0 = [(-1,0,2),(2,-10,-7),(4,-8,8),(3,5,-1)]

test12 = test [
  "steps-10" ~: (totalEnergy $ runSteps 10 $ makeSystem map0) ~?= 179,
  "systemPeriodSize" ~: (systemPeriodSize $ makeSystem map0) ~?= 2772
  ]