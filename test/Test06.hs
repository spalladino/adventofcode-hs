module Test06 (
  test06
) where

import Test.HUnit
import Day06

test06 = test [
  "orbitsCount" ~: ((orbitsCount 0) . parseOrbits) "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L" ~?= 42,
  "orbitsDistance" ~: ((orbitsDistance "YOU" "SAN") . parseOrbits) "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN" ~?= 4
  ]