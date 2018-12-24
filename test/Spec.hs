import Test.HUnit
import Day01

day01tests = test [
  "testPositives" ~: (calculateFrequency "+10\n+20\n+30") ~?= "60",
  "testNegatives" ~: (calculateFrequency "-10\n-20\n-30") ~?= "-60",
  "testMixed"     ~: (calculateFrequency "-10\n+20\n-30") ~?= "-20",
  "testFind"      ~: (findRepeatedFrequency "+10\n+20\n+50\n-40\n-10") ~?= "30",
  "testFindCycle" ~: (findRepeatedFrequency "+3\n+3\n+4\n-2\n-4") ~?= "10"]

main :: IO ()
main = do
  runTestTT day01tests
  return ()
