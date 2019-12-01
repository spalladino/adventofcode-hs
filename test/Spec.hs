import Test.HUnit
import Day01

day01tests = test [
  "test" ~: (1 + 1) ~?= 2]

main :: IO ()
main = do
  runTestTT day01tests
  return ()
