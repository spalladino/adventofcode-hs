import Test.HUnit
import Day01
import Day02

day01tests = test [
  "testPositives" ~: (calculateFrequency "+10\n+20\n+30") ~?= "60",
  "testNegatives" ~: (calculateFrequency "-10\n-20\n-30") ~?= "-60",
  "testMixed"     ~: (calculateFrequency "-10\n+20\n-30") ~?= "-20",
  "testFind"      ~: (findRepeatedFrequency "+10\n+20\n+50\n-40\n-10") ~?= "30",
  "testFindCycle" ~: (findRepeatedFrequency "+3\n+3\n+4\n-2\n-4") ~?= "10"]

day02tests = test [
  "testChecksum" ~: (calculateChecksum "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab\n") ~?= "12",
  "testCommonLetters" ~: (findCommonLetters "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz\n") ~?= "fgij"]

main :: IO ()
main = do
  runTestTT day02tests
  return ()
