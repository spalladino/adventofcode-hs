module Day02
    ( day02,
      intcode
    ) where

import Utils.Parse
import Utils.Data
import Data.List.Split

-- |https://adventofcode.com/2019/day/2
day02p1 :: IO ()
day02p1 = interact $ show . head . intcode . setupProgram 12 2 . readIntegerList

day02 :: IO ()
day02 = interact $ show . encodeInput . findInput 19690720 . readIntegerList

type Program = [Integer]
type IP = Int
type Noun = Integer
type Verb = Integer

encodeInput :: (Noun, Verb) -> Integer
encodeInput (n, v) = 100 * n + v

findInput :: Integer -> Program -> (Noun, Verb)
findInput target p = snd $ head $ dropWhile ((/= target) . fst) search
      where search = [(head $ intcode $ setupProgram n v p, (n, v)) | n <- [0..100], v <- [0..100]]

setupProgram :: Noun -> Verb -> Program -> Program
setupProgram n v = assocIn 1 n . assocIn 2 v

intcode :: Program -> Program
intcode = intcodeIP 0

intcodeIP :: IP -> Program -> Program
intcodeIP ip program
    | code == 1 = intcodeIP (ip + 4) (intcodeStep program (ip + 1) (+)) 
    | code == 2 = intcodeIP (ip + 4) (intcodeStep program (ip + 1) (*))
    | code == 99 = program
    | otherwise = error ("Unexpected code" ++ show code)
    where code = program !! ip

intcodeStep :: Program -> IP -> (Integer -> Integer -> Integer) -> Program
intcodeStep program ip op = assocIn (fromIntegral dest) res program
        where aPos:bPos:dest:_ = drop ip program
              aVal = program !! fromIntegral aPos
              bVal = program !! fromIntegral bPos
              res = op aVal bVal
