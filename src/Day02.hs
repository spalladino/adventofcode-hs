module Day02
    ( day02,
      intcode
    ) where

import Utils.Parse
import Utils.Data
import Data.List.Split

-- |https://adventofcode.com/2019/day/2
day02 :: IO ()
day02 = interact $ show . head . intcode . setupProgram . (map (\x -> read x :: Integer)) . (splitOn ",")

type Program = [Integer]
type IP = Int

setupProgram :: Program -> Program
setupProgram = assocIn 1 12 . assocIn 2 2

intcode :: Program -> Program
intcode p = intcodeIP p 0

intcodeIP :: Program -> IP -> Program
intcodeIP program ip
    | code == 1 = intcodeIP (intcodeStep program (ip + 1) (+)) (ip + 4)
    | code == 2 = intcodeIP (intcodeStep program (ip + 1) (*)) (ip + 4)
    | code == 99 = program
    | otherwise = error ("Unexpected code" ++ show code)
    where code = program !! ip

intcodeStep :: Program -> IP -> (Integer -> Integer -> Integer) -> Program
intcodeStep program ip op = assocIn (fromIntegral dest) res program
        where aPos:bPos:dest:_ = drop ip program
              aVal = program !! (fromIntegral aPos)
              bVal = program !! (fromIntegral bPos)
              res = op aVal bVal
