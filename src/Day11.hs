module Day11
    ( day11
    ) where

import Data.Set
import Data.List.Split
import Data.Ratio
import Data.Function
import Day09
import Debug.Trace (trace)
import qualified Data.List as List

type Coord = (Int,Int)
type Hull = Set Coord

data Direction = RUp | RDown | RRight | RLeft
data Robot = Robot Direction Coord
data HullRobot = HullRobot{hull :: Hull, robot :: Robot }

initialHullRobot :: HullRobot
initialHullRobot = HullRobot empty (Robot RUp (0,0))

initialWhiteHullRobot :: HullRobot
initialWhiteHullRobot = HullRobot (singleton (0,0)) (Robot RUp (0,0))

position :: Robot -> Coord
position (Robot _ pos) = pos

-- |https://adventofcode.com/2019/day/11
day11 :: IO ()
day11 = interact $ showHull . hull . last . (iterateRobot initialWhiteHullRobot) . readProgram

showHull :: Hull -> String
showHull hull = List.intercalate "\n" $ List.map showHullRow $ List.transpose $ hullRows hull
      where showHullRow = List.map toHullChar
            toHullChar 1 = '\x2588'
            toHullChar 0 = ' '

hullRows :: Hull -> [[Int]]
hullRows whites = [[boolToColor (member (i,j) whites) | i <- [mini..maxi]] | j <- [minj..maxj]]
      where boolToColor True = 1
            boolToColor False = 0
            allj = List.map snd (elems whites)
            mini = fst (findMin whites)
            maxi = fst (findMax whites)
            minj = minimum allj
            maxj = maximum allj

panelsPainted :: Program -> [Coord]
panelsPainted = List.nub . List.map (position . robot) . (iterateRobot initialHullRobot)

iterateRobot :: HullRobot -> Program -> [HullRobot]
iterateRobot hullRobot program = List.map fst $ takeWhile (not . isEnded . snd) $ iterate stepRobot (hullRobot, initialState program [])

stepRobot :: (HullRobot, Execution) -> (HullRobot, Execution)
stepRobot (hullRobot, execution) = (newHullRobot, newExecution)
      where (newExecution, output) = consumeOutput $ intcodeRun [readRobotPanel hullRobot] execution
            newHullRobot = robotMove hullRobot output

robotMove :: HullRobot -> [Int] -> HullRobot
robotMove (HullRobot hull robot) [outColor, dirChange] 
  = HullRobot (paintHull hull (position robot) outColor) (moveRobot robot dirChange)

readRobotPanel :: HullRobot -> Int
readRobotPanel (HullRobot whites robot)
    | member (position robot) whites = 1
    | otherwise = 0

paintHull :: Hull -> Coord -> Int -> Hull
paintHull whites position 1 = insert position whites
paintHull whites position 0 = delete position whites

moveRobot :: Robot -> Int -> Robot
moveRobot (Robot direction position) dirChange = Robot updatedDirection (newPosition updatedDirection position)
    where updatedDirection = newDirection direction dirChange

newDirection :: Direction -> Int -> Direction
newDirection RUp 0 = RLeft
newDirection RDown 0 = RRight
newDirection RRight 0 = RUp
newDirection RLeft 0 = RDown
newDirection RUp 1 = RRight
newDirection RDown 1 = RLeft
newDirection RRight 1 = RDown
newDirection RLeft 1 = RUp

newPosition :: Direction -> Coord -> Coord
newPosition RUp (i,j) = (i-1,j)
newPosition RDown (i,j) = (i+1,j)
newPosition RLeft (i,j) = (i,j-1)
newPosition RRight (i,j) = (i,j+1)