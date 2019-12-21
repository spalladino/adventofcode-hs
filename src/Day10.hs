module Day10
    ( day10,
      parseMap,
      maxVisibleCount,
      visibleCount,
      destructionList,
      offsetsByAngle
    ) where

import Data.Array
import Data.List
import Data.List.Split
import Data.Ratio
import Data.Function
import Control.Applicative (liftA2)
import Debug.Trace (trace)

data Object = Asteroid | Empty deriving (Eq, Show)

type Coord = (Int, Int)
type Offset = (Int, Int)
type Angle = (Int, Ratio Int)
type Polar = (Angle, Int)
type Map = Array Coord Object

-- |https://adventofcode.com/2019/day/10
day10 :: IO ()
day10 = interact $ show . (!! 199) . destructionList (20,20) . parseMap

parseMap :: String -> Map
parseMap = makeArray . map (map toObject) . lines
    where toObject '#' = Asteroid
          toObject '.' = Empty
          maxIndex xss = (length (head xss) - 1, length xss - 1)
          makeArray xss = listArray ((0,0), maxIndex xss) (concat (transpose xss))

bestPosition :: Map -> Coord
bestPosition m = maximumBy (compare `on` (flip visibleCount m)) (asteroids m)
          
maxVisibleCount :: Map -> Int
maxVisibleCount m = maximum $ map (flip visibleCount m) $ asteroids m

visibleCount :: Coord -> Map -> Int
visibleCount origin = length . nubBy eqAngle . offsets origin

destructionList :: Coord -> Map -> [Coord]
destructionList origin = map (from origin) . concat . transpose . offsetsByAngle origin

offsetsByAngle :: Offset -> Map -> [[Offset]]
offsetsByAngle origin = map (sortOn distance) . groupBy eqAngle . sortOn angle . offsets origin

asteroids :: Map -> [Coord]
asteroids = map fst . filter (isAsteroid . snd) . assocs

offsets :: Coord -> Map -> [Offset]
offsets origin = map (offset origin) . delete origin . asteroids

from :: Coord -> Offset -> Coord
from (x0,y0) (x,y) = (x+x0, y0-y)

offset :: Coord -> Coord -> Offset
offset (x0,y0) (x,y) = (x-x0, y0-y)

polar :: Coord -> Polar
polar (x,y) = (angle (x,y), distance (x,y))

distance :: Coord -> Int
distance (x,y) = x*x + y*y

angle :: Coord -> Angle
angle (x,y) = (quad, ratio)
    where quad 
            = quadrant (signum x) (signum y)
          ratio
            | x == 0 || y == 0 = 0
            | quad == 2 || quad == 4 = abs y % abs x
            | otherwise = abs x % abs y

quadrant :: Int -> Int -> Int
quadrant 0 0 = 0
quadrant 0 1 = 1
quadrant 1 1 = 1
quadrant 1 0 = 2
quadrant 1 (-1) = 2
quadrant 0 (-1) = 3
quadrant (-1) (-1) = 3
quadrant (-1) 0 = 4
quadrant (-1) 1 = 4
quadrant x y = error ("Invalid input: " ++ show x ++ "," ++ show y)

eqAngle :: Coord -> Coord -> Bool
eqAngle x y = angle x == angle y

isAsteroid :: Object -> Bool
isAsteroid Asteroid = True 
isAsteroid _ = False