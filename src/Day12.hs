{-# LANGUAGE NamedFieldPuns #-}

module Day12
    ( day12,
      totalEnergy,
      runSteps,
      makeSystem,
      systemPeriodSize
    ) where

import Data.List.Split
import Data.Ratio
import Data.Function
import Debug.Trace (trace)
import Data.List ()
import Control.Applicative (Applicative, (<*>), liftA2)
import Data.Set (member, empty, insert, size, Set)

data Moon = Moon { getPosition :: Position, getVelocity :: Velocity} deriving (Eq, Show, Ord)

newtype Coords a = Coords { getCoords :: (a, a, a) } deriving (Eq, Show, Ord)
type Position = Coords Int
type Velocity = Coords Int

instance Functor Coords where
    fmap f (Coords (x,y,z)) = Coords (f x, f y, f z)

instance Applicative Coords where
    pure x = Coords (x,x,x)
    Coords (xf,yf,zf) <*> Coords (x,y,z) = Coords (xf x, yf y, zf z)

coord :: Int -> Coords a -> a
coord 0 (Coords (x,y,z)) = x
coord 1 (Coords (x,y,z)) = y
coord 2 (Coords (x,y,z)) = z

-- |https://adventofcode.com/2019/day/12
day12 :: IO ()
day12 = interact $ const $ show $ systemPeriodSize $ makeSystem day12input

day12input = [(1,2,-9),(-1,-9,-4),(17,6,8),(12,4,2)]


makeSystem :: [(Int,Int,Int)] -> [Moon]
makeSystem = map makeMoon
    where makeMoon pos = Moon (Coords pos) (pure 0)


totalEnergy :: [Moon] -> Int
totalEnergy = sum . map moonEnergy

moonEnergy :: Moon -> Int
moonEnergy (Moon position velocity) = coordsEnergy position * coordsEnergy velocity

coordsEnergy :: Coords Int -> Int
coordsEnergy (Coords (x,y,z)) = sum $ map abs [x,y,z]


runSteps :: Int -> [Moon] -> [Moon]
runSteps count system = iterate step system !! count

step :: [Moon] -> [Moon]
step = applyVelocities . applyGravities

applyVelocities :: [Moon] -> [Moon]
applyVelocities = map applyVelocity

applyVelocity :: Moon -> Moon
applyVelocity (Moon position velocity) = Moon newPosition velocity
    where newPosition = (+) <$> position <*> velocity

applyGravities :: [Moon] -> [Moon]
applyGravities moons = map (applyGravity moons) moons

applyGravity :: [Moon] -> Moon -> Moon
applyGravity ms (Moon position velocity) = Moon position newVelocity 
    where newVelocity = foldr (liftA2 (+)) velocity offsets
          offsets = map (liftA2 coordOffset position . getPosition) ms


systemPeriodSize :: [Moon] -> Int
systemPeriodSize ms = foldr1 lcm $ map coordPeriodSize [xs,ys,zs]
    where xs = map (coord 0 . getPosition) ms
          ys = map (coord 1 . getPosition) ms
          zs = map (coord 2 . getPosition) ms

coordPeriodSize :: [Int] -> Int
coordPeriodSize ps = (*2) $ snd $ until (areVelocitiesZero . fst) coordStepPeriod $ coordStepPeriod (map toPV ps, 0)
    where toPV p = (p,0)
          areVelocitiesZero = all ((== 0) . snd)
          coordStepPeriod (pvs, n) = (stepCoord pvs, n+1)

stepCoord :: [(Int,Int)] -> [(Int,Int)]
stepCoord = applyCoordVelocities . applyCoordGravities

applyCoordVelocities :: [(Int,Int)] -> [(Int,Int)]
applyCoordVelocities = map applyCoordVelocity
    where applyCoordVelocity (pos,vel) = (pos+vel,vel)

applyCoordGravities :: [(Int,Int)] -> [(Int,Int)]
applyCoordGravities pvs = map (applyCoordGravity pvs) pvs

applyCoordGravity :: [(Int,Int)] -> (Int,Int) -> (Int,Int)
applyCoordGravity pvs (pos,vel) = (pos,newvel)
    where newvel = foldr (+) vel offsets
          offsets = map (coordOffset pos . fst) pvs


coordOffset :: Int -> Int -> Int
coordOffset x y
    | x < y = 1
    | x > y = -1
    | otherwise  = 0

