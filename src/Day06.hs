module Day06
    ( day06,
      parseOrbits,
      orbitsCount
    ) where

import Utils.Data
import Data.List
import Data.Maybe
import Data.List.Split
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

-- |https://adventofcode.com/2019/day/6
day06 :: IO ()
day06 = interact $ show . orbitsCount 0 . parseOrbits

data Planet = 
    Planet { orbiters :: [Planet], name :: String }
    deriving (Show, Eq)

type Planets = [Planet]

parseOrbits :: String -> Planet
parseOrbits = flip resolveOrbits "COM" . foldl' parseOrbit Map.empty . lines

parseOrbit :: Map String [String] -> String -> Map String [String]
parseOrbit ps str = Map.alter addOrbiter orbitedName $ Map.alter addPlanet planetName ps
    where addOrbiter = Just . maybe [planetName] (planetName:)
          addPlanet = Just . fromMaybe []
          [orbitedName, planetName] = splitOn ")" str

resolveOrbits :: Map String [String] -> String -> Planet
resolveOrbits ps name = Planet (map (resolveOrbits ps) (ps ! name)) name

orbitsCount :: Int -> Planet -> Int
orbitsCount n (Planet [] _) = n
orbitsCount n (Planet orbiters _) = n + sum (map (orbitsCount (n + 1)) orbiters)
