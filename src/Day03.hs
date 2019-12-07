module Day03
    ( day03,
      wireCrossing,
      wireCrossingSteps,
      resolveInstructions,
      PathInstruction (..)
    ) where

import Utils.Parse
import Utils.Data
import Data.List
import Data.List.Split

data PathInstruction = R Int | L Int | D Int | U Int deriving (Show, Read)
type PathInstructions = [PathInstruction]
type Coord = (Int, Int)
type Path = [Coord]

-- |https://adventofcode.com/2019/day/3
day03 :: IO ()
day03 = interact $ show . wireCrossingSteps . parse
        where parse str = let [p1,p2] = parsePaths str in (p1,p2)
              parsePaths = map (map parseInstruction) . map (splitOn ",") . lines
              parseInstruction ('R' : n) = R (read n)
              parseInstruction ('L' : n) = L (read n)
              parseInstruction ('D' : n) = D (read n)
              parseInstruction ('U' : n) = U (read n)

wireCrossingSteps :: (PathInstructions, PathInstructions) -> Int
wireCrossingSteps (p1, p2) = minimum $ filter (>0) $ map snd $ wireCrossingsWithSteps (p1, p2)

wireCrossing :: (PathInstructions, PathInstructions) -> Int
wireCrossing (p1, p2) = minimum $ filter (>0) $ map (manhattanDistance (0,0)) $ wireCrossings (p1, p2)

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (i,j) (p,q) = abs(i-p) + abs(j-q)

wireCrossingsWithSteps :: (PathInstructions, PathInstructions) -> [(Coord, Int)]
wireCrossingsWithSteps (pi1, pi2) = [(c1, s1 + s2) | (c1,s1) <- ps1, (c2,s2) <- filter ((==c1) . fst) ps2]
                                     where ps1 = withIndex (resolveInstructions pi1)
                                           ps2 = withIndex (resolveInstructions pi2)
                                           withIndex = flip zip [0..]

wireCrossings :: (PathInstructions, PathInstructions) -> [Coord]
wireCrossings (p1, p2) = intersect (resolveInstructions p1) (resolveInstructions p2)

resolveInstructions :: PathInstructions -> Path
resolveInstructions = concat . scanl (execInstruction . last) [(0,0)]

execInstruction :: Coord -> PathInstruction -> Path
execInstruction (i0,j0) (R n) = tail [(i0,j) | j <- [j0..j0+n]]
execInstruction (i0,j0) (L n) = tail [(i0,j) | j <- [j0,j0-1..j0-n]]
execInstruction (i0,j0) (U n) = tail [(i,j0) | i <- [i0..i0+n]]
execInstruction (i0,j0) (D n) = tail [(i,j0) | i <- [i0,i0-1..i0-n]]