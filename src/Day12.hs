{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Day12 where

import Common.Runner
import Common.Parser
import Common.Grid
import Common.Search
import Data.Char (ord)
import Control.Monad (guard)
import Data.Array.Unboxed (Array, (!), (//), assocs)
import Data.Array.Unboxed qualified as A
import Data.Array (inRange)


part1 :: String -> Int
part1 i = getAns grid [start] end
 where
    (start, end, grid) = pInput i

part2 :: String -> Int
part2 i = getAns grid starts end
  where
    starts = [pt | (pt, c) <- assocs grid, c == 'a']
    (start, end, grid) = pInput i

-- Search states includes number of steps,
-- But only position is stored
getAns :: Array Point Char -> [Point] -> Point -> Int
getAns grid pts end =
  minimum [n | (loc,n) <- bfsOnN fst (step grid) starts, loc == end]
  where
    starts = (,0) <$> pts

-- Step once
step :: Array Point Char -> (Point, Int) -> [(Point, Int)]
step grid (loc, n) = do
  next <- cardinalNeighbors loc
  guard $ inRange (A.bounds grid) next -- Is the next point in bounds?
  guard $ succ (grid!loc) >= grid!next -- loc + 1 must be same or higher then next
  pure (next,n+1)

-- Find start and end and replace them with a and z
pInput :: String -> (Point, Point, Array Point Char)
pInput i = (start, end, grid // [(start, 'a'), (end, 'z')])
  where
    grid = asciiGridArray id i
    pts = A.assocs grid
    -- Find a location in the grid
    findPt :: Char -> Point
    findPt c = fst $ head $ filter ((== c) . snd) pts
    start = findPt 'S'
    end   = findPt 'E'

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 12
