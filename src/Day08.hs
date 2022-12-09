{-# LANGUAGE ImportQualifiedPost #-}

module Day08 where

import Common.Runner
import Common.Parser
import Common.Grid
import Common.Util (countIf)
import Data.Char (digitToInt)
import Data.Ix (Ix(..))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Foldable (maximumBy)

-- Look from each tree to the outside
-- if there's a line of all smaller trees, then it visible
part1 :: String -> Int
part1 i = countIf visible $ range bounds
  where
    grid = pInput i
    bounds = boundingBox $ M.keysSet grid
    -- Get the height of tree at a given point
    getHeight pt = grid M.!? pt
    -- Is this tree visible at all?
    visible :: Point -> Bool
    visible = any clearView . sightLines bounds
    -- Is there a clear view from the first spot
    clearView []       = error "clearView: empty list"
    clearView (pt:pts) = all ((< getHeight pt) . getHeight) pts

part2 :: String -> Int
part2 i = maximum $ map scenicScore $ range bounds
  where
    grid = pInput i
    bounds = boundingBox $ M.keysSet grid
    -- Get the height of tree at a given point
    getHeight pt = grid M.!? pt
    scenicScore :: Point -> Int
    scenicScore = product . map treesSeen . sightLines bounds
    -- Count the number of trees seen in a given direction
    -- Break appropriately counts the last tree if not on edge
    treesSeen :: [V2 Int] -> Int
    treesSeen [] = error "treesSeen: empty list"
    treesSeen (pt:pts) =
      case break (>= getHeight pt) $ map getHeight pts of
        (a,[])  -> length a
        (a,_:_) -> length a + 1

-- Get a list of all points visible from a given spot
-- incluing the initial point
sightLines :: (Point, Point) {- ^ Bounds -}
           -> Point          {- ^ Start  -}
           -> [[Point]]      {- ^ Points to the edge of grid in each dir -}
sightLines bounds pt = map lineToEdge allDirs
  where
    -- Get a list all points from here to the edge
    -- incluing the initial point
    lineToEdge :: Dir -> [Point]
    lineToEdge = takeWhile (inRange bounds) . lineFrom' pt

pInput :: String -> Map Point Int
pInput = asciiGridMap (Just . digitToInt)

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 8
