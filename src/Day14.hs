{-# LANGUAGE ImportQualifiedPost #-}

module Day14 where

import Common.Runner
import Common.Parser
import Common.Grid
import Common.Util (pairs, iterateMaybe)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S


part1 :: String -> Int
part1 = getAns False

part2 :: String -> Int
part2 = getAns True

-- iterateMaybe includes the inital element
-- Using tail excludes that
getAns :: Bool -> String -> Int
getAns floorExists i = length
                 $ tail
                 $ iterateMaybe (fall limit floorExists) rocks
  where
    rocks = pInput i
    -- Get largest y value
    limit = maximum (S.map (\(V2 _ y) -> y) rocks)

-- Have single grain of sand fall
-- Nothing is given when either all sand will fall forever
-- or source is blocked
fall :: Int               -- ^ Falling Limit
     -> Bool              -- ^ Is there a floor?
     -> Set Point         -- ^ Prior rocks and sand
     -> Maybe (Set Point) -- ^ New rocks and sand iff sand comes to a stop
fall limit floorExists rocks = go $ V2 500 0
  where
    go :: Point -> Maybe (Set Point)
    go pt@(V2 x y)
      | V2 500 0 `S.member` rocks = Nothing -- Source of sand is blocked
      | not floorExists && y >= limit = Nothing -- Sand will fall forever
      | floorExists && y == (limit + 1) = Just $ S.insert pt rocks -- Hit infinite floor
      -- Attempt to fall down a tile
      | (pt + V2   0  1) `S.notMember` rocks = go $ pt + V2   0  1
      | (pt + V2 (-1) 1) `S.notMember` rocks = go $ pt + V2 (-1) 1
      | (pt + V2   1  1) `S.notMember` rocks = go $ pt + V2   1  1
      -- Sand is stationary
      | otherwise = Just $ S.insert pt rocks

pInput :: String -> Set Point
pInput = S.unions . pLines pFormation
  where
    pFormation :: Parser (Set Point)
    pFormation = do
      pts <- pPt `sepBy1` string " -> "
      -- Convert a list of endpoints into a list of all points
      -- Some endpoints are included twice, but constructing a set fixes this
      let rocks = concatMap (uncurry lineBetween) $ pairs pts
      pure (S.fromList rocks)
    pPt :: Parser Point
    pPt = do
      x <- pNumber <* char ','
      V2 x <$> pNumber


solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 14
