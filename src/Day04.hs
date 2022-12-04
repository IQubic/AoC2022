{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Day04 where

import Common.Runner
import Common.Parser
import Common.Util (countIf)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS


part1 :: String -> Int
part1 = countIf (uncurry valid)
      . parseInput
  where
    valid r1 r2 = r1 `IS.isSubsetOf` r2 || r2 `IS.isSubsetOf` r1

part2 :: String -> Int
part2 = countIf (\(x, y) -> not $ IS.disjoint x y)
      . parseInput

parseInput :: String -> [(IntSet, IntSet)]
parseInput = parseLines $ do
  first <- pRange <* char ','
  second <- pRange
  pure (first, second)
    where
      pRange :: Parser IntSet
      pRange = do
        first <- number <* char '-'
        second <- number
        pure $ IS.fromList [first..second]

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 4
