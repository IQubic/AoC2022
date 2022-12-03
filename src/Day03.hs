{-# LANGUAGE ImportQualifiedPost #-}

module Day03 where

import Common.Runner
import Common.Parser
import Control.Arrow (first)
import Data.Bifoldable (biList)
import Data.List.Split
import Data.Char
import Data.Set (Set)
import Data.Set qualified as S

part1 :: String -> Int
part1 = sum
      . map (setScore . biList . splitHalf)
      . lines
      where
        -- Tortoise and Hare
        splitHalf :: [a] -> ([a],[a])
        splitHalf xs = go xs xs
          where
            go (y:ys) (_:_:zs) = first (y:) (go ys zs)
            go ys _ = ([], ys)

part2 :: String -> Int
part2 = sum
      . map setScore
      . chunksOf 3
      . lines

-- Find the letter that's present in all strings given
-- then calc the score of that letter
-- head . S.toList is safe because there's only one match
setScore :: [String] -> Int
setScore = letterScore
         . head
         . S.toList
         . foldl1 S.intersection
         . map S.fromList
  where
    letterScore :: Char -> Int
    letterScore c = if isLower c
                       then ord c - ord 'a' + 1
                       else ord c - ord 'A' + 27

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 3
