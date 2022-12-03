{-# LANGUAGE ImportQualifiedPost #-}

module Day03 where

import Common.Runner
import Common.Parser
import Data.List.Split
import Data.Char
import Data.Set (Set)
import Data.Set qualified as S
import Data.Bifunctor (bimap)

part1 :: String -> Int
part1 = sum
      . map (setScore . mkBag)
      . lines
      where
        bagScore :: (Set Char, Set Char) -> Int
        bagScore (xs, ys) = letterScore $ head $ S.toList $ S.intersection xs ys
        mkBag :: String -> [String]
        mkBag bag = let (xs, ys) = splitBag bag
                        in [xs, ys]
        splitBag :: String -> (String, String)
        splitBag xs = splitAt half xs
          where
            half = length xs `div` 2


part2 :: String -> Int
part2 = sum
      . map setScore
      . chunksOf 3
      . lines

setScore :: [String] -> Int
setScore = letterScore
         . head
         . S.toList
         . foldl1 S.intersection
         . map S.fromList

letterScore :: Char -> Int
letterScore c = if isUpper c
                   then ord c - 38
                   else ord c - 96

-- parseInput :: String -> [Rucksack]
-- parseInput = map (bimap S.fromList S.fromList . _)
--            . lines
--   where
--     mkRucksack :: String -> String -> (String, String)
--     mkRucksack = undefined

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 3
