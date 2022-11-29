module Day01 where

import Common.Runner
import Common.Parser

part1 :: String -> a
part1 i = undefined

part2 :: String -> a
part2 i = undefined

parseInput :: String -> [Int]
parseInput = parseLines number

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 1
