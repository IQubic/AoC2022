module Day10 where

import Common.Runner
import Common.Parser

part1 :: String -> a
part1 i = undefined

part2 :: String -> a
part2 i = undefined

parseInput :: String -> [a]
parseInput = parseLines undefined

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 10
