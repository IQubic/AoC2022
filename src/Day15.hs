module Day15 where

import Common.Runner
import Common.Parser

part1 :: String -> a
part1 i = undefined

part2 :: String -> a
part2 i = undefined

pInput :: String -> [a]
pInput = pLines undefined

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 15
