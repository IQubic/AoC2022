module Day01 where

import Common.Runner
import Common.Parser
import Data.List (sortOn)
import Data.Ord (Down(..))

-- For each reindeer, sum the number of calories
-- then take the maximum.
part1 :: String -> Int
part1 = maximum
      . map sum
      . pInput

-- For each reindeer, sum the number of calories
-- then sortOn Down to get the largest first,
-- then sum the first three
part2 :: String -> Int
part2 = sum
      . take 3
      . sortOn Down
      . map sum
      . pInput

pInput :: String -> [[Int]]
pInput = pAll $ pElf `sepEndBy1` eol
  where
    pElf = pNumber `endBy1` eol

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 1
