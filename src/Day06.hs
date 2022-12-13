module Day06 where

import Common.Runner
import Common.Parser
import Data.Maybe (fromJust)
import Common.Util (slidingWindow, indicesWhere, uniq)

part1 :: String -> Int
part1 = getAns 4

part2 :: String -> Int
part2 = getAns 14

-- Head is safe because there's always at least one match
-- (+n) gets us the end of the window where uniq
getAns :: Int -> String -> Int
getAns n = (+n)
         . head
         . indicesWhere uniq
         . slidingWindow n

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 6
