module Day06 where

import Common.Runner
import Common.Parser
import Data.Maybe (fromJust)
import Common.Util (slidingWindow, allDiff)

part1 :: String -> Int
part1 = getAns 4

part2 :: String -> Int
part2 = getAns 14

getAns :: Int -> String -> Int
getAns n = (+n)
         . fromJust
         . firstIndex 0 allDiff
         . slidingWindow n
 where
   firstIndex :: Int -> (a -> Bool) -> [a] -> Maybe Int
   firstIndex n _ []     = Nothing
   firstIndex n p (x:xs) = if p x
                              then Just n
                              else firstIndex (n+1) p xs

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 6
