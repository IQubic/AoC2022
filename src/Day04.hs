{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Day04 where

import Common.Runner
import Common.Parser
import Common.Util (countIf)
import Data.IntegerInterval (IntegerInterval, (<=..<=))
import Data.IntegerInterval qualified as I

part1 :: String -> Int
part1 = getAns overlap
  where
    overlap r1 r2 = r1 `I.isSubsetOf` r2 || r2 `I.isSubsetOf` r1

part2 :: String -> Int
part2 = getAns (I.==?)

getAns :: (IntegerInterval -> IntegerInterval -> Bool)
       -> String
       -> Int
getAns p = countIf (uncurry p)
         . parseInput

pInput :: String -> [(IntegerInterval, IntegerInterval)]
pInput = pLines $ do
  first <- pRange <* char ','
  (first,) <$> pRange
    where
      pRange :: Parser IntegerInterval
      pRange = do
        first <- pNumber <* char '-'
        second <- pNumber
        pure $ I.Finite first <=..<= I.Finite second

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 4
