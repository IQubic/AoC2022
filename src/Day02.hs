{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

module Day02 where

import Common.Runner
import Common.Parser
import Data.Finite
import Data.Foldable (asum)

-- Second column gives shape we throw
-- me + 2 = opp -> L (0)
-- me + 0 = opp -> D (1)
-- me + 1 = opp -> W (2)
part1 :: String -> Integer
part1 = getAns (\_ me -> me) (\opp me -> (me - opp) + 1)

-- Second column gives result
-- L (0) -> opp + 2
-- D (1) -> opp + 0
-- W (2) -> opp + 1
part2 :: String -> Integer
part2 = getAns (\opp res -> opp + (res - 1)) (\_ res -> res)

-- R = 0, P = 1, S = 2
-- L = 0, W = 1, D = 2
getAns :: (Finite 3 -> Finite 3 -> Finite 3)
       -> (Finite 3 -> Finite 3 -> Finite 3)
       -> String -> Integer
getAns shapeVal resVal = sum
                       . map score
                       . pInput
  where
    score :: (Finite 3, Finite 3) -> Integer
    -- Plus 1 to shapeVal because 0-2 isn't 1-3
    score (opp, me) = getFinite (shapeVal opp me) + 1
                    + 3 * getFinite (resVal opp me)


pInput :: String -> [(Finite 3, Finite 3)]
pInput = pLines $ do
  you <- pShape <* char ' '
  (you,) <$> pShape
  where
    pShape = asum [ char 'A' $> finite 0
                  , char 'B' $> finite 1
                  , char 'C' $> finite 2
                  , char 'X' $> finite 0
                  , char 'Y' $> finite 1
                  , char 'Z' $> finite 2
                  ]

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 2
