module Day09 where

import Common.Runner
import Common.Parser
import Common.Util (ordNub)
import Common.Grid
import Data.Foldable (asum)
import Control.Applicative (liftA2)

part1 :: String -> Int
part1 = getAns 1

part2 :: String -> Int
part2 = getAns 9

-- Given an amount of tail segments
-- compute how many unique cells the last tail hits
getAns :: Int -> String -> Int
getAns n i = length
           $ ordNub
           $ tailPaths !! n
  where
    -- Where does the head go?
    headPath = scanl moveHead 0 $ pInput i
    -- Iterate takes a list saying where segment n goes
    -- and figures out where segment n+1 goes, then loop
    tailPaths = iterate (scanl1 updateTail) headPath

-- Move the head once
moveHead :: Point -> Dir -> Point
moveHead h dir = h + dirPoint dir

-- Given a pair of points move the tail closer, if needed
updateTail
  :: Point {- ^ Trailing Point -}
  -> Point {- ^ Leading  Point -}
  -> Point
updateTail t h
  | chebyshev h t < 2 = t -- No movement
  | otherwise = liftA2 closer h t
  where
    -- Where does the tail need to go to be closer on this axis?
    -- Signum finds the offset direction
    closer hy ty = signum (hy - ty) + ty

-- "U 3" -> [North, North, North]
pInput :: String -> [Dir]
pInput = concat . pLines pMove
  where
    pMove :: Parser [Dir]
    pMove = do
      dir <- pDir <* hspace
      n   <- pNumber
      pure $ replicate n dir
    pDir :: Parser Dir
    pDir = asum [ string "U" $> North
                , string "R" $> East
                , string "D" $> South
                , string "L" $> West
                ]

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 9
