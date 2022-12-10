{-# LANGUAGE ImportQualifiedPost #-}

module Day10 where

import Common.Runner
import Common.Parser
import Common.Grid
import Data.Foldable (asum)
import Data.Set qualified as S

part1 :: String -> Int
part1 = sum
      . map (uncurry (*))
      . filter ((`elem` cycles) . fst)
      . execute
      . pInput
  where
    cycles = [20,60..220]

-- part2 :: String -> String
part2 = displayAsciiSet '.' '#'
      . S.fromList
      . map (screenPos . fst)
      . filter visible
      . execute
      . pInput
  where
    visible (cycle, x) = abs (horiz cycle - x) <= 1
    -- CRT starts at pos 0 on cycle 1
    horiz cycle = (cycle - 1) `mod` 40
    -- flip gives x = cycle `mod` 40, y = cycle `div` 40
    screenPos :: Int -> Point
    screenPos = uncurry (flip V2) . (`divMod` 40)

-- Fst is cycle number
-- Snd is X val
execute :: [Op] -> [(Int, Int)]
execute ops = zip [1..] $ go 1 ops
  where
    go :: Int -> [Op] -> [Int]
    go n []          = []
    go n (Nop:ops)   = n     :  go n     ops
    go n (Add x:ops) = [n,n] ++ go (n+x) ops -- x only changes after two cycles

data Op = Nop | Add Int deriving Show

pInput :: String -> [Op]
pInput = pLines $ asum [ string "noop" $> Nop
                       , pAdd]
  where
    pAdd = do
      n <- string "addx " *> pNumber
      pure $ Add n

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 10
