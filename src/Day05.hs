{-# LANGUAGE ImportQualifiedPost #-}

module Day05 where

import Common.Runner
import Common.Parser
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Foldable (foldl')

import System.IO

part1 :: String -> String
part1 = getAns reverse

part2 :: String -> String
part2 = getAns id

getAns :: (String -> String)
       -> String
       -> String
getAns f i = map head
           $ M.elems
           $ foldl' (move f) cratesInit moves
  where
    moves = parseMoves i

move :: (String -> String)
     -> Map Int String
     -> Move
     -> Map Int String
move f crates (n, from, to) = let (top, bot) = splitAt n (crates M.! from) in
  M.adjust (f top ++) to $ M.insert from bot crates

cratesInit :: Map Int String
cratesInit = M.fromList $ zip [1..]
           [ "TZB"
           , "NDTHV"
           , "DMFB"
           , "LQVWGJT"
           , "MQFVPGDW"
           , "SFHGQZV"
           , "WCTLRNSZ"
           , "MRNJDWHZ"
           , "SDFLQM"
           ]

type Move = (Int, Int, Int)

-- TODO Write Full Parser
parseMoves :: String -> [Move]
parseMoves = parseAll $ do
  pMove `endBy1` eol
  where
    pMove :: Parser Move
    pMove = do
      n    <- string "move "  *> number
      from <- string " from " *> number
      to   <- string " to "   *> number
      pure (n, from, to)

-- TODO Use actual Parser
-- Day05.txt is the moves only
solve :: Show a => (String -> a) -> IO ()
solve f = do
  handle <- openFile "/home/avi/hs/aoc22/src/Day05.txt" ReadMode
  contents <- hGetContents handle
  print $ f contents

-- solve :: Show a => (String -> a) -> IO (Either AoCError a)
-- solve = runSolutionOnInput 5
