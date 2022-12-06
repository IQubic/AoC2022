{-# LANGUAGE ImportQualifiedPost #-}

module Day05 where

import Common.Runner
import Common.Parser
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Foldable (asum, foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

part1 :: String -> String
part1 = getAns reverse

part2 :: String -> String
part2 = getAns id

getAns :: (String -> String)
       -> String
       -> String
getAns f i = map head
           $ M.elems
           $ foldl' (move f) crateInit moves
  where
    (crateInit, moves) = pInput i

move :: (String -> String)
     -> CrateMap
     -> Move
     -> CrateMap
move f crates (n, from, to) = let (top, bot) = splitAt n (crates M.! from) in
  M.adjust (f top ++) to $ M.insert from bot crates


type Move     = (Int, Int, Int)
type CrateMap = Map Int String

pInput :: String -> (CrateMap, [Move])
pInput = pAll $ do
  ascii  <- (pCell `sepBy1` char ' ') `sepEndBy1` eol
  labels <- pLine (hspace *> (pNumber `endBy1` hspace)) <* eol
  moves  <- pMove `endBy1` eol
  let crates = M.fromList $ zip labels $ map catMaybes $ transpose ascii
  pure (crates, moves)
  where
    pCell :: Parser (Maybe Char)
    pCell = asum [ Just    <$> between (char '[') (char ']') anySingle
                 , Nothing <$  string "   "
                 ]
    pMove :: Parser Move
    pMove = do
      n    <- string "move "  *> pNumber
      from <- string " from " *> pNumber
      to   <- string " to "   *> pNumber
      pure (n, from, to)

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 5
