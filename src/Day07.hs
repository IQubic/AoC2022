{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

module Day07 where

import Common.Runner
import Common.Parser
import Data.Foldable (asum)
import Data.Char (isSpace)
import Data.List (inits)
import Data.Map (Map)
import Data.Map qualified as M


part1 :: String -> Int
part1 i = sum [n | n <- M.elems sizes, n <= 100_000]
  where
    sizes = dirSizes $ pInput i

part2 :: String -> Int
part2 i = minimum [n | n <- M.elems sizes, n >= required]
  where
    sizes = dirSizes $ pInput i
    free = 70_000_000 - sizes M.! ["/"]
    required = 30_000_000 - free

-- Get the size of each dir
-- files in /foo/bar add to the size of /, /foo, and /foo/bar
dirSizes :: [TermCmd] -> Map Path Int
dirSizes xs = M.fromListWith (+) [(d',n) | (d,n) <- go [] xs, d' <- inits d]
  where
    -- go gets the size of all the files (not dirs) at each level
    go :: Path -> [TermCmd] -> [(Path, Int)]
    go _   []              = []
    go _   (CD "/"   : xs) = go ["/"] xs
    go cwd (CD ".."  : xs) = go (init cwd) xs
    go cwd (CD dir   : xs) = go (cwd ++ [dir]) xs
    go cwd (LS files : xs) = (cwd, sum [n | (File n _) <- files]) : go cwd xs

type Path = [String]
data TermCmd = CD String
             | LS [LSOutput]
             deriving Show
data LSOutput = File Int String
              | Dir String
              deriving Show

-- Create a list of term commands, which contain output
pInput :: String -> [TermCmd]
pInput = pAll $ many $ asum [pCD, pLS]
  where
    pCD :: Parser TermCmd
    pCD = CD <$> (string "$ cd " *> pID <* eol)
    pLS :: Parser TermCmd
    pLS = do
      string "$ ls\n"
      LS <$> asum [pDir, pFile] `sepEndBy1` eol
    pFile = do
      size <- pNumber <* spaceChar
      File size <$> pID
    pDir = Dir <$> (string "dir " *> pID)
    pID :: Parser String
    pID = some $ satisfy (not . isSpace)

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 7
