module Day02 where

import Common.Runner
import Common.Parser
import Data.Foldable (asum)

part1 :: String -> Int
part1 = getAns outcome
  where
    outcome :: Round -> (Shape, Outcome)
    outcome (Rock,     'X') = (Rock,     Draw)
    outcome (Rock,     'Y') = (Paper,    Win)
    outcome (Rock,     'Z') = (Scissors, Lose)
    outcome (Paper,    'X') = (Rock,     Lose)
    outcome (Paper,    'Y') = (Paper,    Draw)
    outcome (Paper,    'Z') = (Scissors, Win)
    outcome (Scissors, 'X') = (Rock,     Win)
    outcome (Scissors, 'Y') = (Paper,    Lose)
    outcome (Scissors, 'Z') = (Scissors, Draw)
    outcome _ = error "Impossible"

part2 :: String -> Int
part2 = getAns outcome
  where
    outcome :: Round -> (Shape, Outcome)
    outcome (Rock,     'X') = (Scissors, Lose)
    outcome (Rock,     'Y') = (Rock,     Draw)
    outcome (Rock,     'Z') = (Paper,    Win)
    outcome (Paper,    'X') = (Rock,     Lose)
    outcome (Paper,    'Y') = (Paper,    Draw)
    outcome (Paper,    'Z') = (Scissors, Win)
    outcome (Scissors, 'X') = (Paper,    Lose)
    outcome (Scissors, 'Y') = (Scissors, Draw)
    outcome (Scissors, 'Z') = (Rock,     Win)
    outcome _ = error "Impossible"



getAns :: (Round -> (Shape, Outcome)) -> String -> Int
getAns outcome = sum
               . map (score . outcome)
               . parseInput
  where
    score (shape, res) = shapeVal shape + resVal res
    shapeVal Rock     = 1
    shapeVal Paper    = 2
    shapeVal Scissors = 3
    resVal   Lose     = 0
    resVal   Draw     = 3
    resVal   Win      = 6

data Shape   = Rock | Paper | Scissors
data Outcome = Lose | Draw  | Win
type Round = (Shape, Char)


parseInput :: String -> [Round]
parseInput = parseLines $ do
  you <- pShape <* char ' '
  (you,) <$> anySingle
  where
    pShape = asum [ char 'A' $> Rock
                  , char 'B' $> Paper
                  , char 'C' $> Scissors
                  ]

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 2
