{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

module Day11 where

import Common.Runner
import Common.Parser
import Common.Util (times)
import Data.Foldable (asum)
import Data.List (sortOn, partition)
import Data.Ord (Down(..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM

part1 :: String -> Int
part1 = getAns 20 Nothing . pInput

part2 :: String -> Int
part2 i = getAns 10_000 (Just reducer) ms
  where
    reducer = foldl1 lcm $ map test $ IM.elems ms
    ms = pInput i

getAns :: Int -> Maybe Int -> IntMap Monkey -> Int
getAns n reducer = product
                 . take 2
                 . sortOn Down
                 . IM.elems
                 . fmap throws
                 . times n (runRound reducer)

-- Run a single round of monkeys
runRound :: Maybe Int -> IntMap Monkey -> IntMap Monkey
runRound reducer ms = foldl (runMonkey reducer) ms (IM.keys ms)

-- Run a single monkey
runMonkey :: Maybe Int -> IntMap Monkey -> Int -> IntMap Monkey
runMonkey reducer ms idx =
  let m = ms IM.! idx in
    case items m of
      [] -> ms -- no items
      xs ->
        let op = case reducer of Just modulus -> \item -> f m item `mod` modulus
                                 Nothing      -> \item -> f m item `div` 3
            (trues, falses) = partition (\item -> item `mod` test m == 0)
                            $ map op $ items m -- Apply reducing function, and THEN test
        in -- Append all trues and falses, clear current monkey's list, and update throws
          IM.adjust (pushMany trues)  (ifTrue m) $
          IM.adjust (pushMany falses) (ifFalse m) $
          IM.adjust (\x -> x{items = [], throws = throws m + length (items m)}) idx ms

-- Push many items to the list of a given monkey's items
pushMany :: [Int] -> Monkey -> Monkey
pushMany xs m = m {items = items m ++ xs}

-- What a monkey is
data Monkey = Monkey { items :: [Int]
                     , f :: Int -> Int
                     , test :: Int
                     , ifTrue :: Int
                     , ifFalse :: Int
                     , throws :: Int
                     }

pInput :: String -> IntMap Monkey
pInput = IM.fromList . pAll (pMonkey `sepBy1` eol)
  where
    pMonkey :: Parser (Int, Monkey)
    pMonkey = do
      n <- pLine (string "Monkey " *> pNumber <* char ':')
      items <- pLine (hspace *> string "Starting items: " *> commaSep pNumber)
      op <- pLine (hspace *> string "Operation: new = old " *> pOp)
      test <- pLine (hspace *> string "Test: divisible by " *> pNumber)
      ifTrue <- pLine (hspace *> string "If true: throw to monkey " *> pNumber)
      ifFalse <- pLine (hspace *> string "If false: throw to monkey " *> pNumber)
      pure (n, Monkey items op test ifTrue ifFalse 0)
    pOp :: Parser (Int -> Int)
    pOp = do
      op <- anySingle <* hspace
      rhs <- asum [string "old" $> Nothing, Just <$> pNumber]
      pure $ eval op rhs
    eval :: Char -> Maybe Int -> (Int -> Int)
    eval '*' (Just n) x = x * n
    eval '+' (Just n) x = x + n
    eval '*' Nothing  x = x * x
    eval '+' Nothing  x = x + x
    eval op _ _ = error ("Unexpected operation: " ++ [op])

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 11
