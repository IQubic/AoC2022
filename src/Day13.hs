module Day13 where

import Common.Runner
import Common.Parser
import Common.Util (pairs)
import Data.Foldable (asum)
import Data.List (sort)

-- Numbering the packets before filtering
-- Allows us to keep the packet numbers
part1 :: String -> Int
part1 = sum
      . map fst
      . filter ((== LT) . uncurry compare . snd)
      . zip [1..]
      . pInput

part2 :: String -> Int
part2 i = product
        $ map fst
        $ filter (isDecoder . snd)
        $ zip [1..]
        $ sort packets
  where
    -- All packets including decoder packets
    -- Also converts from the part 1 pair groupings to a flat list
    packets :: [Packet Int]
    packets = [p2, p6] ++ concatMap (\(x, y) -> [x, y]) (pInput i)
    isDecoder x = x == p2 || x == p6
    p2 = Seq [Seq [Lit 2]]
    p6 = Seq [Seq [Lit 6]]

data Packet a = Lit a | Seq [Packet a] deriving (Eq, Show)

-- Mostly just a routine implemention of the rules
instance Ord a => Ord (Packet a) where
  compare (Lit x) (Lit y)  = compare x y
  compare (Seq xs) (Lit y) = compare (Seq xs) (Seq [Lit y])
  compare (Lit x) (Seq ys) = compare (Seq [Lit x]) (Seq ys)
  compare (Seq []) (Seq []) = EQ
  compare (Seq _) (Seq []) = GT
  compare (Seq []) (Seq _) = LT
  compare (Seq (x:xs)) (Seq (y:ys)) = case compare x y of
                                        EQ -> compare xs ys
                                        comp -> comp

pInput :: String -> [(Packet Int, Packet Int)]
pInput = pAll $ pPair `sepBy1` eol
  where
    pPair :: Parser (Packet Int, Packet Int)
    pPair = do
      x <- pLine pPacket
      y <- pLine pPacket
      pure (x, y)
    pPacket :: Parser (Packet Int)
    pPacket = Seq <$> between (char '[') (char ']') pList
    -- commaSep fails if given parser fails
    pList = try (commaSep pElem) <|> pure []
    pElem = asum [Lit <$> pNumber, pPacket]

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 13
