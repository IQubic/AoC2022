module Day13 where

import Common.Runner
import Common.Parser
import Common.Util (indicesWhere)
import Data.Foldable (asum)
import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)

part1 :: String -> Int
part1 i = sum [idx | (idx,(x,y)) <- zip [1..] (pInput i), x < y]

-- Need to account for first packet having index 1
part2 :: String -> Int
part2 i = product
        $ map (+1)
        $ indicesWhere (`elem` extras)
        $ sort packets
  where
    -- All packets including decoder packets
    -- Also converts from the part 1 pair groupings to a flat list
    packets :: [Packet Int]
    packets = extras ++ concatMap (\(x, y) -> [x, y]) (pInput i)
    extras = [ Seq [Seq [Lit n]] | n <- [2, 6] ]

-- Recursive packets are FUN!!!
data Packet a = Lit a | Seq [Packet a] deriving (Eq, Show)

-- Just a routine implemention of the rules
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
    pPacket = asum [ Seq <$> between (char '[') (char ']') (commaSep pPacket)
                   , Lit <$> pNumber
                   ]

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 13
