{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Common.Grid ( module Common.Grid
                   , module Linear.V2
                   , module Data.Finite
                   , (*^)
                   , (^*)
                   ) where

import Common.Util (perturbations)
import Linear.V2
import Linear.Vector ((*^), (^*))
import Control.Lens
import Data.Map.Lens
import Data.Set.Lens
import Data.Tuple.Strict
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Monoid
import Data.Group
import Data.Finite
import GHC.TypeNats
import Data.Ratio
import Data.Proxy
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Containers.NonEmpty

-- LATTICE POINTS
type Point = V2 Int
type FinPoint n = V2 (Finite n)

-- | Gets the cardinal neighbors of a N-dimensional point.
cardinalNeighbors
  :: (Each (f a) (f a) a a, Num a)
  => f a
  -> [f a]
cardinalNeighbors = perturbations (\x -> [x+1,x-1])

-- | Gets the Chebyshev neighbors of an N-dimensional point.
fullNeighbors
  :: (Applicative f, Traversable f, Num (f a), Num a, Eq (f a))
  => f a
  -> [f a]
fullNeighbors p =
    [ p + d
    | d <- sequenceA (pure [-1,0,1])
    , d /= pure 0
    ]

-- | Computes the Manhattan distance between two N-dimensional points.
manhattan :: (Foldable f, Num (f a), Num a) => f a -> f a -> a
manhattan p1 p2 = sum $ abs (p1 - p2)

-- CARDINAL DIRECTIONS
-- | Cardinal directions
data Dir = North | East | South | West
  deriving (Show, Eq, Ord, Enum)

-- | Enumeration of the directions
allDirs :: [Dir]
allDirs = [North ..]

-- | Gets the basis vector for a given direction
-- North is (0,1) here
dirPoint :: Num a => Dir -> V2 a
dirPoint North = V2   0   1
dirPoint East  = V2   1   0
dirPoint West  = V2 (-1)  0
dirPoint South = V2   0 (-1)

-- | Gets the basis vector for a given direction
-- North is (0,-1) here
dirPoint' :: Num a => Dir -> V2 a
dirPoint' North = V2   0 (-1)
dirPoint' East  = V2   1   0
dirPoint' West  = V2 (-1)  0
dirPoint' South = V2   0   1

-- | Rotates a point using a given direction
rotPoint :: Num a => Dir -> V2 a -> V2 a
rotPoint North (V2 x y) = V2   x    y
rotPoint East  (V2 x y) = V2   y  (-x)
rotPoint West  (V2 x y) = V2 (-y)   x
rotPoint South (V2 x y) = V2 (-x) (-y)

-- | Rotates a point using a given direction
rotFin :: forall n. KnownNat n => Dir -> FinPoint n -> FinPoint n
rotFin d = over (mapping centeredFinite) (rotPoint d)

-- | An Iso from a Finite N to a Rational
-- This is just the same number shifted so the center lies at the origin
centeredFinite :: forall n. KnownNat n => Iso' (Finite n) Rational
centeredFinite = iso (subtract r . (% 1) . getFinite)
                 (finite . numerator . (+ r))
  where
    r = fromIntegral (natVal (Proxy @n) - 1) % 2

-- | @<>@ performs a rotation.
instance Semigroup Dir where
  North <> d     = d
  d     <> North = d
  South <> d     = invert d
  d     <> South = invert d
  East  <> East  = South
  East  <> West  = North
  West  <> East  = North
  West  <> West  = South
  stimes n x = case n `mod` 4 of
    1 -> x
    2 -> x <> x
    3 -> invert x -- (x <> x) <> x = invert x
    _ -> North    -- invert x <> x  = mempty
-- | North is the do nothing option.
instance Monoid Dir where
  mempty = North
-- | Invertng a @Dir@ rotates it 180 degrees.
instance Group Dir where
  invert North = South
  invert South = North
  invert East  = West
  invert West  = East
  pow = flip stimes
instance Abelian Dir

-- DIHEDRAL GROUP OF ORDER 8 (D4)
-- First rotate, then flip horizontally, if required
data D4 = D4 { d4Rot :: !Dir, d4Flip :: !Bool }
  deriving (Show, Eq, Ord)

-- | Enumeration of the D4 values
allD4 :: [D4]
allD4 = D4 <$> allDirs <*> [False, True]

-- This is left to right composition of D4s
instance Semigroup D4 where
  D4 x1 False <> D4 x2 y2 = D4 (x1 <> x2) y2
  D4 x1 True  <> D4 x2 y2 = D4 (x1 <> invert x2) (not y2)
instance Monoid D4 where
    mempty = D4 North False
instance Group D4 where
    invert (D4 x False) = D4 (invert x) False
    invert (D4 x True ) = D4 x          True

-- | Orients a point by a @D4@
orientPoint :: Num a => D4 -> V2 a -> V2 a
orientPoint (D4 North False) (V2 x y) = V2   x    y
orientPoint (D4 East  False) (V2 x y) = V2   y  (-x)
orientPoint (D4 West  False) (V2 x y) = V2 (-y)   x
orientPoint (D4 South False) (V2 x y) = V2 (-x) (-y)
orientPoint (D4 North True)  (V2 x y) = V2 (-x)   y
orientPoint (D4 East  True)  (V2 x y) = V2   y    x
orientPoint (D4 West  True)  (V2 x y) = V2 (-y) (-x)
orientPoint (D4 South True)  (V2 x y) = V2   x  (-y)

-- | Orients a point by a @D4@
orientFin :: KnownNat n => D4 -> FinPoint n -> FinPoint n
orientFin d = over (mapping centeredFinite) (orientPoint d)

-- 2D GRIDS
-- | Creates a sparse map representing a 2D square grid from a String
-- Requires newlines between the rows of the grid
asciiGridMap
    :: (Num n, Ord n)
    => (Char -> Maybe a)
    -> String
    -> Map (V2 n) a
asciiGridMap f = toMapOf (gridTraverse <. folding f) . lines

-- | Creates a set representing a 2D square grid from a String
-- Requires newlines between the rows of the grid
asciiGridSet
    :: (Num n, Ord n)
    => (Char -> Bool)
    -> String
    -> Set (V2 n)
asciiGridSet f = setOf (gridTraverse . filtered f . asIndex) . lines

-- | An Indexed Traversal over the elements of a 2D structure
gridTraverse
  :: forall n a b f g. (Num n, Traversable f, Traversable g) =>
  IndexedTraversal (V2 n) (f (g a)) (f (g b)) a b
gridTraverse = icompose (flip toPoint) traversed traversed
  where
    toPoint :: Num n => Int -> Int -> V2 n
    toPoint x y = V2 (fromIntegral x) (fromIntegral y)

-- | Displays a Map of Points as a String
displayAsciiMap
    :: Char             -- ^ missing char
    -> Map Point Char   -- ^ tile map
    -> String
displayAsciiMap missing m = unlines
    [ [ M.findWithDefault missing (V2 x y) m
      | x <- [xMin .. xMax]]
    | y <- [yMin .. yMax]]
  where
    (V2 xMin yMin, V2 xMax yMax) = boundingBox $ M.keysSet m

-- | Displays a Set of Points as a String
displayAsciiSet
    :: Char      -- ^ missing char
    -> Char      -- ^ present char
    -> Set Point -- ^ tile set
    -> String
displayAsciiSet missing here =
  displayAsciiMap missing . M.fromSet (const here)

-- | Returns @'V2' (V2 xMin yMin) (V2 xMax yMax)@.
boundingBox :: (Applicative f, Ord a) => Set (f a) -> (f a, f a)
boundingBox (IsNonEmpty g) = unpack $ foldMap1 pack g
  where
    pack p = T2 (Ap (Min <$> p)) (Ap (Max <$> p))
    unpack (T2 (Ap mn) (Ap mx)) = (getMin <$> mn, getMax <$> mx)
boundingBox _ = error "Empty Grid"

-- | Checks if a point is in a bounding box
inBoundingBox
    :: (Applicative f, Foldable f, Ord a)
    => (f a, f a)
    -> f a
    -> Bool
inBoundingBox (mn, mx) p = and $ check <$> p <*> mn <*> mx
  where
    check p' min' max' = min' <= p' && p' <= max'

-- | Returns the minimum corner of a grid
-- | Only works on structures with data
minCorner :: (Applicative f, Ord a) => Set (f a) -> f a
minCorner (IsNonEmpty g) =
  fmap getMin $ getAp $ foldMap1 (Ap . fmap Min) g
minCorner _ = error "Empty Grid"

-- | Shift corner to (0,0)
-- | Works with possibly empty sets
shiftToZero
    :: (Applicative f, Num (f a), Ord a)
    => Set (f a) -> Set (f a)
shiftToZero g
  | S.null g = S.mapMonotonic (subtract $ minCorner g) g
  | otherwise = g

-- | Returns all the lattice points between the given endpoints
lineBetween :: Point -> Point -> [Point]
lineBetween p0 p1 = [p0 + t *^ step | t <- [0 .. gcf]]
  where
    d@(V2 dx dy) = p1 - p0
    gcf          = gcd dx dy
    step         = (`div` gcf) <$> d

-- -- | Do ocr on a grid in a Map format.
-- -- A Just means that least 50% of letter forms are recognized.
-- -- Unrecognized characters will be replaced with "?".
-- ocrMap :: (a -> Bool) -> Map Point a -> Maybe String
-- ocrMap p = parseLettersWith (view _x) (view _y)
--          . M.keysSet
--          . M.filter p

-- -- | Do ocr on a grid in a set format.
-- -- A Just means that least 50% of letter forms are recognized.
-- -- Unrecognized characters will be replaced with "?".
-- ocrSet :: Set Point -> Maybe String
-- ocrSet = parseLettersWith (view _x) (view _y)
