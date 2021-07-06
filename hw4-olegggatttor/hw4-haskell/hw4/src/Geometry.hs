{-# LANGUAGE BangPatterns #-}

module Geometry
  ( plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , doubleArea
  , Point(..)
  , X
  , Y
  , getX
  , getY
  ) where

import Data.Foldable (foldr')

-- | Type alias for x Point coordinate
type X = Int

-- | Type alias for y Point coordinate
type Y = Int

-- | 2D Point type
newtype Point
  = Point { coordinates :: (X, Y) }
  deriving Show

-- | X getter
getX
  :: Point -- ^ Given Point
  -> X     -- ^ Resulting X coordinate
getX = fst . coordinates

-- | Y getter
getY
  :: Point -- ^ Given Point
  -> Y     -- ^ Resulting X coordinate
getY = snd . coordinates



-- | Performs point operation
invokePointsOp
  :: (Int -> Int -> Int) -- ^ Given coordinates operation
  -> Point               -- ^ First point
  -> Point               -- ^ Second point
  -> Point               -- ^ Resulting point
invokePointsOp f (Point (x1, y1)) (Point (x2, y2)) = newX `seq` newY `seq` (Point $ (,) newX newY)
  where
    newX
      :: Int
    newX = x1 `f` x2

    newY
      :: Int
    newY = y1 `f` y2

-- | Sums two points
plus
  :: Point -- ^ First point
  -> Point -- ^ Second point
  -> Point -- ^ Resulting point
plus = invokePointsOp (+)

-- | Differ two points
minus
  :: Point -- ^ First point
  -> Point -- ^ Second point
  -> Point -- ^ Resulting point
minus = invokePointsOp (-)

-- | Non-lazy sum function
sum'
  :: Num a -- ^ a must be Num
  => [a]   -- ^ List of numbers
  -> a     -- ^ Resulting sum
sum' = foldr' (+) 0

-- | Non-lazy production
prod2
  :: Int -- ^ First Int
  -> Int -- ^ Second Int
  -> Int -- ^ Result of production
prod2 a b = foldr' (*) 1 [a, b]

-- | Scalar product
scalarProduct
  :: Point -- ^ First point
  -> Point -- ^ Second point
  -> Int   -- ^ Result of production
scalarProduct pt1 pt2 = sum' [prod2 (getX pt1) (getX pt2), prod2 (getY pt1) (getY pt2)]

-- | Cross product
crossProduct
  :: Point -- ^ First point
  -> Point -- ^ Second point
  -> Int   -- ^ Result of production
crossProduct pt1 pt2 = sum' [prod2 (getX pt1) (getY pt2), negate $ prod2 (getY pt1) (getX pt2)]

-- | Calculates distance between two points
distance
  :: Point  -- ^ First point
  -> Point  -- ^ Second point
  -> Double -- ^ Distance
distance pt1 pt2 = (** 0.5) $ fromIntegral $ (+) (xDiff * xDiff) (yDiff * yDiff)
  where
    xDiff
      :: X
    xDiff = (getX pt2) - (getX pt1)

    yDiff
      :: Y
    yDiff = (getY pt2) - (getY pt1)

-- | Folds list of point with function
countF
  :: Num a                 -- ^ a must be Num
  => (Point -> Point -> a) -- ^ Given funciton
  -> [Point]               -- ^ Given list
  -> a                     -- ^ Num result
countF _ []         = 0
countF f xs@(point1:_) = snd $ foldr (\newPt (pt, !res) -> (newPt, res + f newPt pt)) (point1, 0) xs

-- | Non-lazy calculation of perimeter
perimeter
  :: [Point] -- ^  List of points
  -> Double  -- ^  Resulting perimeter
perimeter [] = 0
perimeter xs = countF (distance) xs

-- | Non-lazy calculation of doubled area
doubleArea
  :: [Point] -- ^  List of points
  -> Int     -- ^  Resulting perimeter
doubleArea  [] = 0
doubleArea  xs = countF (crossProduct) xs
