module NaiveGeometry
  ( plus
  , minus
  , naiveScalarProduct
  , naiveCrossProduct
  , naivePerimeter
  , naiveDoubleArea
  ) where

import Geometry (Point(..), X, Y, getX, getY)

invokeNaivePointOp
  :: (Int -> Int -> Int)
  -> Point
  -> Point
  -> Point
invokeNaivePointOp f (Point (x1, y1)) (Point (x2, y2)) = Point $ (,) (x1 `f` x2) (y1 `f` y2)

plus
  :: Point
  -> Point
  -> Point
plus = invokeNaivePointOp (+)

minus
  :: Point
  -> Point
  -> Point
minus = invokeNaivePointOp (-)

prodNaive2
  :: Int
  -> Int
  -> Int
prodNaive2 a b = a * b

naiveScalarProduct
  :: Point
  -> Point
  -> Int
naiveScalarProduct pt1 pt2 = sum [prodNaive2 (getX pt1) (getX pt2),
                                  prodNaive2 (getY pt1) (getY pt2)]

naiveCrossProduct
  :: Point
  -> Point
  -> Int
naiveCrossProduct pt1 pt2 = sum [prodNaive2 (getX pt1) (getY pt2),
                           negate $ prodNaive2 (getY pt1) (getX pt2)]

distance
  :: Point
  -> Point
  -> Double
distance pt1 pt2 = (** 0.5) $ fromIntegral $ (+) (xDiff * xDiff) (yDiff * yDiff)
  where
    xDiff
      :: X
    xDiff = (getX pt2) - (getX pt1)

    yDiff
      :: Y
    yDiff = (getY pt2) - (getY pt1)


countF
  :: Num a
  => (Point -> Point -> a)
  -> [Point]
  -> a
countF _ []         = 0
countF f xs@(point1:_) = snd $ foldr (\newPt (pt, res) -> (newPt, res + f newPt pt)) (point1, 0) xs

naivePerimeter
  :: [Point]
  -> Double
naivePerimeter [] = 0
naivePerimeter xs = countF (distance) xs

naiveDoubleArea
  :: [Point]
  -> Int
naiveDoubleArea  [] = 0
naiveDoubleArea  xs = countF (naiveCrossProduct) xs
