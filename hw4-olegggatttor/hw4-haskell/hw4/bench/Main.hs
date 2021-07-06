module Main where


import Criterion.Main
import Geometry (Point(..), perimeter, doubleArea)
import NaiveGeometry (naivePerimeter, naiveDoubleArea)

getPoints
  :: Int
  -> [Point]
getPoints n = fmap (\x -> Point (x, x)) [1..n]

main
  :: IO ()
main = do
  defaultMain [ bgroup "perimeter" [ bench "perimeter 10^7 points" $ nf perimeter (getPoints (10 ^ 7))
                                   , bench "naive perimeter 10^7 points" $ nf naivePerimeter (getPoints (10 ^ 7))
                                   ]
              , bgroup "doubleArea" [ bench "doubleArea 10^7 points" $ nf doubleArea (getPoints (10 ^ 7))
                                    , bench "naive doubleArea 10^7 points" $ nf naiveDoubleArea (getPoints (10 ^ 7))
                                    ]
              ]
