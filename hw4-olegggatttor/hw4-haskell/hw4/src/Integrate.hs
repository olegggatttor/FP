module Integrate
  ( integrateInParallel
  , integrateSequentially
  ) where

import Control.Monad.Par.Combinator (parMapReduceRange, InclusiveRange(..))
import Control.Monad.Par (Par(..), runPar)
import Data.Foldable (foldl')
import System.Random (mkStdGen, randomR)

-- | Function to integrate
function
  :: Double -- ^ function argument
  -> Int    -- ^ Generator seed
  -> Double -- ^ result
function x seed = do
  let gen = mkStdGen seed
  let (val, _) = randomR (x, x + 1.0) gen
  (exp val) * cos (cos val - val ^ 2)


-- | Integrate function in parallel
integrateInParallel
  :: Int    -- ^ Left bound
  -> Int    -- ^ Right bound
  -> Int    -- ^ Seed
  -> Double -- ^ Result of integration
integrateInParallel a b seed | a == b    = 0
                             | otherwise = runPar $ parMapReduceRange (InclusiveRange a (b - 1))
                                           (\x -> return $ function (fromIntegral x :: Double) seed)
                                           (\x y -> return $ x + y)
                                           0.0
-- | Integrate function sequentially
integrateSequentially
  :: Int    -- ^ Left bound
  -> Int    -- ^ Right bound
  -> Int    -- ^ Seed
  -> Double -- ^ Result of integration
integrateSequentially a b seed | a == b = 0
                               | otherwise =
                                  (foldl' (+) 0 (map (\x -> function (fromIntegral x) seed) [a..b]))
