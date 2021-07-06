module SimpleMovingAverage where

import Control.Monad.State
import Data.Maybe


data Queue a
  = Queue [a] [a]
  deriving Show

add
  :: Queue a
  -> a
  -> Queue a
add (Queue front back) x = Queue front (x:back)

pop
  :: Queue a
  -> (Maybe a, Queue a)
pop q@(Queue [] []) = (Nothing, q)
pop (Queue [] back) = pop $ Queue  (reverse back)  []
pop (Queue (x:front) back) = (Just x, Queue front back)

size
  :: Queue a
  -> Int
size (Queue front back) = length front + length back

simpleMovingAverage
  :: Int
  -> [Int]
  -> State ([Float], Queue Int) [Float]
simpleMovingAverage _ []     = do
                               (result, _) <- get
                               return result
simpleMovingAverage n (x:xs) = do
                               (counted, prevQueue) <- get
                               let sz = size prevQueue
                               let (oldMaybe, updatedQueue) = if sz < n
                                                              then (Just 0, prevQueue)
                                                              else pop prevQueue
                               let old = fromMaybe 0 oldMaybe
                               let divider = if sz < n
                                             then (fromIntegral (sz + 1) :: Float)
                                             else (fromIntegral n :: Float)
                               let avg = if sz < n
                                         then ((divider - 1) * (head counted) +
                                              (fromIntegral (x - old) :: Float)) / divider
                                         else head counted +
                                              ((fromIntegral (x - old) :: Float) / divider)
                               put (avg:counted, add updatedQueue x)
                               simpleMovingAverage n xs
moving
  :: Int
  -> [Int]
  -> [Float]
moving _ []     = []
moving n (x:xs) = reverse $ (evalState $ simpleMovingAverage n xs)
                              ([fromIntegral x :: Float], (Queue [x] []))
