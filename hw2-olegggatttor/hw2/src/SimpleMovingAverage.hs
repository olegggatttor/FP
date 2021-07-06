module SimpleMovingAverage
  ( moving
  ) where

import Control.Monad.State
import Data.Maybe (fromMaybe)

-- | Queue type for sma algorithm.
data Queue a
  = Queue [a] [a]
  deriving Show

-- | Adds element to the back Queue.
add
  :: Queue a -- ^ Given Queue.
  -> a       -- ^ Given element to add.
  -> Queue a -- ^ New Queue with added element.
add (Queue front back) x = Queue front (x:back)

-- | Pops head of the Queue.
pop
  :: Queue a            -- ^ Given Queue.
  -> (Maybe a, Queue a) -- ^ Poped element (or Nothing if Queue was empty) and new Queue.
pop q@(Queue [] [])        = (Nothing, q)
pop (Queue [] back)        = pop $ Queue  (reverse back)  []
pop (Queue (x:front) back) = (Just x, Queue front back)

-- | Simple Moving Average algorithm.
simpleMovingAverage
  :: Int                                     -- ^ Given size of window.
  -> [Int]                                   -- ^ Given numbers.
  -> State ([Float], Queue Int, Int) [Float] -- ^ State monad of (CountedSMA, Queue of numbers, Size of Queue)
                                             -- ^ and CounteSMA as result.

-- | Converts Bool to Float value.
boolToFloat
  :: Bool   -- ^ Given Bool.
  -> Float  -- ^ One or Zero as a result.
boolToFloat True  = 1.0
boolToFloat False = 0.0

simpleMovingAverage _ []     = do
                               (result, _, _) <- get
                               return result
simpleMovingAverage n (x:xs) = do
                               (counted, prevQueue, sz) <- get
                               let (oldMaybe, updatedQueue) = if sz < n
                                                              then (Just 0, prevQueue)
                                                              else pop prevQueue
                               let old = fromMaybe 0 oldMaybe
                               let divider = fromIntegral (min n (sz + 1)) :: Float
                               let isLessThanN = boolToFloat (sz < n)
                               let avg = ((divider - isLessThanN) * (head counted) +
                                          (fromIntegral (x - old) :: Float)) / divider
                               let newQ = add updatedQueue x
                               put (avg:counted, newQ, min n (sz + 1))
                               simpleMovingAverage n xs

-- | Simple Moving Average algorithm start point.
moving
  :: Int     -- ^ Given size of window.
  -> [Int]   -- ^ Given numbers.
  -> [Float] -- ^ Resulting list.
moving _ []     = []
moving n (x:xs) = reverse $ (evalState $ simpleMovingAverage n xs)
                              ([fromIntegral x :: Float], (Queue [x] []), 1)
