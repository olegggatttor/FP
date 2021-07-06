module HashTable
  where

import Control.Concurrent.STM (STM, TVar, atomically, readTVar, newTVar, modifyTVar', writeTVar)
import Data.Hashable (Hashable(..))
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Foldable (for_)


newtype Table k v
  = Table (V.Vector (TVar (M.Map k v)))

type Size = Int

data ConcurrentHashTable k v
  = CHT
  { count :: TVar Size
  , table :: TVar (Table k v)
  }


findByKey
  :: (Hashable k, Ord k)
  => k
  -> Table k v
  -> STM (Maybe v)
findByKey key (Table vec) = do
  let pos = (hash key) `mod` (length vec)
  bucket <- readTVar $ vec V.! pos
  return $ bucket M.!? key


initSize
  :: Size
initSize = 10

expandMultiplier
  :: Int
expandMultiplier = 2

newCHT
  :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  sz  <- newTVar 0
  vec <- V.replicateM initSize (newTVar M.empty)
  cht <- newTVar $ Table vec
  return $ CHT sz cht

getCHT
  :: (Hashable k, Ord k)
  => k
  -> ConcurrentHashTable k v
  -> IO (Maybe v)
getCHT key cht = atomically $ do
  hashTable <- readTVar $ table cht
  findByKey key hashTable

putCHT
  :: (Hashable k, Ord k)
  => k
  -> v
  -> ConcurrentHashTable k v
  -> IO ()
putCHT key value cht = atomically $ do
  (Table hashTable) <- readTVar $ table cht
  let len = length hashTable
  sz                <- readTVar $ count cht
  if (expandMultiplier * sz + 1 >= len)
  then do
    let newSize = expandMultiplier * len
    vec <- V.replicateM newSize (newTVar M.empty)
    V.forM_ hashTable $ \tVarMap -> do
      mp <- readTVar tVarMap
      for_ (M.toList mp) $ \(k, v) -> do
        let pos = (hash k) `mod` newSize
        modifyTVar' (vec V.! pos) (M.insert k v)
    let newElemPos = (hash key) `mod` newSize
    modifyTVar' (vec V.! newElemPos) (M.insert key value)
    modifyTVar' (count cht) ((+) 1)
    writeTVar (table cht) (Table vec)
  else do
    let pos = (hash key) `mod` len
    modifyTVar' (hashTable V.! pos) (M.insert key value)
    modifyTVar' (count cht) ((+) 1)
    return ()


sizeCHT
  :: ConcurrentHashTable k v -> IO Int
sizeCHT table = atomically $ readTVar $ count table
