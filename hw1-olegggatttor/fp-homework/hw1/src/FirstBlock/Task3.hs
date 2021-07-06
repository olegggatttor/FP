{-# LANGUAGE InstanceSigs #-}

module FirstBlock.Task3
  ( Tree(..)
  , isEmpty
  , count
  , find
  , insert
  , FirstBlock.Task3.fromList
  , remove
  ) where

import Data.List.NonEmpty hiding (insert)
import Prelude hiding (length)

type Left a = Tree a
type Right a = Tree a

-- | Tree data struct with empty leaves and nodes with nonempty lists.
data Tree a
  = Leaf
  | Node (NonEmpty a) (Left a) (Right a)
  deriving Show

-- | Check if given tree is empty.
isEmpty
  :: Tree a -- ^ given tree
  -> Bool   -- ^ True if tree is empty and False otherwise
isEmpty Leaf = True
isEmpty _    = False

-- | Returns amount of elements in the given tree.
count
  :: Tree a -- ^ given tree
  -> Int    -- ^ amount of elements in the given tree
count Leaf              = 0
count (Node values l r) = length values + count l + count r

-- | Check wether the tree contains given element.
find :: Ord a -- ^ given tree must contains ordered type in nodes
  => a        -- ^ given element
  -> Tree a   -- ^ given tree
  -> Bool     -- ^ True if tree contains given element and False otherwise
find _       Leaf          = False
find element (Node (cur:|_) l r)
  | cur == element         = True
  | cur < element          = find element r
  | otherwise              = find element l

-- | Inserts given element into the tree.
insert
  :: Ord a  -- ^ given tree must contains ordered type in nodes
  => a      -- ^ given element
  -> Tree a -- ^ given tree
  -> Tree a -- ^ new tree with inserted element
insert element Leaf = Node (element :| []) Leaf Leaf
insert element (Node values@(cur:|rest) l r)
  | cur == element  = Node (element:|(cur:rest)) l r
  | cur < element   = Node values l (insert element r)
  | otherwise       = Node values (insert element l) r

-- | Converts given list to tree.
fromList
  :: Ord a  -- ^ type of tree and list should be ordered
  => [a]    -- ^ given list
  -> Tree a -- ^ resulting tree
fromList = foldr insert Leaf

-- | Remove given element from tree.
remove
  :: Ord a  -- ^ given tree must contain ordered type in nodes
  => a      -- ^ given element
  -> Tree a -- ^ given tree
  -> Tree a -- ^ resulting tree with removed element
remove _       Leaf                     = Leaf
remove element node@(Node values@(cur:|rest) Leaf Leaf)
  | cur == element && length values > 1 = Node (Data.List.NonEmpty.fromList rest) Leaf Leaf
  | cur == element                      = Leaf
  | otherwise = node
remove element node@(Node values@(cur:|rest) l Leaf)
  | cur == element && length values > 1 = Node (Data.List.NonEmpty.fromList rest) l Leaf
  | cur == element                      = l
  | cur > element                       = Node values (remove element l) Leaf
  | otherwise                           = node
remove element node@(Node values@(cur:|rest) Leaf r)
  | cur == element && length values > 1 = Node (Data.List.NonEmpty.fromList rest) Leaf r
  | cur == element                      = r
  | cur < element                       = Node values Leaf (remove element r)
  | otherwise = node
remove element (Node values@(cur:|rest) l@(Node _ _ _) r@(Node _ _ _))
  | cur > element                       = Node values (remove element l) r
  | cur < element                       = Node values l (remove element r)
  | cur == element && length values > 1 = Node (Data.List.NonEmpty.fromList rest) l r
  | otherwise                           = Node (findNext r) l (removeNext r) where
                                                    findNext :: Tree a -> NonEmpty a
                                                    findNext (Node xs Leaf _) = xs
                                                    findNext (Node _ left _)  = findNext left
                                                    findNext Leaf             = undefined

                                                    removeNext :: Tree a -> Tree a
                                                    removeNext (Node _ Leaf right) = right
                                                    removeNext (Node _ left _)     = removeNext left
                                                    removeNext Leaf                = undefined

instance Foldable Tree where
  foldr
    :: (a -> b -> b)
    -> b
    -> Tree a
    -> b
  foldr _ zero Leaf              = zero
  foldr f zero (Node values l r) = foldr f (foldr f (foldr f zero r) values) l

  foldMap
    :: Monoid m
    => (a -> m)
    -> Tree a
    -> m
  foldMap _ Leaf              = mempty
  foldMap f (Node values l r) = foldMap f l <> foldMap f values <> foldMap f r
