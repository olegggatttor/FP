module TreeInstances where

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving Show

instance Eq a => Eq (Tree a) where
  (Branch a b) == (Branch c d) = a == c && b == d
  (Leaf a)     == (Leaf b)     = a == b
  _            == _            = False

instance Functor (Tree) where
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative (Tree) where
  pure = Leaf

  (Branch f g) <*> (Branch l r) = Branch (f <*> l) (g <*> r)
  (Branch f g) <*> leaf         = Branch (f <*> leaf) (g <*> leaf)
  (Leaf f)     <*> tree         = fmap f tree

instance Foldable (Tree) where
  foldr f z (Leaf x) = f x z
  foldr f z (Branch l r) = foldr f (foldr f z r) l

instance Traversable (Tree) where
  traverse f (Leaf x) = fmap Leaf $ f x
  traverse f (Branch l r) = (fmap Branch $ traverse f l) <*> traverse f r
