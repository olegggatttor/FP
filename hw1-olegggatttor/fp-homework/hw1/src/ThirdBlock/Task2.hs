module ThirdBlock.Task2 where

-- | Non empty list of data
data NonEmpty a
  = a :| [a]
  deriving (Show)

instance Eq a => Eq (NonEmpty a) where
  (==) (x:|xs) (y:|ys) = x == y && xs == ys

instance Semigroup (NonEmpty a) where
  (x:|xs) <> (y:|ys) = x :| (xs ++ (y:ys))

-- | ThisOrThat data type
data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving Show

instance (Eq a, Eq b) => Eq (ThisOrThat a b) where
  (==) (This x)   (This y)   = x == y
  (==) (That x)   (That y)   = x == y
  (==) (Both a b) (Both x y) = a == x && b == y
  (==) _          _          = False

instance Semigroup (ThisOrThat a b) where
       (This _)   <> (This x)   = This x
       (This x)   <> (That y)   = Both x y

       (That _)   <> (That x)   = That x
       (That x)   <> (This y)   = Both y x

       (Both _ y) <> (This x)   = Both x y
       (Both x _) <> (That y)   = Both x y

       _ <> (Both x y) = Both x y

-- | Name data type
newtype Name
  = Name [Char]
  deriving Show

instance Eq Name where
  (==) (Name a) (Name b) = a == b

instance Semigroup Name where
  (Name "") <> (Name y)  = Name y
  (Name x)  <> (Name "") = Name x
  (Name x)  <> (Name y)  = Name (x ++ ('.' : y))

instance Monoid Name where
  mempty = Name ""

newtype Endo a
  = Endo { getEndo :: a -> a}

instance Semigroup (Endo a) where
  (Endo f) <> (Endo g) = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id
