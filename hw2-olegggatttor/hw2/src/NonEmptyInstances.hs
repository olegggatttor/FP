module NonEmptyInstances
  ( NonEmpty(..)
  ) where

-- NonEmpty list type.
data NonEmpty a
  = a :| [a]
  deriving Show

instance Functor NonEmpty where
  fmap f (x:|xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure x = x :| []
  (f:|fs) <*> (x:|xs) = f x :| ((fmap f xs) ++ (fs <*> (x:xs)))

instance Foldable NonEmpty where
  foldr f z (x:|xs) = f x (foldr f z xs)

instance Monad NonEmpty where
  return = pure
  list >>= f = let
    -- | Concats two NonEmpty lists.
    append
      :: NonEmpty a -- ^ First list.
      -> NonEmpty a -- ^ Second list.
      -> NonEmpty a -- ^ Resulting list.
    append (x:|xs) (y:|ys) = x :| (xs ++ (y:ys))

    -- | Concats given NonEmpty lists inside NonEmpty list.
    concatNonEmpty
      :: NonEmpty (NonEmpty a) -- ^ Given NonEmpty list of NonEmpty lists.
      -> NonEmpty a            -- ^ Concated lists.
    concatNonEmpty (x:|[])     = x
    concatNonEmpty (x:|(y:ys)) = append x (concatNonEmpty (y:|ys))
    in concatNonEmpty $ fmap f list

instance Traversable NonEmpty where
  sequenceA (x:|xs) = fmap (:|) x <*> sequenceA xs
