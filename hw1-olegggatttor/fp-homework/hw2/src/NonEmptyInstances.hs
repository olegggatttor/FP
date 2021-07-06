module NonEmptyInstances where

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
    append
      :: NonEmpty a
      -> NonEmpty a
      -> NonEmpty a
    append (x:|xs) (y:|ys) = x :| (xs ++ (y:ys))

    concatNonEmpty
      :: NonEmpty (NonEmpty a)
      -> NonEmpty a
    concatNonEmpty (x:|[]) = x
    concatNonEmpty (x:|(y:ys)) = append x (concatNonEmpty (y:|ys))
    in concatNonEmpty $ fmap f list

instance Traversable NonEmpty where
  sequenceA (x:|xs) = fmap (:|) x <*> sequenceA xs
