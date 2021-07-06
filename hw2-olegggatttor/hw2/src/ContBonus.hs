module ContBonus where

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  fmap f (Cont g) = Cont $ \k -> g (k . f)

instance Applicative (Cont r) where
  pure x = Cont $ \k -> k x

  (Cont f) <*> (Cont g) = Cont $ \k -> f $  \h -> g $ k . h

instance Monad (Cont r) where
  return = pure

  (Cont f) >>= g = Cont $ \k -> f (\a -> (runCont $ g a) k)
