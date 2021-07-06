module ThirdBlock.Task1
  ( maybeConcat
  , eitherConcat
  ) where

import Data.Maybe

-- | The function that takes a list of lists inside.
-- Maybe and returns the concatenation of all the inner lists
maybeConcat
  :: [Maybe [a]] -- ^ given list of Maybe
  -> [a]         -- ^ resulting list
maybeConcat = concatMap (Data.Maybe.fromMaybe [])

-- | Returns pair of concatenations of list of either.
eitherConcat
  :: (Monoid a, Monoid b)
  => [Either a b] -- ^ given list of Either
  -> (a, b)       -- ^ resulting pair
eitherConcat = foldr (\element (first, second) -> case element of
                                                (Left x)  -> (x <> first, second)
                                                (Right x) -> (first, x <> second)) (mempty, mempty)
