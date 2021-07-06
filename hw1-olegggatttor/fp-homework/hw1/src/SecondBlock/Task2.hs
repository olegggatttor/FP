module SecondBlock.Task2
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty hiding (init)

-- | Splits given list by separator.
splitOn
  :: Eq a         -- ^ type of list must be instance of Eq
  => a            -- ^ given separator
  -> [a]          -- ^ given list
  -> NonEmpty [a] -- ^ resulting nonempty list of splitted data
splitOn element = foldr (\symbol list@(x:|xs) -> if symbol == element
                                                 then []:|toList list
                                                 else (symbol:x):|xs) (fromList [[]])

-- | Joins given list with given element.
joinWith
  :: a            -- ^ given element for joining
  -> NonEmpty [a] -- ^ splitted list
  -> [a]          -- ^ joined list
joinWith separator = init . foldr (\part list -> part ++ (separator:list)) [] . toList
