module StringSum
  ( stringSum
  ) where

import Text.Read

-- | Returns Maybe sum of integers in string splitted by spaces.
stringSum
  :: String    -- ^ Given String.
  -> Maybe Int -- ^ Maybe sum of numbers of String.
stringSum = fmap sum . traverse readMaybe . words
