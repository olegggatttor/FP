module StringSum where

import Text.Read

stringSum
  :: String
  -> Maybe Int
stringSum = fmap sum . traverse readMaybe . words
