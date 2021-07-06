module NotSimpleParser
  ( listParser
  , listListParser
  , ws
  ) where

import BaseCombinators
import Control.Applicative
import Control.Monad (guard, replicateM)
import CopyPaste
import Data.Char (isSpace)
import SimpleParsers (numberParser)


-- | Parses String and removes whitespace in the fron of input.
ws
  :: Parser Char () -- ^ Resulting parser.
ws = ((satisfy isSpace) >> ws) <|> ok

-- | Parses String to list of ints. First number is size of list.
listParser
  :: Parser Char [Int] -- ^ Resulting parser.
listParser = do
             ws
             n <- numberParser
             guard (n >= 0)
             ints <- replicateM n (ws >> element ',' >> ws >> numberParser)
             ws
             return ints

-- | Parses String as List of Int Lists using listParser.
listListParser
  :: Parser Char [[Int]] -- ^ Resulting parser.
listListParser = do
                   numbers <- listParser
                   rest <- (ws >> element ',' >> listListParser) <|> ([] <$ eof)
                   return (numbers : rest)
