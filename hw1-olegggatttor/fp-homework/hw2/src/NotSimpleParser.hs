module NotSimpleParser where

import CopyPaste
import BaseCombinators
import SimpleParsers
import Control.Monad
import Control.Applicative

listParser
  :: Parser Char [Int]
listParser = do
             ws
             n <- numberParser
             ints <- replicateM n (ws >> element ',' >> ws >> numberParser >>= \x -> x <$ ws)
             return ints

listListParser
  :: Parser Char [[Int]]
listListParser = do
                   ws
                   numbers <- listParser
                   ws
                   rest <- (element ',' >> listListParser) <|> ([] <$ eof)
                   return ([numbers] ++ rest)
