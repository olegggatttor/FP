module SimpleParsers where

import CopyPaste
import BaseCombinators
import Control.Applicative
import Data.Char
import Control.Monad

bracketParser
  :: Parser Char Char
bracketParser = bracketSeq >> ('ε' <$ eof) where
                  bracketSeq
                    :: Parser Char Char
                  bracketSeq = (element '(' >> bracketSeq >> element ')' >> bracketSeq)
                               <|> ('ε' <$ ok)
getDigits
  :: Parser Char [Int]
getDigits = fmap (fmap digitToInt) digits where
                    digits = ((satisfy isDigit >>= \x -> fmap (x:) digits) <|> ([] <$ ok))

ws
  :: Parser Char ()
ws = ((satisfy isSpace) >> ws) <|> ok

numberParser
  :: Parser Char Int
numberParser = do
               sign   <- id <$ element '+' <|> negate <$ element '-' <|> id <$ ok
               ws
               numberDigits <- getDigits
               guard (not $ null numberDigits)
               let number = foldl (\counted cur -> 10 * counted + cur) 0 numberDigits
               return (sign number)
