module SimpleParsers
  ( bracketParser
  , numberParser
  ) where

import BaseCombinators (element, eof, ok, satisfy)
import Control.Applicative
import Control.Monad (guard)
import CopyPaste
import Data.Char (isDigit)

-- | Parses correct bracket sequence and otherwise fails.
bracketParser
  :: Parser Char Char -- ^ Resulting parser.
bracketParser = bracketSeq >> ('ε' <$ eof) where
                  bracketSeq
                    :: Parser Char Char
                  bracketSeq = (element '(' >> bracketSeq >> element ')' >> bracketSeq)
                               <|> ('ε' <$ ok)

-- | Parser that parses sequence of digits.
getDigits
  :: Parser Char String -- ^ Resulting parser.
getDigits = digits where
                     digits = ((satisfy isDigit >>= \x -> fmap (x:) digits) <|> ([] <$ ok))

-- | Parses number with maybe starting with +/- sign with spaces in the begining.
numberParser
  :: Parser Char Int -- | Resulting parser.
numberParser = do
               sign   <- id <$ element '+' <|> negate <$ element '-' <|> id <$ ok
               numberDigits <- getDigits
               guard (not $ null numberDigits)
               let number = read numberDigits :: Int
               return (sign number)
