module Main where

import ArithmeticsSpec
import BaseCombinatorsSpec
import CopyPasteSpec
import NotSimpleParserSpec
import SimpleParsersSpec
import StringSumSpec
import Test.Hspec (hspec)

-- | Main function for testing.
main
  :: IO ()
main = hspec $ do
         specStringSum
         specArithmetics
         specCopyPaste
         specSimpleParsers
         specBaseCombinators
         specSimpleParsers
         specNotSimpleParser
