module Main where

import Test.Hspec
import StringSumSpec
import ArithmeticsSpec
import CopyPasteSpec

main
  :: IO ()
main = hspec $ do
         specStringSum
         specArithmetics
         specCopyPaste
