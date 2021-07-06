module NotSimpleParserSpec where

import CopyPaste
import NotSimpleParser
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Tests for NotSimpleParser module.
specNotSimpleParser
  :: Spec
specNotSimpleParser = do
  describe "ws parser" $ do
    it "Skips whitespaces in the begining of input" $ do
      runParser ws "    abc"    `shouldBe` Just ((), "abc")
      runParser ws "abc   "     `shouldBe` Just ((), "abc   ")
      runParser ws "x   abc"    `shouldBe` Just ((), "x   abc")
  describe "listParser" $ do
    it "Parses list of ints. First elemnt is size of list" $ do
      runParser listParser "1, 0"                  `shouldBe` Just ([0], "")
      runParser listParser "3, 1, 2, 3"            `shouldBe` Just ([1, 2, 3], "")
      runParser listParser "  3,    1, 2,    3   " `shouldBe` Just ([1, 2, 3], "")
      runParser listParser "2, 1, -2, 3"           `shouldBe` Just ([1, (-2)], ", 3")
      runParser listParser "2, 1. 2, 3"            `shouldBe` Nothing
      runParser listParser "2, 1a, 2, 3"           `shouldBe` Nothing
  describe "listListParser" $ do
    it "Parses list if list of ints from prev test" $ do
      runParser listListParser "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
      runParser listListParser "2, 1, 0, 0"            `shouldBe` Just ([[1, 0], []], "")
      runParser listListParser "-1, 2, 1, 0"           `shouldBe` Nothing
      runParser listListParser ""                      `shouldBe` Nothing
