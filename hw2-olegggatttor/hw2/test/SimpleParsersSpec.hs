module SimpleParsersSpec where

import BaseCombinators
import CopyPaste
import Data.Maybe (isJust)
import SimpleParsers
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Tests for SimpleParsers module.
specSimpleParsers
  :: Spec
specSimpleParsers = do
  describe "bracketParser" $ do
    it "Parses correct bracket sequence otherwise fails" $ do
      isJust (runParser bracketParser "")             `shouldBe` True
      isJust (runParser bracketParser "()")           `shouldBe` True
      isJust (runParser bracketParser "()((()()))()") `shouldBe` True
      isJust (runParser bracketParser "((()))(")      `shouldBe` False
      isJust (runParser bracketParser "()()()()(a)")  `shouldBe` False
      isJust (runParser bracketParser "{}(){}")       `shouldBe` False
  describe "numberParser" $ do
    it "Parses number with possible +/- sign in the beggining" $ do
      runParser numberParser "123"        `shouldBe` Just (123, "")
      runParser numberParser "+123"       `shouldBe` Just (123, "")
      runParser numberParser "-254"       `shouldBe` Just (-254, "")
      runParser numberParser "  -254"     `shouldBe` Nothing
      runParser numberParser "  -254 abc" `shouldBe` Nothing
      runParser numberParser "-1xbc"      `shouldBe` Just (-1, "xbc")
