module BaseCombinatorsSpec where

import BaseCombinators
import CopyPaste
import Data.Char (isAlpha, isDigit)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Tests for BaseCombinators.
specBaseCombinators
  :: Spec
specBaseCombinators = do
  describe "ok parser" $ do
    it "Never fails and does not absorb input" $ do
      runParser ok "abc"     `shouldBe` Just ((), "abc")
      runParser ok "xyzsjda" `shouldBe` Just ((), "xyzsjda")
      runParser ok [1, 2, 3] `shouldBe` Just ((), [1, 2, 3])
  describe "eof parser" $ do
    it "Fails on not empty input" $ do
      runParser eof ""        `shouldBe` Just ((), "")
      runParser eof "abc"     `shouldBe` Nothing
      runParser eof "xxx"     `shouldBe` Nothing
      runParser eof [1, 2, 3] `shouldBe` Nothing
  describe "satisfy parser" $ do
    it "Absorbs element if it satisfies predicate and otherwise fails" $ do
      runParser (satisfy isDigit) "1ab"    `shouldBe` Just ('1', "ab")
      runParser (satisfy isAlpha) "x1ab"   `shouldBe` Just ('x', "1ab")
      runParser (satisfy (== 5)) [5, 1, 2] `shouldBe` Just (5, [1, 2])
      runParser (satisfy isAlpha) "1ab"    `shouldBe` Nothing
  describe "element parser" $ do
    it "Absorbs one element if it is equal to first element of input otherwise fails" $ do
      runParser (element 'a') "abc"   `shouldBe` Just ('a', "bc")
      runParser (element 1) [1, 2, 3] `shouldBe` Just (1, [2, 3])
      runParser (element 1) [2, 3]    `shouldBe` Nothing
  describe "stream parser" $ do
    it "Absorbs stream of elements and if does not match fails" $ do
      runParser (stream "abc") "abcdef"            `shouldBe` Just ("abc", "def")
      runParser (stream "1234") "1234abcdef"       `shouldBe` Just ("1234", "abcdef")
      runParser (stream [1, 2, 5]) [1, 2, 3, 4, 5] `shouldBe` Nothing
