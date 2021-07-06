module CopyPasteSpec
  ( specCopyPaste
  ) where

import Control.Applicative
import CopyPaste
import Data.Char (isAlpha, toLower, toUpper)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Parser for testing that consumes one lement of input.
simpleParser
  :: Eq a       -- ^ Elements of input must be instance of Eq.
  => Parser a a -- ^ Resulting parser.
simpleParser = Parser $ \input -> case input of
                                    []     -> Nothing
                                    (x:xs) -> Just (x, xs)

-- | Tests for CopyPaste module.
specCopyPaste
  :: Spec
specCopyPaste = do
  describe "Functor instance for Parser" $ do
    it "Law: fmap id = id" $ do
      runParser (fmap id simpleParser) "abc"         `shouldBe` runParser (id simpleParser) "abc"
      runParser (fmap id simpleParser) "xyz"         `shouldBe` runParser (id simpleParser) "xyz"
      runParser (fmap id simpleParser) [1, 2, 3]     `shouldBe` runParser (id simpleParser) [1, 2, 3]
      runParser (fmap id simpleParser) ([] :: [Int]) `shouldBe` runParser (id simpleParser) []
    it "Law: fmap (f . g) == fmap f . fmap g" $ do
      runParser (fmap (toUpper . toLower) simpleParser) "abc"   `shouldBe`
                                     runParser (fmap toUpper . fmap toLower $  simpleParser) "abc"
      runParser (fmap ((&& True) . isAlpha) simpleParser) "xyz" `shouldBe`
                                     runParser (fmap (&& True) . fmap isAlpha $  simpleParser) "xyz"
      runParser (fmap ((+1) . (* 2)) simpleParser) [1, 2, 3]    `shouldBe`
                                     runParser (fmap (+ 1) . fmap (* 2) $ simpleParser) [1, 2, 3]
  describe "Applicative instance for Parser" $ do
    it "Law: pure id <*> v = v" $ do
      runParser (pure id <*> simpleParser) "abc"         `shouldBe` runParser simpleParser "abc"
      runParser (pure id <*> simpleParser) "xyz"         `shouldBe` runParser simpleParser "xyz"
      runParser (pure id <*> simpleParser) [1, 2, 3]     `shouldBe` runParser simpleParser [1, 2, 3]
      runParser (pure id <*> simpleParser) ([] :: [Int]) `shouldBe` runParser simpleParser []
    it "Law: pure f <*> pure x = pure (f x)" $ do
      runParser (pure (&& True) <*> pure True) [True] `shouldBe` runParser (pure (True && True)) [True]
      runParser (pure (+ 1) <*> pure 2) [3]           `shouldBe` runParser (pure (1 + 2)) [3]
    it "Law: u <*> pure y = pure ($ y) <*> u" $ do
      runParser ((fmap (\x y-> x + y) simpleParser) <*> pure 5) [1, 2] `shouldBe`
                                 runParser (pure ($ 5) <*> (fmap (\x y-> x + y) simpleParser)) [1, 2]
    it "Law: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
      runParser (pure (.) <*> pure (+ 1) <*> pure (* 2)  <*> pure 1) [1, 2, 3] `shouldBe`
         runParser (pure (+ 1) <*> (pure (* 2)  <*> pure 1)) [1, 2, 3]
  describe "Monad instance for Parser" $ do
    it "Law: return a >>= f = f a" $ do
      runParser (return 5 >>= \x -> return (x + 1)) [1, 2] `shouldBe` runParser (return 6) [1, 2]
      runParser (return 7 >>= \x -> return (x * 2)) [1, 2] `shouldBe` runParser (return 14) [1, 2]
      runParser (return 'a' >>= \x -> return $ toUpper x) "abc" `shouldBe`
                                                                      runParser (return 'A') "abc"
    it "Law: m >>= return = m" $ do
      runParser (simpleParser >>= return) [1, 2] `shouldBe` runParser simpleParser [1, 2]
      runParser (return 5 >>= return) [1, 2] `shouldBe` runParser (return 5) [1, 2]
    it "Law: (m >>= f) >>= g = m >>= (\\x -> f x >>= g)" $ do
      runParser (simpleParser >>= \x -> return (x + 1) >>= \y -> return (y * 2)) [1, 2] `shouldBe`
       runParser (simpleParser >>= \z -> (\x -> return (x + 1)) z >>= \y -> return (y * 2)) [1, 2]
  describe "Alternative laws for Parser" $ do
    it "empty <|> x = x" $ do
      runParser (empty <|> simpleParser) "abc" `shouldBe` runParser simpleParser "abc"
      runParser (simpleParser <|> empty) "abc" `shouldBe` runParser simpleParser "abc"
    it "u <|> (v <|> w) = (u <|> v) <|> w" $ do
      runParser ((Parser $ \_ -> Nothing) <|> (simpleParser <|> return 'y')) "abc" `shouldBe`
        runParser (((Parser $ \_ -> Nothing) <|> simpleParser) <|> return 'y') "abc"
