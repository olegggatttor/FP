module StringSumSpec where

import Data.Char             (isDigit)
import StringSum
import Test.Hspec            (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck       (Gen, elements, forAll, listOf1)

-- | Custom chars generator
genSymbols
  :: Gen Char -- ^ returns Char generator
genSymbols = elements ('!' : '@' : '$' : '(' : ')' : '?' : '>' : ['a'..'z'])

-- | Custom string generator
genString
  :: Gen String -- ^ returns String generator
genString = listOf1 genSymbols

-- | Tests for StringSum module.
specStringSum
  :: Spec
specStringSum = do
  describe "stringSum" $ do
    it "returns sum of digits separated by blank characters contained in Just" $ do
      stringSum "1"                        `shouldBe` Just 1
      stringSum "1\n2\n3\n4"                        `shouldBe` Just (1 + 2 + 3 + 4)
      stringSum "1 2 3 4 5"                `shouldBe` Just (1 + 2 + 3 + 4 + 5)
      stringSum "1      2\n\t\t\t 3 4\t 5" `shouldBe` Just (1 + 2 + 3 + 4 + 5)
      stringSum ""                         `shouldBe` Just 0
    it "returns Nothing if there were something except blanks and numbers"     $ do
      stringSum "1 a"           `shouldBe` Nothing
      stringSum "abc def"       `shouldBe` Nothing
      stringSum "1 2 3 4 a 5"   `shouldBe` Nothing
      stringSum "1, 2, 3, 4, 5" `shouldBe` Nothing
      stringSum "1.5 2.3 3.5"   `shouldBe` Nothing
  describe "stringSum property tests" $ do
    prop "valid input" $ \list ->
      Just (sum list) `shouldBe` (stringSum $ concatMap (\x -> " \n\t" ++ show x) (list :: [Int]))
    prop "invalid input" $ \list -> forAll genString $ \str ->
      Nothing         `shouldBe`
        (stringSum $ concatMap (\x -> str ++ " \n\t" ++ show x) (0 : (list :: [Int])))
