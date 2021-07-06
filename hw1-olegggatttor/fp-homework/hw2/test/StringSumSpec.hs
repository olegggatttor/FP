module StringSumSpec where

import StringSum
import Test.Hspec

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
