module ThirdBlockSpec.Task1Spec where

import Data.Monoid
import Test.Hspec
import ThirdBlock.Task1

specConcat
  :: Spec
specConcat = do
  describe "maybeConcat" $ do
    it "returns a concatenation of all the inner lists in list of Maybe" $ do
      maybeConcat [Just [1, 2, 3], Nothing, Just [4, 5]] `shouldBe` [1, 2, 3, 4, 5]
      maybeConcat [Nothing, Just [25], Nothing]          `shouldBe` [25]
      maybeConcat [Just [1], Just [1], Just [1]]         `shouldBe` [1, 1, 1]
  describe "eitherConcat" $ do
    it "returns pair of concatenations of two monoids in Either list" $ do
      eitherConcat [Left (Sum 3), Right [1, 2, 3], Left (Sum 3), Right [4, 5]]
                          `shouldBe` (Sum 6, [1, 2, 3, 4, 5])
      eitherConcat [Left (Product 2), Right (First (Just "me")), Right (First (Just "!me")),
       Left (Product 10)] `shouldBe` (Product 20, First (Just "me"))
      eitherConcat [Left (Sum 1), Right (Just [1, 2, 3]), Left (Sum 2), Right Nothing]
                          `shouldBe` (Sum 3, Just [1, 2, 3])
