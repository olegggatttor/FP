module FirstBlockSpec.Task3Spec where

import           Data.Foldable    hiding (find)
import           FirstBlock.Task3
import           Test.Hspec

specTree
  :: Spec
specTree = do
  describe "isEmpty" $ do
    it "returns True if the tree is Empty and False otherwise" $ do
      isEmpty Leaf                 `shouldBe` True
      isEmpty (fromList [0])       `shouldBe` False
      isEmpty (fromList [0, 1, 2]) `shouldBe` False
  describe "count" $ do
    it "returns amount of elements in the tree" $ do
      count Leaf                             `shouldBe` 0
      count (fromList [0])                   `shouldBe` 1
      count (fromList [1, 2, 3])             `shouldBe` 3
      count (fromList [1, 2, 3, 4])          `shouldBe` 4
      count (fromList ["aaa", "bbb", "ccc"]) `shouldBe` 3
  describe "find" $ do
    it "returns True if element present in the tree and False otherwise" $ do
      find 0        Leaf                                              `shouldBe` False
      find "abc"    Leaf                                              `shouldBe` False
      find 5        (fromList [1, 2, 3])                              `shouldBe` False
      find 0        (fromList [1, 2, 3, 5, 0, 6])                     `shouldBe` True
      find "haskil" (fromList ["abc", "haskell", "money"])            `shouldBe` False
      find "A"      (fromList ["abc", "haskell", "money", "FX", "A"]) `shouldBe` True
  describe "insert" $ do
    it "inserts given element into the tree" $ do
     toList (insert 5 Leaf)                       `shouldBe` [5]
     toList (insert 6 (fromList [1, 2, 3, 5]))    `shouldBe` [1, 2, 3, 5, 6]
     toList (insert (-5) (fromList [1, 2, 3, 5])) `shouldBe` [-5, 1, 2, 3, 5]
     toList (insert 3 (fromList [1, 2, 3, 5]))    `shouldBe` [1, 2, 3, 3, 5]
     toList (insert 1 (fromList [1, 2, 3, 5]))    `shouldBe` [1, 1, 2, 3, 5]
  describe "fromList" $ do
    it "converts list to tree" $ do
      toList (fromList ([] :: [Int]))             `shouldBe` []
      toList (fromList [5, 4, 3, 1, 2])           `shouldBe` [1, 2, 3, 4, 5]
      toList (fromList ["a", "c", "x", "y", "b"]) `shouldBe` ["a", "b", "c", "x", "y"]
  describe "remove" $ do
    it "removes element from tree if there is one" $ do
      toList (remove 5 Leaf)                                   `shouldBe` []
      toList (remove 3 (fromList [1, 2, 3, 5]))                `shouldBe` [1, 2, 5]
      toList (remove 2 (fromList [5, 2, 2, 2, 3, 10, 1]))      `shouldBe` [1, 2, 2, 3, 5, 10]
      toList (remove 5 (fromList [5, 2, 2, 2, 3, 10, 1]))      `shouldBe` [1, 2, 2, 2, 3, 10]
      toList (remove "d" (fromList ["a", "c", "a", "d", "e"])) `shouldBe` ["a", "a", "c", "e"]
