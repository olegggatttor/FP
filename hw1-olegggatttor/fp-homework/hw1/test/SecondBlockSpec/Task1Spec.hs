module SecondBlockSpec.Task1Spec where

import Data.Char
import Data.Foldable hiding (find)
import Data.List
import Data.List.NonEmpty hiding (fromList, sort, toList)
import FirstBlock.Task3
import Test.Hspec
import Test.Hspec.QuickCheck

specTreeFoldable
  :: Spec
specTreeFoldable = do
  describe "foldr" $ do
    it "foldrs given tree" $ do
      foldr (+)                          5  (fromList [1, 2, 3, 4, 5])  `shouldBe` 20
      foldr (*)                          0  (fromList [1, 2, 3, 4, 5])  `shouldBe` 0
      foldr (\x res -> x ++ ":)" ++ res) [] (fromList ["a", "c", "x", "d", "1"])
                                                                        `shouldBe` "1:)a:)c:)d:)x:)"
      foldr (\x res -> toUpper x : res)  [] (fromList ['d', 'c', 'a', 'r', 'o'])
                                                                        `shouldBe` "ACDOR"
  describe "foldMap" $ do
    it "foldMaps given tree" $ do
      foldMap (\(x:_) -> toUpper x : "") (fromList ["heal", "pounce", "poor", "young", "image"])
                                                                                  `shouldBe` "HIPPY"
  describe "toList" $ do
    prop "property that toList . fromList = sort (Int example)" $ \list ->
      toList  (fromList (list :: [Int]))    `shouldBe` sort (list :: [Int])
    prop "property that toList . fromList = sort (String example)" $ \list ->
      toList  (fromList (list :: [String])) `shouldBe` sort (list :: [String])
