module TreeInstancesSpec where

import TreeInstances
import Test.Hspec
import Data.Char

specTreeInstances
  :: Spec
specTreeInstances = do
  describe "Functor" $ do
    it "applies given function to the tree" $ do
      fmap (+ 1) (Leaf 5)                        `shouldBe` (Leaf 6)
      fmap (^ 2) (Branch (Leaf 5) (Leaf 10))     `shouldBe` (Branch (Leaf 25) (Leaf 100))
      fmap toUpper (Branch (Leaf 'a') (Leaf 'b')) `shouldBe` (Branch (Leaf 'A') (Leaf 'B'))
    it "fmap id ≣ id" $ do
      fmap id (Leaf 5)                       `shouldBe` id (Leaf 5)
      fmap id (Branch (Leaf 5) (Leaf 10))    `shouldBe` id (Branch (Leaf 5) (Leaf 10))
      fmap id (Branch (Leaf 'a') (Leaf 'b')) `shouldBe` id (Branch (Leaf 'a') (Leaf 'b'))
    it "fmap (f . g) ≣ (fmap f) . (fmap g)"
      fmap ((+ 1) . (* 2)) (Leaf 5)                 `shouldBe` fmap (+ 1) . fmap (* 2) (Leaf 5)
      fmap (id . (^ 2)) (Branch (Leaf 5) (Leaf 10)) `shouldBe`
                                               fmap id . fmap (^ 2) (Branch (Leaf 5) (Leaf 10))
  describe "Applicative" $ do
    it "applies tree of funcitons to the tree of values" $ do
      (Leaf (+ 5)) <*> (Leaf 10)                         `shouldBe` (Leaf 15)
      (Leaf (+ 5)) <*> (Branch (Leaf 5) (Leaf 10))       `shouldBe` (Branch (Leaf 10) (Leaf 15))
      (Branch (Leaf (+ 5)) (Leaf (+ 10))) <*> (Leaf 5)   `shouldBe` (Branch (Leaf 10) (Leaf 15))
  describe "Foldable" $ do
    it "applies tree of funcitons to the tree of values" $ do
      foldr (+)  0  (Branch (Leaf 1) (Leaf 2))                         `shouldBe` 3
      foldr (:)  [] (Branch (Leaf 'd') (Branch (Leaf 'o') (Leaf 'g'))) `shouldBe` "dog"
      foldr (++) [] (Branch (Leaf [1, 2]) (Leaf [3, 4]))               `shouldBe` [1, 2, 3, 4]
