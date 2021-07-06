module FirstBlockSpec.Task2Spec where

import FirstBlock.Task2
import Test.Hspec
import Test.Hspec.QuickCheck

-- | Tests fro data type Nat.
specNat
  :: Spec
specNat = do
  describe "toNat" $ do
    it "transform Int to Nat if Int is positive or zero" $ do
      toNat 0  `shouldBe` Z
      toNat 1  `shouldBe` S Z
      toNat 2  `shouldBe` S (S Z)
      toNat 3  `shouldBe` S (S (S Z))
      toNat 4  `shouldBe` S (S (S (S Z)))
      toNat 5  `shouldBe` S (S (S (S (S Z))))
      toNat 6  `shouldBe` S (S (S (S (S (S Z)))))
      toNat 7  `shouldBe` S (S (S (S (S (S (S Z))))))
      toNat 8  `shouldBe` S (S (S (S (S (S (S (S Z)))))))
      toNat 9  `shouldBe` S (S (S (S (S (S (S (S (S Z))))))))
      toNat 10 `shouldBe` S (S (S (S (S (S (S (S (S (S Z)))))))))
  describe "fromNat" $ do
    it "transform Nat to Int" $ do
      fromNat Z                                          `shouldBe` 0
      fromNat (S Z)                                      `shouldBe` 1
      fromNat (S (S Z))                                  `shouldBe` 2
      fromNat (S (S (S Z)))                              `shouldBe` 3
      fromNat (S (S (S (S Z))))                          `shouldBe` 4
      fromNat (S (S (S (S (S Z)))))                      `shouldBe` 5
      fromNat (S (S (S (S (S (S Z))))))                  `shouldBe` 6
      fromNat (S (S (S (S (S (S (S Z)))))))              `shouldBe` 7
      fromNat (S (S (S (S (S (S (S (S Z))))))))          `shouldBe` 8
      fromNat (S (S (S (S (S (S (S (S (S Z)))))))))      `shouldBe` 9
      fromNat (S (S (S (S (S (S (S (S (S (S Z))))))))))  `shouldBe` 10
  describe "add" $ do
    it "returns sum of two Natural values" $ do
      add (toNat 0)  (toNat 0) `shouldBe` toNat 0
      add (toNat 0)  (toNat 1) `shouldBe` toNat 1
      add (toNat 1)  (toNat 0) `shouldBe` toNat 1
      add (toNat 1)  (toNat 1) `shouldBe` toNat 2
      add (toNat 2)  (toNat 2) `shouldBe` toNat 4
      add (toNat 13) (toNat 7) `shouldBe` toNat 20
    prop "Nat x + Nat y = Nat (x + y)" $ \num1 num2 ->
      add (toNat (abs num1 :: Int)) (toNat (abs num2 :: Int)) `shouldBe` toNat (abs num1 + abs num2)
  describe "mul" $ do
    it "returns product of two Natural values" $ do
      mul (toNat 0)  (toNat 0)  `shouldBe` toNat 0
      mul (toNat 0)  (toNat 1)  `shouldBe` toNat 0
      mul (toNat 1)  (toNat 0)  `shouldBe` toNat 0
      mul (toNat 1)  (toNat 1)  `shouldBe` toNat 1
      mul (toNat 1)  (toNat 5)  `shouldBe` toNat 5
      mul (toNat 5)  (toNat 1)  `shouldBe` toNat 5
      mul (toNat 4)  (toNat 2)  `shouldBe` toNat 8
      mul (toNat 2)  (toNat 4)  `shouldBe` toNat 8
      mul (toNat 6)  (toNat 6)  `shouldBe` toNat 36
      mul (toNat 3)  (toNat 10) `shouldBe` toNat 30
    prop "Nat x * Nat y = Nat (x * y)" $ \num1 num2 ->
      mul (toNat (abs num1 :: Int)) (toNat (abs num2 :: Int)) `shouldBe` toNat (abs num1 * abs num2)
  describe "sub" $ do
    it "returns a - b if a > b and 0 otherwise in Natural values" $ do
      sub (toNat 0)   (toNat 0) `shouldBe` toNat 0
      sub (toNat 0)   (toNat 5) `shouldBe` toNat 0
      sub (toNat 5)   (toNat 0) `shouldBe` toNat 5
      sub (toNat 5)   (toNat 3) `shouldBe` toNat 2
      sub (toNat 3)   (toNat 5) `shouldBe` toNat 0
      sub (toNat 9)   (toNat 2) `shouldBe` toNat 7
      sub (toNat 20)  (toNat 9) `shouldBe` toNat 11
    prop "Nat x - Nat y = Nat (x - y) if x >= y else 0" $ \num1 num2 ->
      if ((abs num1 :: Int) >= (abs num2 :: Int))
      then sub (toNat (abs num1)) (toNat (abs num2)) `shouldBe` toNat (abs num1 - abs num2)
      else sub (toNat (abs num1)) (toNat (abs num2)) `shouldBe` toNat 0
  describe "isEven" $ do
    it "returns True if given Natural value is even and false otherwis" $ do
      isEven (toNat 0)  `shouldBe` True
      isEven (toNat 1)  `shouldBe` False
      isEven (toNat 2)  `shouldBe` True
      isEven (toNat 3)  `shouldBe` False
      isEven (toNat 4)  `shouldBe` True
      isEven (toNat 5)  `shouldBe` False
      isEven (toNat 6)  `shouldBe` True
      isEven (toNat 7)  `shouldBe` False
      isEven (toNat 8)  `shouldBe` True
      isEven (toNat 9)  `shouldBe` False
      isEven (toNat 10) `shouldBe` True
    prop "Check if number is even" $ \num ->
      isEven (toNat (abs num :: Int)) `shouldBe` (abs num) `mod` 2 == 0
  describe "divide" $ do
    it "integer division of natural numbers" $ do
      divide (toNat 0)   (toNat 1) `shouldBe` toNat 0
      divide (toNat 1)   (toNat 1) `shouldBe` toNat 1
      divide (toNat 5)   (toNat 2) `shouldBe` toNat 2
      divide (toNat 10)  (toNat 3) `shouldBe` toNat 3
      divide (toNat 7)   (toNat 5) `shouldBe` toNat 1
      divide (toNat 3)   (toNat 1) `shouldBe` toNat 3
    prop "property test integer division" $ \divider divisor ->
      if (divisor :: Int) == 0
      then 1 `shouldBe` 1
      else divide (toNat (abs divider :: Int)) (toNat (abs divisor)) `shouldBe`
                                                               toNat (abs divider `div` abs divisor)
  describe "modulo" $ do
    it "remainder of dividing a natural number by another" $ do
      modulo (toNat 0)   (toNat 1) `shouldBe` toNat 0
      modulo (toNat 1)   (toNat 1) `shouldBe` toNat 0
      modulo (toNat 5)   (toNat 2) `shouldBe` toNat 1
      modulo (toNat 10)  (toNat 3) `shouldBe` toNat 1
      modulo (toNat 7)   (toNat 5) `shouldBe` toNat 2
      modulo (toNat 3)   (toNat 1) `shouldBe` toNat 0
    prop "property test remainder of division" $ \divider divisor ->
      if (divisor :: Int) == 0
      then 1 `shouldBe` 1
      else modulo (toNat (abs divider :: Int)) (toNat (abs divisor)) `shouldBe`
                                                               toNat (abs divider `mod` abs divisor)
