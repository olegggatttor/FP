module ArithmeticsSpec where

import Test.Hspec
import Arithmetics

specArithmetics
  :: Spec
specArithmetics = do
  describe "eval" $ do
    it "evaluates given correct expression and returns the result" $ do
      eval (BinaryOperation Sum (Const 2) (Const 2))                        `shouldBe` Right 4
      eval (BinaryOperation Mul (Const 2) (Const 2))                        `shouldBe` Right 4
      eval (BinaryOperation Div (Const 2) (Const 2))                        `shouldBe` Right 1
      eval (BinaryOperation Sub (Const 5) (Const 2))                        `shouldBe` Right 3
      eval (BinaryOperation Pow (Const 5) (Const 3))                        `shouldBe` Right 125
      eval (BinaryOperation Sum (BinaryOperation Pow (Const 5) (Const 2))
                                (BinaryOperation Div (Const 10) (Const 5))) `shouldBe` Right 27
      eval (BinaryOperation Sub (Const 10) (BinaryOperation Pow (Const 2)
           (BinaryOperation Sum (Const 2) (Const 2))))                      `shouldBe` Right (-6)
    it "evaluates given incorrect expression and returns the exception" $ do
      eval (BinaryOperation Div (Const 2) (Const 0))            `shouldBe` Left DivisionByZero
      eval (BinaryOperation Sum (BinaryOperation Div (Const 5) (Const 0))
                                                     (Const 2)) `shouldBe` Left DivisionByZero
      eval (BinaryOperation Pow (Const 10) (Const -2))          `shouldBe` Left NegativePow
      eval (BinaryOperation Sub (Const 10) (BinaryOperation Pow (Const 2)
           (BinaryOperation Sum (Const 2) (Const (-2)))))       `shouldBe` Left NegativePow
