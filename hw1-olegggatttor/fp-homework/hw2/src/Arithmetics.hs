module Arithmetics where

data Operation
  = Sum
  | Sub
  | Mul
  | Div
  | Pow
  deriving Show

data Expr
  = Const Int
  | BinaryOperation Operation Expr Expr
  deriving Show

data ArithmeticError
  = DivisionByZero
  | NegativePow
  deriving (Show, Eq)

eval
  :: Expr
  -> Either ArithmeticError Int
eval (Const x) = return x
eval (BinaryOperation op l r) = do
                                  leftRes <- eval l
                                  rightRes <- eval r
                                  case op of
                                    Sum -> return (leftRes + rightRes)
                                    Sub -> return (leftRes - rightRes)
                                    Mul -> return (leftRes * rightRes)
                                    Div -> if rightRes == 0
                                           then Left DivisionByZero
                                           else return (leftRes `div` rightRes)
                                    Pow -> if rightRes < 0
                                           then Left NegativePow
                                           else return (leftRes ^ rightRes)
