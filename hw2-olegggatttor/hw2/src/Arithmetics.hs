module Arithmetics
  ( Operation(..)
  , Expr(..)
  , ArithmeticError(..)
  , eval
  ) where

-- | Math operations type.
data Operation
  = Sum
  | Sub
  | Mul
  | Div
  | Pow
  deriving Show

-- | Expression type.
data Expr
  = Const Int
  | BinaryOperation Operation Expr Expr
  deriving Show

-- | Arithmetic errors type.
data ArithmeticError
  = DivisionByZero
  | NegativePow
  deriving (Show, Eq)

-- | Evaluates given Expr.
eval
  :: Expr                       -- ^ Given Expr.
  -> Either ArithmeticError Int -- ^ Error if was DivisionByZero or NegativePow else Int boxed in Either.
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
