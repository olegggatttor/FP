{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module HalyavaToJS
  ( ToJS(..)
  ) where

import HalyavaScript (Halyava(..), HalyavaVar(..))
import Data.Functor.Const (Const)

newtype ToJS a = ToJS { toJS :: Int -> String -> String }
  deriving (Semigroup)

-- | Converts operation to JS representation
showOp
  :: (HalyavaVar a, HalyavaVar b)
  => String -- ^ Operation sign
  -> ToJS a -- ^ First argument
  -> ToJS a -- ^ Second argument
  -> ToJS b -- ^ Resulting boxed value
showOp op a b = ToJS $ \n tab -> "(" <>  toJS a n tab <> " " <> op <> " " <> toJS b n tab <> ")"

instance Halyava ToJS where
  type Ref ToJS = Const String

  scope ini block = ToJS $ \n tab -> tab <> "{\n" <> tab
                                     <> "var " <> newVar n <> " = " <> toString ini <> ";\n" <>
                                     toJS (block (ToJS $ \_ _ -> newVar n)) (n + 1) (tab <> "\t")
                                     <> tab <> "}\n"
    where
      newVar
        :: Int
        -> String
      newVar n = "v" <> show n

  ref @= value = ToJS $ \n tab -> tab <> toJS ref n tab <> " = " <> toJS value n tab <> ";"

  block1 ! block2 = ToJS $ \n tab -> toJS block1 n tab <> "\n" <> toJS block2 n tab

  new ini = ToJS $ \_ _ -> toString ini

  unbox ref = ToJS $ \n tab -> toJS ref n tab

  ret ref = ToJS $ \n tab -> tab <> "return " <> toJS ref n tab <> ";\n"

  while cond block = ToJS $ \n tab -> tab <> "while (" <> toJS cond n tab <> ") {\n"
                                      <> toJS block n (tab <> "\t") <> tab <> "}\n"

  when cond block1 block2 = ToJS $ \n tab -> tab <> "if(" <> toJS cond n tab <> ") {\n"
                                          <> toJS block1 n (tab <> "\t") <> "\n" <> tab
                                          <> "} else {\n"
                                          <> toJS block2 n (tab <> "\t") <> "\n" <> tab <> "}\n"

  (@==) = showOp "=="

  (@!=) = showOp "!="

  (@>) = showOp ">"

  (@>=) = showOp ">="

  (@<=) = showOp "<="

  (@<) = showOp "<"

  (@&&) = showOp "&&"

  (@||) = showOp "||"

  not' x = ToJS $ \n tab -> "!(" <> toJS x n tab <> ")"

  (@++) = showOp "+"

  (@+) = showOp "+"

  (@-) = showOp "-"

  (@*) = showOp "*"

  (@/) = showOp "/"

  (@%) = showOp "%"
