{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HalyavaScript
  ( Script(..)
  , Halyava(..)
  , HalyavaVar(..)
  ) where

import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST(..))

class (Show a, Ord a) => HalyavaVar a where
  toString
    :: a
    -> String
  toString = show

instance HalyavaVar Int

instance HalyavaVar String

instance HalyavaVar Bool where
  toString True = "true"
  toString False = "false"

class Halyava expr where
  type Ref expr :: * -> *

  infix 2 @=
  (@=)  :: HalyavaVar a => expr (Ref expr a) -> expr a -> expr ()

  infixl 1 !
  (!)   :: expr a -> expr b -> expr b

  infix 4 @>
  (@>)  :: (Ord a, HalyavaVar a) => expr a -> expr a -> expr Bool

  infix 4 @>=
  (@>=) :: (Ord a, HalyavaVar a) => expr a -> expr a -> expr Bool

  infix 4 @<
  (@<)  :: (Ord a, HalyavaVar a) => expr a -> expr a -> expr Bool

  infix 4 @<=
  (@<=) :: (Ord a, HalyavaVar a) => expr a -> expr a -> expr Bool

  infix 4 @==
  (@==) :: (Ord a, HalyavaVar a) => expr a -> expr a -> expr Bool

  infix 4 @!=
  (@!=) :: (Ord a, HalyavaVar a) => expr a -> expr a -> expr Bool

  infixr 3 @&&
  (@&&) :: expr Bool -> expr Bool -> expr Bool

  infixr 3 @||
  (@||) :: expr Bool -> expr Bool -> expr Bool

  not'  :: expr Bool -> expr Bool

  infixr 5 @++
  (@++) :: expr String -> expr String -> expr String

  infixl 6 @+
  (@+) :: (Num a, HalyavaVar a) => expr a -> expr a -> expr a

  infixl 6 @-
  (@-) :: (Num a, HalyavaVar a) => expr a -> expr a -> expr a

  infixl 7 @*
  (@*) :: (Num a, HalyavaVar a) => expr a -> expr a -> expr a

  infixl 7 @/
  (@/) :: (Integral a, HalyavaVar a) => expr a -> expr a -> expr a

  infixl 7 @%
  (@%) :: (Integral a, HalyavaVar a) => expr a -> expr a -> expr a

  while :: expr Bool -> expr () -> expr ()
  when  :: expr Bool -> expr () -> expr () -> expr ()

  scope :: (HalyavaVar a, HalyavaVar b) => a -> (expr (Ref expr a) -> expr b) -> expr b

  new     :: HalyavaVar a => a -> expr a
  unbox   :: expr (Ref expr a) -> expr a
  ret     :: expr (Ref expr a) -> expr a
  ret = unbox

-- | Script type
newtype Script s a = Script { runScript :: ST s a}
  deriving (Functor, Applicative, Monad)

-- | Invokes binary operation on two Scripts
invokeOp
  :: (a -> a -> b) -- ^ operation
  -> Script s a    -- ^ First value
  -> Script s a    -- ^ Second value
  -> Script s b    -- ^ Resulting value
invokeOp f (Script value1) (Script value2) = Script $ do
  x <- value1
  y <- value2
  return (x `f` y)

instance Halyava (Script s) where
  type Ref (Script s) = STRef s

  (@=) (Script refST) (Script newValueST) = Script $ do
    ref      <- refST
    newValue <- newValueST
    writeSTRef ref newValue

  (!) (Script line1) (Script line2) = Script $ line1 >> line2

  (@==) = invokeOp (==)

  (@!=) = invokeOp (/=)

  (@>) = invokeOp (>)

  (@>=) = invokeOp (>=)

  (@<=) = invokeOp (<=)

  (@<) = invokeOp (<)

  (@&&) = invokeOp (&&)

  (@||) = invokeOp (||)

  not' = invokeOp (\_ y -> not y) (new False)

  (@++) = invokeOp (++)

  (@+) = invokeOp (+)

  (@-) = invokeOp (-)

  (@*) = invokeOp (*)

  (@/) = invokeOp (div)

  (@%) = invokeOp (mod)

  while (Script cond) (Script block) = Script $ whileST
    where
      whileST = do
        statement <- cond
        if statement
        then do
          block
          whileST
        else return ()

  when (Script cond) (Script block1) (Script block2) = Script $ do
    statement <- cond
    if statement
    then block1
    else block2

  scope ini block = do
    ref <- Script $ newSTRef $ ini
    block (return ref)


  new = Script . return

  unbox (Script value) = Script $ value >>= readSTRef

-- ^ Example ith 3 types is example1 in test/Spec.hs
