module FirstBlock.Task2
  ( Nat(..)
  , add
  , mul
  , sub
  , fromNat
  , toNat
  , isEven
  , divide
  , modulo
  ) where

import Prelude hiding (Integer)

-- | Natural numbers type.
data Nat
  = Z
  | S Nat
  deriving Show

instance Eq Nat where
  (==) Z     Z     = True
  (==) (S a) (S b) = a == b
  (==) _     _     = False

instance Ord Nat where
  (<=) Z     _     = True
  (<=) _     Z     = False
  (<=) (S a) (S b) = a <= b

instance Enum Nat where
  toEnum 0 = Z
  toEnum a
    | a > 0 = S $ toNat (a - 1)
    | otherwise = error "number must be positive or zero"

  fromEnum Z     = 0
  fromEnum (S a) = 1 + fromEnum a

-- | Returns Natural value of sum of two Natural numbers.
add
  :: Nat -- ^ first  natural number
  -> Nat -- ^ second natural number
  -> Nat -- ^ sum of two given numbers
add a Z     = a
add a (S b) = S $ a `add` b

-- | Returns Natural value of product of two Natural numbers.
mul
  :: Nat -- ^ first  natural number
  -> Nat -- ^ first  natural number
  -> Nat -- ^ product of two natural number
mul _ Z     = Z
mul Z _     = Z
mul a (S b) = a `add` (a `mul` b)

-- | Returns subtraction of a and b if a greater than b and 0 otherwise.
sub
  :: Nat -- ^ first  natural number
  -> Nat -- ^ second  natural number
  -> Nat -- ^ the result of subtraction of two given numbers
sub a     Z     = a
sub Z     _     = Z
sub (S a) (S b) = sub a b

-- | Converts given Natural number to Int value.
fromNat
  :: Nat -- ^ given Natural value
  -> Int -- ^ Int output value
fromNat = fromEnum

-- | Converts given Int value to Natural value if it is positive or zero.
-- and -value if it is negative
toNat
  :: Int -- ^ given Int value
  -> Nat -- ^ natural representation of given Int number
toNat = toEnum

-- | Returns True if given natural value is even and False otherwise.
isEven
  :: Nat  -- ^ given natural value
  -> Bool -- ^ True if given natural value is even and False otherwise
isEven Z         = True
isEven (S Z)     = False
isEven (S (S a)) = isEven a

-- | Integer division of natural numbers or error if divisor is zero.
divide
  :: Nat -- ^ given dividend
  -> Nat -- ^ given divisor
  -> Nat -- ^ the result of division
divide _ Z    = error "division by zero"
divide Z _    = Z
divide a b
  | a < b     = Z
  | otherwise = S $ divide (sub a b) b

-- | Remainder of dividing a natural number by another.
modulo
  :: Nat -- ^ given dividend
  -> Nat -- ^ given divisor
  -> Nat -- ^ remainder of division
modulo _ Z = error "division by zero"
modulo Z _ = Z
modulo a b = sub a (mul (divide a b) b)
