module BaseCombinators
  ( ok
  , eof
  , satisfy
  , element
  , stream
  ) where

import CopyPaste

-- | Parser that does not absorb input and never fails.
ok
  :: Parser s () -- ^ Return parser.
ok = pure ()

-- | Parser that does not fails only on empty input.
eof
  :: Parser s () -- ^ Return parser.
eof = Parser $ \input -> case input of
                           []    -> Just ((), [])
                           (_:_) -> Nothing

-- | Parser that consume one element of input if it matches predicate and otherwise fails.
satisfy
  :: (s -> Bool) -- ^ Given predicate.
  -> Parser s s  -- ^ Resulting parser.
satisfy f = Parser $ \input -> case input of
                                 []     -> Nothing
                                 (x:xs) -> case f x of
                                             True  -> Just (x, xs)
                                             False -> Nothing

-- | Parser one element from the begining of input if they are equal and otherwise fails.
element
  :: Eq s       -- ^ s must be instance of Eq.
  => s          -- ^ Given element to match.
  -> Parser s s -- ^ Resulting parser.
element e = satisfy (== e)

-- | Parses stream of elements if they are begining of the input and otherwise fails.
stream
  :: Eq s         -- ^ s must be instance of Eq.
  => [s]          -- ^ Given stream of elements.
  -> Parser s [s] -- ^ Resulting Parser.
stream [] = [] <$ ok
stream (e:es) = Parser $ \input -> case input of
                                     [] -> Nothing
                                     list -> runParser ((element e) >> (fmap (e:) (stream es))) $ list
