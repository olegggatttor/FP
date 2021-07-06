module BaseCombinators where

import CopyPaste

ok
  :: Parser s ()
ok = Parser $ \input -> Just ((), input)

eof
  :: Parser s ()
eof = Parser $ \input -> case input of
                           [] -> Just ((), [])
                           (_:_) -> Nothing

satisfy
  :: (s -> Bool)
  -> Parser s s
satisfy f = Parser $ \input -> case input of
                                 []     -> Nothing
                                 (x:xs) -> case f x of
                                             True -> Just (x, xs)
                                             False -> Nothing

element
  :: Eq s
  => s
  -> Parser s s
element e = satisfy (== e)

stream
  :: Eq s
  => [s]
  -> Parser s [s]
stream [] = [] <$ ok
stream (e:es) = Parser $ \input -> case input of
                                     [] -> Nothing
                                     list -> runParser ((element e) >> (fmap (e:) (stream es))) $ list
