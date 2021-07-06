module CopyPaste where

import Data.Tuple (swap)
import Control.Applicative

data Parser s a = Parser {runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f parser = let
    first :: (a -> b) -> (a, c) -> (b, c)
    first func pair = swap . fmap func $ swap pair
    in Parser $ \input -> fmap (first f) $ runParser parser $ input

instance Applicative (Parser s) where
  pure x = Parser $ \input -> pure (x, input)

  (Parser fparser) <*> (Parser parser) = Parser $ \input -> do
                                                            (f, frest) <- fparser input
                                                            (a, rest) <- parser frest
                                                            return (f a, rest)

instance Monad (Parser s) where
  return = pure

  (Parser parser) >>= f = Parser $ \input -> do
                                             (parsed, rest) <- parser input
                                             let newParser = f parsed
                                             runParser newParser $ rest

instance Alternative (Parser s) where
  empty = Parser $ \_ -> Nothing

  (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
                                                     Just pair -> Just pair
                                                     Nothing   -> p2 input
