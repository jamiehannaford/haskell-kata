module Parser where

import AParser

import Control.Applicative
import Data.Char

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
  pure a = Parser (\s -> Just (a,s))
  Parser p1 <*> Parser p2 = Parser $ \s -> case p1 s of
                                           Nothing    -> Nothing
                                           Just (f,s) -> case p2 s of
                                             Nothing     -> Nothing
                                             Just (x,s') -> Just (f x, s')

