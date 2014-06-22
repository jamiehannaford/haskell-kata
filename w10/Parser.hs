module Parser where

import AParser

import Control.Applicative
import Data.Char
import Data.List

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

eval :: Maybe (Employee, String) -> String
eval Nothing = ""
eval (Just (e, _)) = show e

type Name = String
data Employee = Emp { name :: Name, phone :: String } deriving (Show)

parserStr :: (Char -> Bool) -> Parser String
parserStr p = Parser f
  where f [] = Nothing
        f x  = Just (match, rest)
          where match = takeWhile p x
                rest  = x \\ match

parseName :: Parser Name
parseName = parserStr isAlpha

parsePhone :: Parser String
parsePhone = parserStr isDigit

checkAb :: String -> Bool
checkAb s
  | length s <= 1 = False
  | otherwise     = if (s !! 0) == 'a' && (s !! 1) == 'b' then True else False

abParser' :: Parser (Char, Char)
abParser' = Parser $ \s@(x:y:z) -> if checkAb s then Just ((x,y), z) else Nothing

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b' 

abParser_ :: Parser ()
abParser_ = pure (const ()) <*> abParser

intPair :: Parser [Integer]
intPair = (\x _ z -> [x,z]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser x) <|> (Parser y) = Parser $ \s -> x s <|> y s

ignore :: Parser a -> Parser ()
ignore = (pure (const ()) <*>)

intOrUppercase :: Parser ()
intOrUppercase = ignore posInt <|> ignore (satisfy isUpper)
