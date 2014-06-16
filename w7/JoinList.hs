{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) x y = Append (tag x `mappend` tag y) x y

{-- TEST HELPERS --}
(!!?) :: [a] -> Int -> Maybe a
[] !!? _         = Nothing
_  !!? i | i < 0 = Nothing
(x:xs) !!? 0     = Just x
(x:xs) !!? i     = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
{-- END TEST HELPERS --}

getListSize :: (Sized b, Monoid b) => JoinList b a -> Int
getListSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ _ Empty = Nothing
indexJ i (Single _ a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i (Append m jl1 jl2)
  | i < size1 = indexJ i jl1
  | otherwise = indexJ (i - size1) jl2
  where size1 = getListSize jl1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i list | i <= 0 = list
dropJ i list = case list of
  Append s left right ->
    if i < size
    then dropJ i left +++ right
    else dropJ (i - size) right
    where size = getListSize left
  _ -> Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i _ | i <= 0 = Empty
takeJ i list = case list of
  (Append m left right) ->
    if i < size
    then takeJ i left
    else left +++ (takeJ (i - size) right)
    where size = getListSize left
  _ -> list

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

getScore :: Score -> Int
getScore (Score x) = x

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString str = convert $ lines str
    where convert [] = Empty
          convert (x:xs) = (Single (scoreString x, Size 1) x) +++ convert xs
  line = indexJ
  replaceLine i s l = (takeJ i l) +++ Single (scoreString s, Size 1) s +++ (dropJ (i+1) l)
  numLines = getSize . snd . tag 
  value = getScore . fst . tag

buffer :: String -> JoinList (Score, Size) String
buffer = fromString

main = runEditor editor $ buffer $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] 
