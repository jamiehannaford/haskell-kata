{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Char

toInt :: String -> Int
toInt s = read s

parseMessage :: String -> LogMessage
parseMessage s = case words s of
              ("E":e:n:rest) -> LogMessage (Error (toInt e)) (toInt n) (unwords rest)
              ("W":n:rest)   -> LogMessage Warning (toInt n) (unwords rest)
              ("I":n:rest)   -> LogMessage Info (toInt n) (unwords rest)
              s              -> Unknown (unwords s)

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

timestamp :: LogMessage -> Int
timestamp (LogMessage (Error _) ts _) = ts
timestamp (LogMessage _ ts _) = ts
timestamp _ = 0

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert _ node@(Node _ (Unknown _) _) = node
insert msg (Node ltree nodeMsg rtree) = if timestamp msg > timestamp nodeMsg
                                        then Node rtree nodeMsg (insert msg ltree) 
                                        else Node (insert msg ltree) nodeMsg rtree

build :: [LogMessage] -> MessageTree
build []  = Leaf
build [x] = insert x Leaf
build (x:xs) = insert x $ build xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node n1 msg n2) = (inOrder n1) ++ [msg] ++ (inOrder n2)
