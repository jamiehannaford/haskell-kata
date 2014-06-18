module Party where

import Employee

import Data.Monoid
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp { empFun = f }) (GL l fc) = GL (l ++ [e]) (f + fc)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL _ f1) b@(GL _ f2) = if f1 >= f2 then a else b

treeFold' :: (a -> b -> b) -> b -> Tree a -> b
treeFold' _ x (Node {subForest = []}) = x
treeFold' f x (Node {rootLabel = rl, subForest = (s:ss)}) = treeFold' f (f rl x) (Node {rootLabel = (rootLabel s), subForest = ss})

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f tree = f (rootLabel tree) $ map (treeFold f) $ subForest tree

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss lists = 
  let withBoss    = glCons boss $ mconcat $ map snd lists
      withoutBoss = mconcat $ map fst lists
  in (withBoss, withoutBoss)

maxFun :: Tree Employee -> GuestList
maxFun l = let pair = treeFold nextLevel l in moreFun (fst pair) (snd pair)
