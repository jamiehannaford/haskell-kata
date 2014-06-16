module Party where

import Employee

import Data.Monoid

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp { empFun = f1 }) (GL es f) = GL (es ++ [e]) (f+f1)

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend l1@(GL es1 f1) l2@(GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) = if f1 >= f2 then g1 else g2
