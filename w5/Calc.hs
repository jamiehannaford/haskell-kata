module Calc where

import ExprT
import Parser

-- evaluate expression into integer
eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- evaluate string into maybe string
evalStr :: String -> Maybe ExprT
evalStr = parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  mul x y = Mul x y
  add x y = Add x y

instance Expr Integer where
  lit x = x
  mul x y = x * y
  add x y = x + y

instance Expr Bool where
  lit x = if x <= 0 then False else True
  mul x y = x || y
  add x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul ("(3 * -4) + 5")

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
