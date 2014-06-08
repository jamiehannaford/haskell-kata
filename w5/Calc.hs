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

reify :: ExprT -> ExprT
reify = id
