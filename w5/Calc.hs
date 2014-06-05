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

instance ExprT Integer where
  lit :: Integer -> ExprT
  mul :: 
  add :: 
