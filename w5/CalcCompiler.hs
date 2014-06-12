{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Calc
import qualified StackVM
import ExprT
import Data.Maybe

instance Expr StackVM.Program where
  lit x   = (StackVM.PushI x):[]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

expr :: ExprT -> StackVM.Program
expr (Lit x)   = lit x
expr (Mul x y) = mul (expr x) (expr y)
expr (Add x y) = add (expr x) (expr y)

compile :: String -> StackVM.Program
compile s = expr (fromMaybe (Lit 0) (evalStr s))
