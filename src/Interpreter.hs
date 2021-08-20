module Interpreter where

import AST

{-|
The backbone of our fancy interpreter, evaluates an expression with a context
to produce a final value.
-}
eval :: Expr -> (String -> Int) -> Int
eval (Add x y) = (+) <$> eval x <*> eval y
eval (Sub x y) = (-) <$> eval x <*> eval y
eval (Mul x y) = (*) <$> eval x <*> eval y
eval (Neg x)   = negate <$> eval x
eval (Num n)   = pure n
eval (Var x)   = ($ x)