{-# LANGUAGE InstanceSigs #-}
module Interpreter where

import qualified WeakAST as Weak
import qualified StrongAST as Strong

import Convert (weaken)

class Eval e where
  {-|
    The backbone of our fancy interpreter, evaluates an expression with a context
    to produce a final value.
  -}
  eval :: e -> (String -> Int) -> Int

instance Eval Weak.Expr where
  eval :: Weak.Expr -> (String -> Int) -> Int
  eval (Weak.Add x y) = (+) <$> eval x <*> eval y
  eval (Weak.Sub x y) = (-) <$> eval x <*> eval y
  eval (Weak.Mul x y) = (*) <$> eval x <*> eval y
  eval (Weak.Neg x)   = negate <$> eval x
  eval (Weak.Num n)   = pure n
  eval (Weak.Var x)   = ($ x)

instance Eval Strong.Expr where
  -- Evaluation is more straightforward with a weak AST
  eval :: Strong.Expr -> (String -> Int) -> Int
  eval = eval . weaken