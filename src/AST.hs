module AST where

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Neg Expr
          | Num Int
          | Var String

