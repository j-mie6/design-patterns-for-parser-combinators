module StrongAST where

-- Pretty printing with minimal brackets is hard with the AST provided to us
-- This datastructure normalises the AST so that parentheses are exposed explicitly
data Expr = Add Expr Term | Sub Expr Term
           | OfTerm Term
data Term = Mul Term Negate | OfNegate Negate
data Negate = Neg Negate | OfAtom Atom
data Atom = Num Int | Var String | Parens Expr