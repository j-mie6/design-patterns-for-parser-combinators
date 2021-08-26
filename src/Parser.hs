{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Prelude hiding (negate)

import StrongAST
import Miniparsec
import Lexer

import Data.Char (digitToInt)

{-|
This function should turn a string into an expression.
-}
parseExpr :: String -> Either String Expr
parseExpr = parse (fully expr)

expr :: Parser Expr
expr = infixl1 OfTerm term (Add <$ "+" <|> Sub <$ "-")

term :: Parser Term
term = infixl1 OfNegate negate (Mul <$ "*")

negate :: Parser Negate
negate = prefix OfAtom (Neg <$ "negate") atom

atom :: Parser Atom
atom = "(" *> (Parens <$> expr) <* ")"
   <|> Num <$> number
   <|> Var <$> ident
