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
expr = infixl1 OfTerm term (Add <$ token (char '+') <|> Sub <$ token (char '-'))

term :: Parser Term
term = infixl1 OfNegate negate (Mul <$ token (char '*'))

negate :: Parser Negate
negate = prefix OfAtom (Neg <$ keyword "negate") atom

atom :: Parser Atom
atom = token (char '(') *> (Parens <$> expr) <* token (char ')')
   <|> Num <$> number
   <|> Var <$> ident
