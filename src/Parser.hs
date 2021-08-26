module Parser where

import Prelude hiding (negate)

import WeakAST
import Miniparsec
import Lexer

import Data.Char (digitToInt)

{-|
This function should turn a string into an expression.
-}
parseExpr :: String -> Either String Expr
parseExpr = parse (fully expr)

expr :: Parser Expr
expr = chainr1 term (Add <$ token (char '+') <|> Sub <$ token (char '-'))

term :: Parser Expr
term = chainr1 negate (Mul <$ token (char '*'))

negate :: Parser Expr
negate = Neg <$> (token (string "negate") *> negate)
     <|> atom

atom :: Parser Expr
atom = token (char '(') *> expr <* token (char ')')
   <|> Num <$> number
   <|> Var <$> ident
