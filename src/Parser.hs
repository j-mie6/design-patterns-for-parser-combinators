module Parser where

import Prelude hiding (negate)

import WeakAST
import Miniparsec

import Data.Char (digitToInt)

{-|
This function should turn a string into an expression.
-}
parseExpr :: String -> Either String Expr
parseExpr = parse expr

alpha :: Parser Char
alpha = oneOf (['a' .. 'z'] ++ ['A' .. 'Z'])
    <?> "letter"
digit :: Parser Char
digit = oneOf ['0' .. '9']
    <?> "digit"
alphaNum :: Parser Char
alphaNum = alpha <|> digit

number :: Parser Int
number = foldl addDigit 0 <$> some digit
     <?> "number"

ident :: Parser [Char]
ident = alpha <:> many alphaNum
    <?> "identifier"

addDigit :: Int -> Char -> Int
addDigit n d = n * 10 + digitToInt d

expr :: Parser Expr
expr = chainr1 term (Add <$ char '+' <|> Sub <$ char '-')

term :: Parser Expr
term = chainr1 negate (Mul <$ char '*')

negate :: Parser Expr
negate = Neg <$> (string "negate" *> negate)
     <|> atom

atom :: Parser Expr
atom = char '(' *> expr <* char ')'
   <|> Num <$> number
   <|> Var <$> ident
