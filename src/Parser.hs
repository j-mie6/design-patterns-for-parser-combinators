module Parser where

import Prelude hiding (negate)

import WeakAST
import Miniparsec
import Lexer

{-|
This function should turn a string into an expression.
-}
-- Should be a good issue for a newcomer?
parseExpr :: String -> Either String Expr
parseExpr = parse (fully expr)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p <**> rest
  where
    rest = flip (.) <$> (flip <$> op <*> p) <*> rest
       <|> pure id

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p <**> (flip <$> op <*> chainr1 p op <|> pure id)

expr = chainr1 term (Add <$ token (char '+') <|> Sub <$ token (char '-'))
term = chainr1 negate (Mul <$ token (char '*'))
negate = Neg <$> (token (string "negate") *> negate) <|> atom

atom = token (char '(') *> expr <* token (char ')')
   <|> Num <$> number
   <|> Var <$> ident