module Parser where

import Prelude hiding (negate)

import WeakAST
import Miniparsec

{-|
This function should turn a string into an expression.
-}
-- Should be a good issue for a newcomer?
parseExpr :: String -> Either String Expr
parseExpr = undefined