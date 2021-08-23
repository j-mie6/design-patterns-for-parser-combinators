module Lexer (number, ident, fully, lexeme, token) where

import Miniparsec

import Data.Char (digitToInt, isSpace)

alpha :: Parser Char
alpha = oneOf (['a' .. 'z'] ++ ['A' .. 'Z'])
    <?> "letter"
digit :: Parser Char
digit = oneOf ['0' .. '9']
    <?> "digit"
alphaNum :: Parser Char
alphaNum = alpha <|> digit

number :: Parser Int
number = token (foldl addDigit 0 <$> some digit)
     <?> "number"

ident :: Parser [Char]
ident = token (alpha <:> many alphaNum)
    <?> "identifier"

addDigit :: Int -> Char -> Int
addDigit n d = n * 10 + digitToInt d

whitespace :: Parser ()
whitespace = () <$ many (satisfy isSpace)

fully :: Parser a -> Parser a
fully p = whitespace *> p <* eof

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

token :: Parser a -> Parser a
token = lexeme . try