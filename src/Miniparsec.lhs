> module Miniparsec (
>    module Miniparsec,
>    module Miniparsec.Impl,
>    module Data.Functor,
>    module Control.Applicative,
>    module Control.Selective
>  ) where

> import Miniparsec.Impl
> import Data.Functor
> import Control.Applicative
> import Control.Selective
> import Data.Foldable (asum)

> infixr 4 <:>
> (<:>) :: Applicative f => f a -> f [a] -> f [a]
> (<:>) = liftA2 (:)

> infixl 4 <~>
> (<~>) :: Applicative f => f a -> f b -> f (a, b)
> (<~>) = liftA2 (,)

> choice :: Alternative f => [f a] -> f a
> choice = asum

> infixl 3 >?>
> (>?>) :: (Selective f, Alternative f) => f a -> (a -> Bool) -> f a
> fx >?> p = select ((\x -> if p x then Right x else Left ()) <$> fx) empty

> filteredBy :: (Selective f, Alternative f) => f a -> (a -> Bool) -> f a
> filteredBy = (>?>)

> char :: Char -> Parser Char
> char c = satisfy (== c) <?> show [c]

> string :: String -> Parser String
> string str = traverse char str <?> show str

> item :: Parser Char
> item = satisfy (const True) <?> "any character"

> oneOf :: [Char] -> Parser Char
> oneOf = choice . map char

> noneOf :: [Char] -> Parser Char
> noneOf = satisfy . (not .) . flip elem

> eof :: Parser ()
> eof = notFollowedBy item <?> "end of file"

> pos :: Parser (Int, Int)
> pos = line <~> col

> infixl1 :: (a -> b) -> Parser a -> Parser (b -> a -> b) -> Parser b
> infixl1 wrap p op = postfix wrap p (flip <$> op <*> p)

> infixr1 :: (a -> b) -> Parser a -> Parser (a -> b -> b) -> Parser b
> infixr1 wrap p op = p <**> (flip <$> op <*> infixr1 wrap p op <|> pure wrap)

> prefix :: (a -> b) -> Parser (b -> b) -> Parser a -> Parser b
> prefix wrap op p = op <*> prefix wrap op p <|> wrap <$> p

> postfix :: (a -> b) -> Parser a -> Parser (b -> b) -> Parser b
> postfix wrap p op = (wrap <$> p) <**> rest
>   where
>     rest = flip (.) <$> op <*> rest
>        <|> pure id

> chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
> chainl1 = infixl1 id

> chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
> chainr1 = infixr1 id
