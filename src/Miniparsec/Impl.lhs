> {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE FlexibleInstances #-}
> module Miniparsec.Impl (
>   Parser, parse,
>   satisfy, try,
>   lookAhead, notFollowedBy,
>   unexpected, fail, (<?>),
>   line, col
> ) where

> import Prelude hiding (fail)
> import Data.Set (Set)
> import qualified Data.Set as Set
> import Data.List (intercalate, intersperse, elemIndices)
> import Data.Maybe (catMaybes)
> import Data.Function (on)
> import Control.Applicative (Applicative(liftA2), Alternative(empty, (<|>)))
> import Control.Selective ( Selective(..))

This implementation of a parser combinator library is based on the error semantics of
   https://github.com/j-mie6/Parsley, which is a refined version of
   https://hackage.haskell.org/package/megaparsec errors. The code for these projects is freely
   available for use here under the following licenses:
Copyright © 2015–present Megaparsec contributors
Copyright © 2007 Paolo Martini
Copyright © 1999–2000 Daan Leijen
All rights reserved.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
Copyright (c) 2020, Jamie Willis
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


> data Pos = Pos {
>   _line :: Int,
>   _col :: Int
> }

> instance Eq Pos where
>   (==) :: Pos -> Pos -> Bool
>   err1 == err2 = _line err1 == _line err2 && _col err1 == _col err2

> instance Ord Pos where
>   (<=) :: Pos -> Pos -> Bool
>   err1 <= err2 = _line err1 < _line err2 || (_line err1 == _line err2 && _col err1 <= _col err2)

> instance Show Pos where
>   show Pos{..} = "(" ++ show _line ++ ", " ++ show _col ++ ")"

> data Input = Input {
>   str :: String,
>   _pos :: Pos
> }

> data Error = Oops {
>   expected :: Set Item,
>   unexpect :: Maybe Item,
>   msgs :: [String],
>   errPos :: Pos
> } deriving Show

> instance Eq Error where
>   (==) :: Error -> Error -> Bool
>   (==) = (==) `on` errPos

> instance Ord Error where
>   (<=) :: Error -> Error -> Bool
>   (<=) = (<=) `on` errPos

> newtype Max a = Max { getMax :: a }
> instance (Alternative f, Ord a) => Semigroup (Max (f a)) where
>   Max m <> Max n = Max $ (max <$> m <*> n) <|> m <|> n

> instance Semigroup Error where
>   err1 <> err2
>     | err1 == err2 = err1 {
>         expected = expected err1 <> expected err2,
>         unexpect = getMax (Max (unexpect err1) <> Max (unexpect err2)),
>         msgs = msgs err1 ++ msgs err2
>       }
>     | err1 > err2 = err1
>     | err1 < err2 = err2

> -- Based on the Parsley's error format
> format :: String -> Error -> String
> format input Oops{..} =
>   let preamble         = Just $ show errPos ++ ":"
>       unexpectLine     = ("unexpected " ++) . show <$> unexpect
>       expectedLine     = fmap ("expected " ++) $ foldMap Just $ intersperse ", " $ map show $ Set.toList expected
>       inputLines       = lines input
>       problem          = Just $ "> " ++ if null inputLines then "" else inputLines !! (_line errPos - 1)
>       caret            = Just $ "  " ++ replicate (_col errPos - 1) ' ' ++ "^"
>       joinTogether p q = liftA2 (\x y -> x ++ " " ++ y) p q <|> p <|> q
>   in  intercalate "\n  " (catMaybes ([preamble `joinTogether` unexpectLine, expectedLine] ++ map Just msgs ++ [problem, caret]))

> data Item = Raw String | Named String | EndOfInput deriving (Eq, Ord)

> instance Show Item where
>   show EndOfInput = "end of input"
>   show (Raw " ") = "space"
>   show (Raw "\n") = "newline"
>   show (Raw "\t") = "tab"
>   show (Raw raw) = show (takeWhile (/= ' ') raw)
>   show (Named named) = named

> newtype Hints = Hints [Set Item] deriving (Semigroup, Monoid)

> data State a r = State {
>   input :: Input,
>   good :: a -> Input -> Hints -> Either String r,
>   bad :: Error -> Input -> Either String r
> }
> newtype Parser a = Parser (forall r. State a r -> Either String r)

> parse :: Parser a -> String -> Either String a
> parse (Parser k) input = k $ State {
>     input = Input {str = input, _pos = Pos {_line = 1, _col = 1}},
>     good = \x _ _ -> Right x,
>     bad = \err _ -> Left (format input err)
>   }

> instance Functor Parser where
>   fmap :: (a -> b) -> Parser a -> Parser b
>   fmap f (Parser k) =
>     Parser $ \st@State{good} -> k (st {
>       good = good . f
>     })

> instance Applicative Parser where
>   pure :: a -> Parser a
>   pure x = Parser $ \st@State{good, input} -> good x input mempty
>
>   liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
>   liftA2 f (Parser p) (Parser q) = Parser $ \st@State{..} ->
>     let doQ x input hs = q (State {
>         good = \y input' hs' -> good (f x y) input' (combineHints hs hs' (_pos input) (_pos input')),
>         bad = \err input' -> bad (withHints err hs (_pos input) (_pos input')) input',
>         input = input
>       })
>     in p (st {good = doQ})

> instance Selective Parser where
>   select :: Parser (Either a b) -> Parser (a -> b) -> Parser b
>   select (Parser p) (Parser q) = Parser $ \st@State{..} ->
>     let handle (Left x) = \input hs -> q (State {
>           good = \f input' hs' -> good (f x) input' (combineHints hs hs' (_pos input) (_pos input')),
>           bad = \err input' -> bad (withHints err hs (_pos input) (_pos input')) input',
>           input = input
>         })
>         handle (Right x) = good x
>     in p (st {good = handle})

> instance Alternative Parser where
>   empty :: Parser a
>   empty = Parser $ \st@State{bad, input} -> bad (Oops {
>       expected = Set.empty,
>       unexpect = Nothing,
>       msgs = [],
>       errPos = _pos input
>     }) input

>   (<|>) :: Parser a -> Parser a -> Parser a
>   Parser p <|> Parser q = Parser $ \st@State{..} ->
>     let doQ err input'
>           | _pos input < _pos input'  = bad err input'
>           | _pos input == _pos input' = q (st {
>               good = \x input' hs ->
>                 if _pos input == _pos input' then good x input' (toHints err (_pos input') <> hs)
>                 else                         good x input' hs,
>               bad = \err' -> bad (err <> err')
>             })
>     in p (st {bad = doQ})

> satisfy :: (Char -> Bool) -> Parser Char
> satisfy f = Parser $ \st@State{..} -> case str input of
>   c:cs | f c -> let p@Pos{..} = _pos input in case c of
>     '\n' -> good '\n' (input {str = cs, _pos = p {_col = 1, _line = _line + 1}}) mempty
>     c    -> good c (input {str = cs, _pos = p {_col = _col + 1}}) mempty
>   cs         -> bad (Oops {
>     expected = Set.empty,
>     unexpect = Just (foldr (const . Raw . pure) EndOfInput cs),
>     msgs = [],
>     errPos = _pos input
>   }) input

> try :: Parser a -> Parser a
> try (Parser p) = Parser $ \st@State{..} -> p (st {bad = \err _ -> bad err input})

> lookAhead :: Parser a -> Parser a
> lookAhead (Parser p) = Parser $ \st@State{..} -> p (st {good = \x _ _ -> good x input mempty})

> notFollowedBy :: Parser a -> Parser ()
> notFollowedBy (Parser p) = Parser $ \st@State{..} ->
>   let oldPos = _pos input
>       item newPos = take (_col newPos - _col oldPos)
>                   $ head (lines (str input))
>       err input' = Oops {
>           expected = Set.empty,
>           unexpect = Just $ if null (str input) then EndOfInput else Raw (item (_pos input')),
>           msgs = [],
>           errPos = oldPos
>         }
>   in p (st {
>     good = \_ input' _ -> bad (err input') input,
>     bad = \_ _ -> good () input mempty
>   })

> unexpected :: String -> Parser a
> unexpected msg = Parser $ \st@State{bad, input} -> bad (Oops {
>     expected = Set.empty,
>     unexpect = Just (Named msg),
>     msgs = [],
>     errPos = _pos input
>   }) input

> fail :: String -> Parser a
> fail msg = Parser $ \st@State{bad, input} -> bad (Oops {
>     expected = Set.empty,
>     unexpect = Nothing,
>     msgs = [msg],
>     errPos = _pos input
>   }) input

> line :: Parser Int
> line = Parser $ \State{good, input} -> good (_line (_pos input)) input mempty

> col :: Parser Int
> col = Parser $ \State{good, input} -> good (_col (_pos input)) input mempty

> infix 0 <?>
> (<?>) :: Parser a -> String -> Parser a
> Parser p <?> label = Parser $ \st@State{..} ->
>   let label'
>         | null label = Nothing
>         | otherwise  = Just (Named label)
>       hintFix x input' hs
>         | _pos input < _pos input', Nothing <- label' = good x input' (refreshLastHint hs Nothing)
>         | _pos input < _pos input'                    = good x input' hs
>         | _pos input == _pos input'                   = good x input' (refreshLastHint hs label')
>       labelApply err input' = flip bad input' $
>         if _pos input == _pos input' then err { expected = maybe Set.empty Set.singleton label' }
>         else                              err
>   in p (st {good = hintFix, bad = labelApply})

> combineHints :: Hints -> Hints -> Pos -> Pos -> Hints
> combineHints hs hs' pos pos'
>   | pos == pos' = hs <> hs'
>   | pos < pos'  = hs'
>   | otherwise   = error (show pos ++ " is not <= " ++ show pos')

> withHints :: Error -> Hints -> Pos -> Pos -> Error
> withHints err (Hints hs) pos pos'
>   | pos == pos' = err { expected = Set.unions (expected err : hs) }
>   | pos < pos'  = err
>   | otherwise   = error (show pos ++ " is not <= " ++ show pos')

> -- Taken from megaparsec
> refreshLastHint :: Hints -> Maybe Item -> Hints
> refreshLastHint (Hints [])       _        = Hints []
> refreshLastHint (Hints (_ : hs)) Nothing  = Hints hs
> refreshLastHint (Hints (_ : hs)) (Just h) = Hints (Set.singleton h : hs)

> -- Taken from megaparsec
> toHints :: Error -> Pos -> Hints
> toHints err@Oops{..} pos
>   | errPos == pos = Hints [expected | not (Set.null expected)]
>   | otherwise     = mempty