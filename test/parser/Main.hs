{-# LANGUAGE FlexibleInstances #-}
module Main where

import Arbitrary
import qualified WeakAST as Weak
import qualified StrongAST as Strong

import Test.Tasty
import Test.Tasty.QuickCheck
import Pretty
import Parser

main :: IO ()
main = defaultMain $ testGroup "Conversions" [
    testProperty "parse . pretty = id" roundtrip
  ]

newtype MakeNice a = MakeNice a deriving Eq

instance Pretty e => Show (MakeNice (Either String e)) where
  show (MakeNice (Right e)) = pretty e
  show (MakeNice (Left err)) = err
roundtrip e = within 100000 $ MakeNice (parseExpr (pretty e)) === MakeNice (Right e)