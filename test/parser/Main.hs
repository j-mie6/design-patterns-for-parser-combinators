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
    --testProperty "parse . pretty = id" roundtrip
  ]

roundtrip e = parseExpr (pretty e) === e