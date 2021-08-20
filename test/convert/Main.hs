{-# LANGUAGE TypeApplications #-}
module Main where

import Arbitrary
import qualified WeakAST as Weak
import qualified StrongAST as Strong

import Test.Tasty
import Test.Tasty.QuickCheck
import Convert

main :: IO ()
main = defaultMain $ testGroup "Conversions" [
    testProperty "weaken . strengthen = id" sw,
    testProperty "strengthen . weaken = id" ws
  ]

ws :: Strong.Expr -> Property
ws e = strengthen @Strong.Expr (weaken e) === e

sw :: Weak.Expr -> Property
sw e = weaken (strengthen @Strong.Expr e) === e