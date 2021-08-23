{-# LANGUAGE TypeApplications, StandaloneDeriving, DeriveGeneric #-}
module Arbitrary where

import qualified WeakAST as Weak
import qualified StrongAST as Strong

import GHC.Generics

import Convert (strengthen)

import Test.Tasty.QuickCheck

newtype Name = Name { fromName :: String }
newtype Nat = Nat { fromNat :: Int }

instance Arbitrary Name where
  arbitrary = Name <$> oneof [return "x", return "y", return "z", return "negatex", return "x1"]

instance Arbitrary Nat where
  arbitrary = Nat . fromEnum <$> arbitrary @Word

instance Arbitrary Weak.Expr where
  arbitrary = sized expr
    where
      atom :: Gen Weak.Expr
      atom = either (Weak.Num . fromNat) (Weak.Var . fromName) <$> arbitrary

      expr :: Int -> Gen Weak.Expr
      expr 0 = atom
      expr n = oneof [ atom
                     , Weak.Add <$> subexpr <*> subexpr
                     , Weak.Sub <$> subexpr <*> subexpr
                     , Weak.Mul <$> subexpr <*> subexpr
                     , Weak.Neg <$> subexpr
                     ]
        where
          subexpr = expr (n `div` 2)
  shrink (Weak.Var v) = []
  shrink e            = genericShrink e

instance Arbitrary Strong.Expr where
  arbitrary = fmap strengthen arbitrary
  shrink = genericShrink
instance Arbitrary Strong.Term where
  arbitrary = fmap strengthen arbitrary
  shrink = genericShrink
instance Arbitrary Strong.Negate where
  arbitrary = fmap strengthen arbitrary
  shrink = genericShrink
instance Arbitrary Strong.Atom where
  arbitrary = fmap strengthen arbitrary
  shrink (Strong.Num n) = map Strong.Num (shrink n)
  shrink (Strong.Var v) = []
  shrink (Strong.Parens x) = map Strong.Parens (shrink x)

deriving instance Generic Weak.Expr
deriving instance Generic Strong.Expr
deriving instance Generic Strong.Term
deriving instance Generic Strong.Negate