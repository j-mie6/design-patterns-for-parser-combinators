{-# LANGUAGE TypeApplications #-}
module Arbitrary where

import qualified WeakAST as Weak
import qualified StrongAST as Strong

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

instance Arbitrary Strong.Expr where arbitrary = fmap strengthen arbitrary
instance Arbitrary Strong.Term where arbitrary = fmap strengthen arbitrary
instance Arbitrary Strong.Negate where arbitrary = fmap strengthen arbitrary
instance Arbitrary Strong.Atom where arbitrary = fmap strengthen arbitrary