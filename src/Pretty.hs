{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Pretty where

import qualified WeakAST as Weak
import qualified StrongAST as Strong

import Convert (strengthen)

class Pretty a where
  pretty :: a -> String

instance Pretty Weak.Expr where
  -- Pretty printing with minimal brackets is hard with the weak AST provided to us
  pretty :: Weak.Expr -> String
  pretty = pretty . strengthen @Strong.Expr

-- But easy with a strongly typed one!
instance Pretty Strong.Expr where
  pretty :: Strong.Expr -> String
  pretty (Strong.Add x y)  = pretty x ++ " + " ++ pretty y
  pretty (Strong.Sub x y)  = pretty x ++ " - " ++ pretty y
  pretty (Strong.OfTerm x) = pretty x

instance Pretty Strong.Term where
  pretty :: Strong.Term -> String
  pretty (Strong.Mul x y)    = pretty x ++ " * " ++ pretty y
  pretty (Strong.OfNegate x) = pretty x

instance Pretty Strong.Negate where
  pretty :: Strong.Negate -> String
  pretty (Strong.Neg x)    = "negate " ++ pretty x
  pretty (Strong.OfAtom x) = pretty x

instance Pretty Strong.Atom where
  pretty :: Strong.Atom -> String
  pretty (Strong.Num n)    = show n
  pretty (Strong.Var v)    = v
  pretty (Strong.Parens x) = pretty x