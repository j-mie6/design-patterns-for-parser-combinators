{-# LANGUAGE InstanceSigs #-}
module Convert where

import qualified WeakAST as Weak
import qualified StrongAST as Strong

class Weaken e where
  weaken :: e -> Weak.Expr

instance Weaken Strong.Expr where
  weaken :: Strong.Expr -> Weak.Expr
  weaken (Strong.Add x y) = Weak.Add (weaken x) (weaken y)
  weaken (Strong.Sub x y) = Weak.Sub (weaken x) (weaken y)
  weaken (Strong.OfTerm x) = weaken x

instance Weaken Strong.Term where
  weaken :: Strong.Term -> Weak.Expr
  weaken (Strong.Mul x y) = Weak.Mul (weaken x) (weaken y)
  weaken (Strong.OfNegate x) = weaken x

instance Weaken Strong.Negate where
  weaken :: Strong.Negate -> Weak.Expr
  weaken (Strong.Neg x) = Weak.Neg (weaken x)
  weaken (Strong.OfAtom x) = weaken x

instance Weaken Strong.Atom where
  weaken :: Strong.Atom -> Weak.Expr
  weaken (Strong.Parens x) = weaken x
  weaken (Strong.Num n) = Weak.Num n
  weaken (Strong.Var v) = Weak.Var v

class Strengthen e where
  -- This function converts a weakly typed AST into a strong one
  strengthen :: Weak.Expr -> e

instance Strengthen Strong.Atom where
  strengthen :: Weak.Expr -> Strong.Atom
  strengthen (Weak.Num n) = Strong.Num n
  strengthen (Weak.Var v) = Strong.Var v
  strengthen e            = Strong.Parens (strengthen e)

instance Strengthen Strong.Negate where
  strengthen :: Weak.Expr -> Strong.Negate
  strengthen (Weak.Neg x) = Strong.Neg (strengthen x)
  strengthen e            = Strong.OfAtom (strengthen e)

instance Strengthen Strong.Term where
  strengthen :: Weak.Expr -> Strong.Term
  strengthen (Weak.Mul x y) = Strong.Mul (strengthen x) (strengthen y)
  strengthen e              = Strong.OfNegate (strengthen e)

instance Strengthen Strong.Expr where
  strengthen :: Weak.Expr -> Strong.Expr
  strengthen (Weak.Add x y) = Strong.Add (strengthen x) (strengthen y)
  strengthen (Weak.Sub x y) = Strong.Sub (strengthen x) (strengthen y)
  strengthen e              = Strong.OfTerm (strengthen e)