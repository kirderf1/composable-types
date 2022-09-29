{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module ExprTrees where

--- | Basis for this is from:
--- Shayan Najd and Simon Peyton Jones. Trees that Grow. Journal of Universal Computer Science, 23(1):42–62, 2017

import Data.Void

-- | Data type for expression language

-- | The data type, with extensible fields and one extensible variant
data Expr e = Const (X_Const e) Int 
            | Add (X_Add e) (Expr e) (Expr e)
            | Mul (X_Mul e) (Expr e) (Expr e) 
            | ExprExt (X_ExprExt e)

-- | The variants are defined through type families
type family X_Const e
type family X_Add e
type family X_Mul e
type family X_ExprExt e

-- | Undecorated version of Expr, i.e. with no extension

data UD

-- | Each type instance is Void since there is no extension
type instance X_Const UD = ()
type instance X_Add UD = ()
type instance X_Mul UD = ()
type instance X_ExprExt UD = Void

-- | Pattern synonyms that are useful to simplify code
pattern Const_UD :: Int -> Expr UD
pattern Const_UD i <- Const _ i
    where Const_UD i = Const () i
          
pattern Add_UD :: Expr UD -> Expr UD -> Expr UD
pattern Add_UD e1 e2 <- Add _ e1 e2
    where Add_UD e1 e2 = Add () e1 e2
          
pattern Mul_UD :: Expr UD -> Expr UD -> Expr UD
pattern Mul_UD e1 e2 <- Mul _ e1 e2
    where Mul_UD e1 e2 = Mul () e1 e2
