{-# LANGUAGE TypeFamilies #-}

module ExprTrees where

-- | Basis for this is from:
-- Shayan Najd and Simon Peyton Jones. Trees that Grow.
-- Journal of Universal Computer Science, 23(1):42–62, 2017

import Data.Void

-- | Data type for expression language

-- | The data type, with extensible fields and 
-- one extensible variant
data Expr e = Const Int 
            | Add (Expr e) (Expr e)
            | Mul (Expr e) (Expr e) 
            | ExprExt (X_ExprExt e)

-- | The extension fields is defined through a type family
type family X_ExprExt e

-- | Undecorated composition of Expr, i.e. with no extension

data UD

-- | The type instance is Void since there is no extension
type instance X_ExprExt UD = Void
