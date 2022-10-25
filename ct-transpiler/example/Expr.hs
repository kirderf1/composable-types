{-# LANGUAGE ComposableTypes #-}

module Expr where

-- | Data type for expression language

-- | Category ExprCat
piececategory ExprCat

-- | Data type variants for Const, Add and Mul, contained in two pieces
data piece ExprCat ==> Const = Const Int

data piece ExprCat ==> Op = Add ExprCat ExprCat
                              | Mul ExprCat ExprCat
    
-- | Composed type Expr
type Expr = ExprCat ==> (Const | Op)