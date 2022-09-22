{-# LANGUAGE TypeOperators #-}

module ExprVariations where

import VariationsOnVariants

-- | Data type for expression language

-- | Data type variations for Const, Add and Mul
data Const e = Const Int
data Op e = Add e e | Mul e e

-- | Composed type Expr
type Expr = Term (Const :+: Op)

-- | Functors
instance Functor Const
    where fmap _ (Const x) = Const x
          
instance Functor Op
    where fmap f (Add e1 e2) = Add (f e1) (f e2)
          fmap f (Mul e1 e2) = Mul (f e1) (f e2)

-- | Smart constructors?
