{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module ExprDTalC where

import DTalC

-- | Data type for expression language

-- | Data type variants for Const, Add and Mul
data Const a = Const Int

data Op a = Add a a | Mul a a 

-- | Composed type Expr
type Expr = Term (Const :+: Op)

-- | Functors
instance Functor Const where
    fmap f (Const i) = Const i

instance Functor Op where
    fmap f (Add e1 e2) = Add (f e1) (f e2)
    fmap f (Mul e1 e2) = Mul (f e1) (f e2) 
    
-- | Smart constructors
iConst :: (Const :<: f) => Int -> Term f
iConst i = inject (Const i)

iAdd :: (Op :<: f) => Term f -> Term f -> Term f
iAdd e1 e2 = inject (Add e1 e2)

iMul :: (Op :<: f) => Term f -> Term f -> Term f
iMul e1 e2 = inject (Mul e1 e2)
