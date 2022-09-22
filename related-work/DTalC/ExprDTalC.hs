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
    fmap f (Const x ) = Const x

instance Functor Op where
    fmap f (Add e1 e2) = Add (f e1) (f e2)
    fmap f (Mul e1 e2) = Mul (f e1) (f e2) 
    
-- | Smart constructors
iConst :: (Const :<: f ) => Int -> Term f
iConst x = inject (Const x)

iAdd :: (Op :<: f ) => Term f -> Term f -> Term f
iAdd x y = inject (Add x y)

iMul :: (Op :<: f ) => Term f -> Term f -> Term f
iMul x y = inject (Mul x y)
