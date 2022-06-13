{-# LANGUAGE ComposableTypes #-}

module ImportEval where

import A
import B

data piece Expr ==> Op = Add Expr Expr | Mult Expr Expr

type Sig = Expr ==> (Value | Op)

ext eval for Op where
    eval (Add e1 e2) = eval e1 + eval e2
    eval (Mult e1 e2) = eval e1 * eval e2
    
evalEx :: Int
evalEx = eval (Const 1 `Add` (Const 2 `Mult` Const 2) :: Sig)
