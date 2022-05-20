{-# LANGUAGE ComposableTypes #-}

module Eval where

piececategory Expr

-- Signature for values and operators
data piece Expr ==> Value = Const Int -- | Pair Expr Expr
data piece Expr ==> Op = Add Expr Expr | Mult Expr Expr -- | Fst Expr | Snd Expr

-- Signature for the simple expression language
type Sig = Expr ==> (Value | Op)

eval -: Expr -> Int


ext eval for Value where 
    eval (Const i) = i
   
ext eval for Op where
    eval (Add e1 e2) = eval e1 + eval e2
    eval (Mult e1 e2) = eval e1 * eval e2
