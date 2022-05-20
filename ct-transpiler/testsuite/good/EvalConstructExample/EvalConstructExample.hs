{-# LANGUAGE ComposableTypes #-}

module EvalConstructExample where

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
    
-- Example: evalEx = 5
evalEx :: Int
evalEx = eval (Const 1 `Add` (Const 2 `Mult` Const 2) :: Sig)
