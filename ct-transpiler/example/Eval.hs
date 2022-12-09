{-# LANGUAGE ComposableTypes #-}

module Eval where

import Expr

-- | Evaluation function
eval -: ExprCat -> Int

-- | Eval extension for constants
ext eval for Const where
    eval (Const i) = i
    
-- | Eval extension for operations
ext eval for Op where
    eval (Add e1 e2) = eval e1 + eval e2
    eval (Mul e1 e2) = eval e1 * eval e2
