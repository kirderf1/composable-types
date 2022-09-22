{-# LANGUAGE TypeOperators #-}

module EvalDTalC where

import DTalC
import ExprDTalC

-- | Eval function using an algebra
class Functor f => Eval f where
    evalAlgebra :: f Int -> Int

-- | Fold using evalAlgebra
eval :: Eval f => Term f -> Int
eval = foldTerm evalAlgebra

-- | Eval instance for constants
instance Eval Const where
    evalAlgebra (Const i) = i

-- | Eval instance for operations
instance Eval Op where
    evalAlgebra (Add e1 e2) = e1 + e2
    evalAlgebra (Mul e1 e2) = e1 * e2

-- | Eval instance for coproduct
instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlgebra (Inl a) = evalAlgebra a
    evalAlgebra (Inr b) = evalAlgebra b