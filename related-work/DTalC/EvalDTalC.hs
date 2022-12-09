{-# LANGUAGE TypeOperators #-}

module EvalDTalC where

import DTalC
import ExprDTalC

-- | Eval function using an algebra
class Functor f => Eval f where
    evalAlg :: f Int -> Int

-- | Fold using evalAlg
eval :: Eval f => Term f -> Int
eval = foldTerm evalAlg

-- | Eval instance for constants
instance Eval Const where
    evalAlg (Const i) = i

-- | Eval instance for operations
instance Eval Op where
    evalAlg (Add e1 e2) = e1 + e2
    evalAlg (Mul e1 e2) = e1 * e2

-- | Eval instance for coproduct
instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlg (Inl a) = evalAlg a
    evalAlg (Inr b) = evalAlg b