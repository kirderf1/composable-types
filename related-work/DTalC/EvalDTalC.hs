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
    evalAlgebra (Const x) = x

-- | Eval instance for operations
instance Eval Op where
    evalAlgebra (Add x y) = x + y
    evalAlgebra (Mul x y) = x * y

-- | Eval instance for coproduct
instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlgebra (Inl x) = evalAlgebra x
    evalAlgebra (Inr y) = evalAlgebra y
