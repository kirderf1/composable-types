{-# LANGUAGE TypeOperators #-}

module EvalDTalC where

import DTalC
import ExprDTalC

-- | Eval function using an algebra
class Functor f => Eval f where
    evalAlgebra :: f Int -> Int

instance Eval Const where
    evalAlgebra (Const x) = x

instance Eval Op where
    evalAlgebra (Add x y) = x + y
    evalAlgebra (Mul x y) = x * y

instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlgebra (Inl x) = evalAlgebra x
    evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Term f -> Int
eval = foldTerm evalAlgebra
