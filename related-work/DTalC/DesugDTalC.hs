{-# LANGUAGE TypeOperators, UndecidableInstances, FlexibleInstances,
    MultiParamTypeClasses #-}

module DesugDTalC where

import DTalC
import ExprDTalC
import NegationDTalC

-- | Transformative function desug using algebra
class (Functor f, Functor g) => Desug f g where
    desugAlg :: f (Term g) -> (Term g)

-- | Fold using desugAlg
desug :: (Desug f g, Functor f) => Term f -> Term g
desug = foldTerm desugAlg

-- | Default instance of Desug
instance {-# OVERLAPPABLE #-} 
        (Functor f, Functor g, f :<: g) 
        => Desug f g where
    desugAlg = inject

-- | Desug instance for negation
instance (Functor g, Const :<: g, Op :<: g) => Desug Neg g where
    desugAlg (Neg e) = iConst (-1) `iMul` e

instance (Desug f h, Desug g h) => Desug (f :+: g) h where
    desugAlg (Inl a) = desugAlg a
    desugAlg (Inr b) = desugAlg b
