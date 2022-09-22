{-#LANGUAGE TypeOperators, UndecidableInstances, FlexibleInstances,
    MultiParamTypeClasses #-}

module DesugDTalC where

import DTalC
import ExprDTalC
import NegationDTalC

-- | Transformative function desug using algebra
class (Functor f, Functor g) => Desug f g where
    desugAlgebra :: f (Term g) -> (Term g)

-- | Fold using desugAlgebra
desug :: (Desug f g, Functor f) => Term f -> Term g
desug = foldTerm desugAlgebra

-- | Default instance of Desug
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, f :<: g) => Desug f g where
    desugAlgebra = inject

-- | Desug instance for negation
instance (Functor g, Const :<: g, Op :<: g) => Desug Neg g where
    desugAlgebra (Neg e) = iConst (-1) `iMul` e

instance (Desug f h, Desug g h) => Desug (f :+: g) h where
    desugAlgebra (Inl x) = desugAlgebra x
    desugAlgebra (Inr y) = desugAlgebra y


