{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts #-}

module DesugVariations where

import VariationsOnVariants
import ExprVariations
import NegationVariations

-- | Desugaring of negation 
desugNeg :: (Const :<: f, Op :<: f) => Neg e -> (e -> Term f) -> Term f
desugNeg (Neg e) r = iMul (iConst (-1)) (r e)

-- | Default case for desugaring where there is nothing to desugar
desugDef :: Functor g => g (Term f) -> (Term f -> Term g) -> Term g
desugDef e r = In (fmap r e)

-- | Desugaring function that removes negation and leave the rest as it is
desug :: (f :-: Neg ~ g, Without f Neg (Minus f Neg)
            , Op :<: g, Const :<: g, Functor g) 
            => Term f -> Term g
desug = cases (desugNeg ? desugDef) 
