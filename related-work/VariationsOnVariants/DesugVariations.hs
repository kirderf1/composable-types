{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts #-}

module DesugVariations where

import VariationsOnVariants
import ExprVariations
import NegationVariations

-- | Desugaring of negation    
desugNeg (Neg e) r = inj' (Mul (inj' (Const (-1))) (r e))

-- | Default case for desugaring where there is nothing to desugar
desugDef e r = In (fmap desug e)

-- | Desugaring function that removes negation and leave the rest as it is
desug :: (f :-: Neg ~ g, Without f Neg (Minus f Neg)
            , Op :<: g
            , Const :<: g
            , Functor g) 
            => Term f -> Term g
desug = cases (desugNeg ? desugDef) 
