{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module NegationVariations where

import VariationsOnVariants
import EvalVariations
import RenderVariations

-- | Data type for negation
data Neg e = Neg e

-- | Functor
instance Functor Neg where
    fmap f (Neg e) = Neg (f e)
    
-- | Smart constructor
iNeg :: Neg :<: f => Term f -> Term f
iNeg x = inject (Neg x)

-- | Evaluation of negation
evalNeg :: Neg e -> (e -> Int) -> Int
evalNeg (Neg e) r = (-1) * r e

-- | Rendering of negation
renderNeg :: Neg e -> (e -> String) -> String
renderNeg (Neg e) r = "(-" ++ r e ++ ")"
