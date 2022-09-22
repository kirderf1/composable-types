{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module NegationDTalC where

import DTalC
import EvalDTalC
import RenderDTalC

-- | Data type for negation
data Neg a = Neg a

-- | Functor
instance Functor Neg where
    fmap f (Neg e) = Neg (f e)

-- | Smart constructor
iNeg :: (Neg :<: f) => Term f -> Term f
iNeg e = inject (Neg e)

-- | Eval instance for negation
instance Eval Neg where
    evalAlgebra (Neg e) = (-1) * e
    
-- | Render instance for negation
instance Render Neg where
    render' (Neg e) = "(-" ++ render e ++ ")"