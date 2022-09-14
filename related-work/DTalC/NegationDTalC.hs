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

-- | Instance of the eval function
instance Eval Neg where
    evalAlgebra (Neg a) = (-1) * a
    
-- | Instance of the render function
instance Render Neg where
    render (Neg e) = "(-" ++ pretty e ++ ")"
