{-# LANGUAGE TypeFamilies, PatternSynonyms #-}

module NegationTrees where

import ExprTrees
import EvalTrees
import AsStringTrees

-- | Data type for Neg, containing Neg for negation, and is also 
-- extensible in variants.
data Neg e = Neg (Expr e) | NegExt (X_NegExt e)

-- | The extension field of Neg is defined through a type family
type family X_NegExt e

 -- | Useful pattern synonym
pattern NegP :: (X_ExprExt e ~ Neg e) => Expr e -> Expr e
pattern NegP e <- ExprExt (Neg e)
    where NegP e = ExprExt (Neg e)
          
-- | Evaluation of Neg          
evalNeg :: (X_ExprExt e ~ Neg e) 
    => (X_NegExt e -> Int) -> Neg e -> Int
evalNeg f (Neg e) = (-1) * eval (evalNeg f) e
evalNeg f (NegExt e) = f e

-- | asString of Neg
asStringNeg :: (X_ExprExt e ~ Neg e) 
    => (X_NegExt e -> String) -> Neg e -> String
asStringNeg f (Neg e) = "(-" ++ asString (asStringNeg f) e ++ ")"
asStringNeg f (NegExt e) = f e
