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
evalNeg :: (Expr e -> Int) -> (X_NegExt e -> Int) -> Neg e -> Int
evalNeg g _ (Neg e) = (-1) * g e
evalNeg _ f (NegExt e) = f e

-- | asString of Neg
asStringNeg :: (Expr e -> String) -> (X_NegExt e -> String) -> Neg e -> String
asStringNeg g _ (Neg e) = "(-" ++ g e ++ ")"
asStringNeg _ f (NegExt e) = f e
