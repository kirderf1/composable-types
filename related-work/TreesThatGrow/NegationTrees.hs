{-# LANGUAGE TypeFamilies, PatternSynonyms #-}

module NegationTrees where

import ExprTrees
import EvalTrees
import AsStringTrees
import Data.Void

-- | Negation

-- | Type instance for the original Expr type, now containing 
-- Neg as as its extension variant
type instance X_ExprExt WithNeg = Neg WithNeg

data WithNeg

-- | Data type for Neg, containing Neg for negation, and is also 
-- extensible in variants.
data Neg e = Neg (Expr e) | NegExt (X_NegExt e)

-- | The extension field of Neg is defined through a type family
type family X_NegExt e

-- | Type instance for the composition of Neg that is not extended
type instance X_NegExt WithNeg = Void

 -- | Useful pattern synonym
pattern Neg_WithNeg :: Expr WithNeg -> Expr WithNeg
pattern Neg_WithNeg e <- ExprExt (Neg e)
    where Neg_WithNeg e = ExprExt (Neg e)
          
-- | Evaluation of Neg          
evalNeg :: (X_ExprExt e ~ Neg e) 
    => (X_NegExt e -> Int) -> Neg e -> Int
evalNeg f (Neg e) = (-1) * eval (evalNeg f) e
evalNeg f (NegExt e) = f e

-- | Evaluation of the composition of Expr where it is extended with Neg,
-- where Neg has no extensions
evalWithNeg :: Expr WithNeg -> Int
evalWithNeg e = eval (evalNeg absurd) e

-- | asString of Neg
asStringNeg :: (X_ExprExt e ~ Neg e) 
    => (X_NegExt e -> String) -> Neg e -> String
asStringNeg f (Neg e) = "(-" ++ asString (asStringNeg f) e ++ ")"
asStringNeg f (NegExt e) = f e

-- | asString of the composition of Expr where it is extended with Neg,
-- where Neg has no extensions
asStringWithNeg :: Expr WithNeg -> String
asStringWithNeg e = asString (asStringNeg absurd) e