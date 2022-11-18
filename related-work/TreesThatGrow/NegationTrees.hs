{-# LANGUAGE TypeFamilies, PatternSynonyms #-}

module NegationTrees where

import ExprTrees
import EvalTrees
import AsStringTrees
import Data.Void

-- | Sugar

-- | Type instance for the original Expr type, now containing 
-- Sug as as its extension variant
type instance X_ExprExt WithNeg = Sug WithNeg

data WithNeg

-- | Data type for Sug, containing Neg for negation, and is also 
-- extensible in variants.
data Sug e = Neg (Expr e) | SugExt (X_SugExt e)

-- | The extension field of Sug is defined through a type family
type family X_SugExt e

-- | Type instance for the composition of Sug that is not extended
type instance X_SugExt WithNeg = Void

 -- | Useful pattern synonym
pattern Neg_WithNeg :: Expr WithNeg -> Expr WithNeg
pattern Neg_WithNeg e <- ExprExt (Neg e)
    where Neg_WithNeg e = ExprExt (Neg e)
          
-- | Evaluation of Sug          
evalSug :: (X_ExprExt e ~ Sug e) 
    => (X_SugExt e -> Int) -> Sug e -> Int
evalSug f (Neg e) = (-1) * eval (evalSug f) e
evalSug f (SugExt e) = f e

-- | Evaluation of the composition of Expr where it is extended with Sug,
-- where Sug has no extensions
evalWithNeg :: Expr WithNeg -> Int
evalWithNeg e = eval (evalSug absurd) e

-- | asString of Sug
asStringSug :: (X_ExprExt e ~ Sug e) 
    => (X_SugExt e -> String) -> Sug e -> String
asStringSug f (Neg e) = "(-" ++ asString (asStringSug f) e ++ ")"
asStringSug f (SugExt e) = f e

-- | asString of the composition of Expr where it is extended with Sug,
-- where Sug has no extensions
asStringWithNeg :: Expr WithNeg -> String
asStringWithNeg e = asString (asStringSug absurd) e