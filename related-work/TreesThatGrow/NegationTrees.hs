{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module NegationTrees where

import TreesThatGrow
import ExprTrees
import EvalTrees
import RenderTrees
import Data.Void

-- | Sugar

-- | Type instances for the original Expr type, now containing Sug as as its extension variant
type instance X_Const S = Void
type instance X_Add S = Void
type instance X_Mul S = Void
type instance X_ExprExt S = Sug S

data S

-- | Data type for Sug, containing Neg for negation, and is also extensible in fields and variants.
data Sug e = Neg (X_Neg e) (Expr e) | SugExt (X_SugExt e)

-- | The variants of Sug are defined through type families 
type family X_Neg e
type family X_SugExt e

-- | Type instances for the version where Sug is not extended
type instance X_Neg S = Void
type instance X_SugExt S = Void

 -- | Useful pattern synonyms 
pattern Sug_S :: Sug S -> Expr S
pattern Sug_S x = ExprExt x

pattern Const_S :: Int -> Expr S
pattern Const_S i <- Const _ i
    where Const_S i = Const void i

pattern Add_S :: Expr S -> Expr S -> Expr S
pattern Add_S e1 e2 <- Add _ e1 e2
    where Add_S e1 e2 = Add void e1 e2
            
pattern Mul_S :: Expr S -> Expr S -> Expr S
pattern Mul_S e1 e2 <- Mul _ e1 e2
    where Mul_S e1 e2 = Mul void e1 e2
          
pattern Neg_S :: Expr S -> Sug S
pattern Neg_S e <- Neg _ e
    where Neg_S e = Neg void e
          
-- | Evaluation of Sug          
evalSug :: (X_ExprExt e ~ Sug e) => (X_SugExt e -> Int) -> Sug e -> Int
evalSug f (Neg _ e) = (-1) * eval (evalSug f) e
evalSug f (SugExt e) = f e

-- | Evaluation of the version of Expr where it is extended with Sug, where Sug has no extensions
evalS :: Expr S -> Int
evalS e = eval (evalSug absurd) e

-- | Rendering of Sug
renderSug :: (X_ExprExt e ~ Sug e) => (X_SugExt e -> String) -> Sug e -> String
renderSug f (Neg _ e) = "(-" ++ render (renderSug f) e ++ ")"
renderSug f (SugExt e) = f e

-- | Rendering of the version of Expr where it is extended with Sug, where Sug has no extensions
renderS :: Expr S -> String
renderS e = render (renderSug absurd) e
