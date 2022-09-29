{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module NegationTrees where

import ExprTrees
import EvalTrees
import RenderTrees
import Data.Void

-- | Sugar

-- | Type instances for the original Expr type, now containing Sug as as its extension variant
type instance X_Const WithNeg = ()
type instance X_Add WithNeg = ()
type instance X_Mul WithNeg = ()
type instance X_ExprExt WithNeg = Sug WithNeg

data WithNeg

-- | Data type for Sug, containing Neg for negation, and is also extensible in fields and variants.
data Sug e = Neg (X_Neg e) (Expr e) | SugExt (X_SugExt e)

-- | The variants of Sug are defined through type families 
type family X_Neg e
type family X_SugExt e

-- | Type instances for the version where Sug is not extended
type instance X_Neg WithNeg = ()
type instance X_SugExt WithNeg = Void

 -- | Useful pattern synonyms 
pattern Const_WithNeg :: Int -> Expr WithNeg
pattern Const_WithNeg i <- Const _ i
    where Const_WithNeg i = Const () i

pattern Add_WithNeg :: Expr WithNeg -> Expr WithNeg -> Expr WithNeg
pattern Add_WithNeg e1 e2 <- Add _ e1 e2
    where Add_WithNeg e1 e2 = Add () e1 e2
            
pattern Mul_WithNeg :: Expr WithNeg -> Expr WithNeg -> Expr WithNeg
pattern Mul_WithNeg e1 e2 <- Mul _ e1 e2
    where Mul_WithNeg e1 e2 = Mul () e1 e2

pattern Neg_WithNeg :: Expr WithNeg -> Expr WithNeg
pattern Neg_WithNeg e <- ExprExt (Neg _ e)
    where Neg_WithNeg e = ExprExt (Neg () e)
          
-- | Evaluation of Sug          
evalSug :: (X_ExprExt e ~ Sug e) => (X_SugExt e -> Int) -> Sug e -> Int
evalSug f (Neg _ e) = (-1) * eval (evalSug f) e
evalSug f (SugExt e) = f e

-- | Evaluation of the version of Expr where it is extended with Sug, where Sug has no extensions
evalWithNeg :: Expr WithNeg -> Int
evalWithNeg e = eval (evalSug absurd) e

-- | Rendering of Sug
renderSug :: (X_ExprExt e ~ Sug e) => (X_SugExt e -> String) -> Sug e -> String
renderSug f (Neg _ e) = "(-" ++ render (renderSug f) e ++ ")"
renderSug f (SugExt e) = f e

-- | Rendering of the version of Expr where it is extended with Sug, where Sug has no extensions
renderWithNeg :: Expr WithNeg -> String
renderWithNeg e = render (renderSug absurd) e
