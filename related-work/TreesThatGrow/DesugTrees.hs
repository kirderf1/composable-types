{-# LANGUAGE TypeFamilies #-}

module DesugTrees where

import ExprTrees
import NegationTrees
import Data.Void

-- | Desugaring function for Expr
desug :: (X_ExprExt e -> Expr UD) -> Expr e -> Expr UD
desug _ (Const i) = Const i
desug f (Add e1 e2) = Add (desug f e1) (desug f e2)
desug f (Mul e1 e2) = Mul (desug f e1) (desug f e2)
desug f (ExprExt e) = f e

-- | Desugaring of the undecorated composition of Expr
desugUD :: Expr UD -> Expr UD
desugUD = desug absurd

-- | Desugaring of Sug
desugSug :: (X_ExprExt e ~ Sug e) 
    => (X_SugExt e -> Expr UD) -> Sug e -> Expr UD
desugSug f (Neg e) = Mul (Const (-1)) (desug (desugSug f) e)
desugSug f (SugExt e) = f e

-- | Desugaring of the composition of Expr where it is extended 
-- with Sug, where Sug has no extensions
desugWithNeg :: Expr WithNeg -> Expr UD
desugWithNeg e = desug (desugSug absurd) e
