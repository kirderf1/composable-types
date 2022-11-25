{-# LANGUAGE TypeFamilies #-}

module DesugTrees where

import ExprTrees
import NegationTrees
import Data.Void

-- | Desugaring function for Expr
desug :: (X_ExprExt e1 -> Expr e2) -> Expr e1 -> Expr e2
desug _ (Const i) = Const i
desug f (Add e1 e2) = Add (desug f e1) (desug f e2)
desug f (Mul e1 e2) = Mul (desug f e1) (desug f e2)
desug f (ExprExt e) = f e

-- | Desugaring of the undecorated composition of Expr
desugUD :: Expr UD -> Expr UD
desugUD = desug absurd

-- | Desugaring of Neg
desugNeg :: (Expr e1 -> Expr e2)
    -> (X_NegExt e1 -> Expr e2) -> Neg e1 -> Expr e2
desugNeg g _ (Neg e) = Mul (Const (-1)) (g e)
desugNeg _ f (NegExt e) = f e
