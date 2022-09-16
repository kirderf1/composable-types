{-# LANGUAGE TypeFamilies #-}

module DesugTrees where

import TreesThatGrow
import ExprTrees
import NegationTrees
import Data.Void

-- | Desugaring function for Expr
desug :: (X_ExprExt e -> Expr UD) -> Expr e -> Expr UD
desug f (ExprExt e) = f e
desug _ (Const _ i) = Const void i
desug f (Add _ e1 e2) = Add void (desug f e1) (desug f e2)
desug f (Mul _ e1 e2) = Mul void (desug f e1) (desug f e2)

-- | Desugaring of the undecorated version of Expr
desugUD :: Expr UD -> Expr UD
desugUD = desug absurd

-- | Desugaring of Sug
desugSug :: (X_ExprExt e ~ Sug e) => (X_SugExt e -> Expr UD) -> Sug e -> Expr UD
desugSug f (Neg _ e) = Mul void (Const void (-1)) (desug (desugSug f) e)
desugSug f (SugExt e) = f e

-- | Desugaring of the version of Expr where it is extended with Sug, where Sug has no extensions
desugS :: Expr S -> Expr UD
desugS e = desug (desugSug absurd) e
