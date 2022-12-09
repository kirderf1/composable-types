{-# LANGUAGE ComposableTypes #-}

module Negation where

import Expr
import Eval
import AsString

-- | Data type piece for negation
data piece ExprCat ==> Neg = Neg ExprCat

-- | Eval extension for negation
ext eval for Neg where
    eval (Neg e) = (-1) * eval e

-- | AsString extension for negation
ext asString for Neg where
    asString (Neg e) = "(-" ++ asString e ++ ")"
