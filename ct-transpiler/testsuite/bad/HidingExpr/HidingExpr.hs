{-# LANGUAGE ComposableTypes #-}

module HidingExpr where

import Init hiding (Expr)

data piece Expr ==> Piece = Cons Int
