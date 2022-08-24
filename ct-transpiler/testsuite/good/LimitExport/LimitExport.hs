{-# LANGUAGE ComposableTypes #-}

module LimitExport (Expr, Piece(Cons)) where

piececategory Expr

data piece Expr ==> Piece = Cons Int

cons = Cons 0
