{-# LANGUAGE ComposableTypes #-}

module LimitExport (Expr, Piece(Cons)) where

piececategory Expr

data piece Expr ==> Piece = Cons Int

cons :: Piece partof e => e
cons = Cons 0
