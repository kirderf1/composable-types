{-# LANGUAGE ComposableTypes #-}

module Init (Expr, Piece(Cons)) where

piececategory Expr

data piece Expr ==> Piece = Cons Int

