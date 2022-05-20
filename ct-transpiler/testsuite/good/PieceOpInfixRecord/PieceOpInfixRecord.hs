{-# LANGUAGE ComposableTypes #-}

module PieceOpInfixRecord where

piececategory Expr

data piece Expr ==> Op = Expr `Add` Expr | Expr `Mult` Expr

data piece Expr ==> A = B
  { c :: Expr
  , d :: Int
  }
