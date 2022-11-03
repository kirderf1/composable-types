{-# LANGUAGE ComposableTypes #-}

module EvalNum where

piececategory ExprCat

data piece ExprCat ==> Const = Const Int

evalNum -: (Num e) => ExprCat -> e

ext evalNum @Int for Const where
        evalNum (Const i) = i

