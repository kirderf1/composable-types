{-# LANGUAGE ComposableTypes #-}

module TyVarInExt where

piececategory Expr

data piece Expr ==> Const = Const Int

evalNum -: Num e => Expr -> e

ext evalNum @Int for Const where
    evalNum (Const i) = i
    
ext evalNum @Double for Const where
    evalNum (Const d) = fromIntegral d
