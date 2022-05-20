{-# LANGUAGE ComposableTypes #-}
module PieceConst where 

piececategory Expr

data piece Expr ==> Value = Const Int
