{-# LANGUAGE ComposableTypes #-}

module CreateRecordByApp where

piececategory Expr

data piece Expr ==> Const = Const
  { name :: String
  , value :: Int
  , good :: Bool
  }

newConst :: Expr ==> (Const)
newConst = Const "x" 2 True
