{-# LANGUAGE ComposableTypes #-}

module CreateRecord where

piececategory Expr

data piece Expr ==> Const = Const
  { name :: String
  , value :: Int
  , good :: Bool
  }

newConst :: Expr ==> Const
newConst = Const {value = 2, name = "x", good = True}
