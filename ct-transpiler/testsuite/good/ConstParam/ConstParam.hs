{-# LANGUAGE ComposableTypes #-}

module ConstParam where

piececategory Expr

data piece Expr ==> Const a = Const a

type Consts = Expr ==> (Const () | Const String)

isUnit -: Expr -> Bool

ext isUnit for Const () where
    isUnit (Const ()) = True

ext isUnit for Const String where
    isUnit (Const str) = False

test :: Consts
test = Const "test"

main = isUnit test
