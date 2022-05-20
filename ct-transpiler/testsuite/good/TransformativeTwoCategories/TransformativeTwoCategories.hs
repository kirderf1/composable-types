{-# LANGUAGE ComposableTypes #-}

module TransforativeTwoCategories where

piececategory Expr

data piece Expr ==> Value = Const Int 
data piece Expr ==> Op = Add Expr Expr | Mult Expr Expr 

type Sig = Expr ==> (Value | Op)

piececategory Text

data piece Text ==> T = T String
data piece Text ==> T2 = T2 Text String Text

transform -: (Text ==> a) => Expr -> a

ext (T partof a) => transform @a for Value where
    transform (Const c) = T $ show c
    
ext (T2 partof a) => transform @a for Op where
    transform (Add e1 e2) = T2 (transform e1) "+" (transform e2)
    transform (Mult e1 e2) = T2 (transform e1) "*" (transform e2)

