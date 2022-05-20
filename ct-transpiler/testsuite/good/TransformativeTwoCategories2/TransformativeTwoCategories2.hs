{-# LANGUAGE ComposableTypes #-}

module TransforativeTwoCategories2 where

piececategory Expr

data piece Expr ==> Value = Const Int
data piece Expr ==> Op = Add Expr Expr | Mult Expr Expr 
data piece Expr ==> PairOp = Fst Expr Expr | Snd Expr Expr | Double Expr

type Sig = Expr ==> (Value | Op | PairOp)

piececategory Text

data piece Text ==> T = T String
data piece Text ==> T2 = T2 Text String Text
data piece Text ==> T3 = T3 Text

transform -: (Text ==> a) => Expr -> a

ext (T partof a) => transform @a for Value where
    transform (Const c) = T $ show c
    
ext (T2 partof a) => transform @a for Op where
    transform (Add e1 e2) = T2 (transform e1) "+" (transform e2)
    transform (Mult e1 e2) = T2 (transform e1) "*" (transform e2)

ext (T3 partof a, T2 partof a) => transform @a for PairOp where
    transform (Fst e1 e2) = T3 (transform e1)
    transform (Snd e1 e2) = T3 (transform e2)
    transform (Double e)  = T2 (transform e) "+" (transform e)
