{-# LANGUAGE ComposableTypes #-}

module B where

import A

data piece Expr ==> Value = Const Int

ext eval for Value where 
    eval (Const i) = i
