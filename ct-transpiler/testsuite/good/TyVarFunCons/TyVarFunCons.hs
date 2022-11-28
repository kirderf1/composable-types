{-# LANGUAGE ComposableTypes #-}

module TyVarFunCons where

piececategory A

data piece A ==> B = C

d -: A -> e

test :: a with d @Int => a -> Int
test a = d a + 1
