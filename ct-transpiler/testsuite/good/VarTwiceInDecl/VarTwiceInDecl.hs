{-# LANGUAGE ComposableTypes #-}

module VarTwiceInDecl where

piececategory A

data piece A ==> B = C

d -: A -> (e, e)

