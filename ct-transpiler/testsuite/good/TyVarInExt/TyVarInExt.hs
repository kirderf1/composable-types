{-# LANGUAGE ComposableTypes #-}

module TyVarInExt where

piececategory A

data piece A ==> B = C

d -: A -> e

ext d @() for B where
    d C = ()

