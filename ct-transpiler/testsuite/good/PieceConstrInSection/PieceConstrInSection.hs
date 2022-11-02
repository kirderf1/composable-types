{-# LANGUAGE ComposableTypes #-}

module PieceConstrInSection where

piececategory A

data piece A ==> B = B Bool Bool

c :: A ==> (B)
c = (`B` True) False

d :: A ==> (B)
d = (True `B`) False
