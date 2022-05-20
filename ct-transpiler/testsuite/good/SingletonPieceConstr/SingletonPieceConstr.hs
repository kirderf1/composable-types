{-# LANGUAGE ComposableTypes #-}

module SingletonPieceConstr where

piececategory A

data piece A ==> B = B

c :: A ==> B
c = B
