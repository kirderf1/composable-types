{-# LANGUAGE ComposableTypes #-}

module Pieces where

piececategory A

data piece A ==> Aa = Aa1

data piece A ==> Ab = Ab1 A A | Ab2 A
