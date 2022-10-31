{-# LANGUAGE ComposableTypes #-}

module WrongCatInComp where

piececategory A
piececategory B

data piece A ==> P = P

p :: B ==> P
p = P
