{-# LANGUAGE ComposableTypes #-}

module PatternMatching1 where

piececategory A

data piece A ==> B = B Int
data piece A ==> C = C

d :: A ==> B -> Int
d (B i) = i

e :: A ==> (B | C) -> Int
e (B i) = i
e C     = 0
