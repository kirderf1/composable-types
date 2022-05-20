{-# LANGUAGE ComposableTypes #-}

module PatternMatching3 where

piececategory A

data piece A ==> B = B Int
data piece A ==> C = C

d :: B partof a => a -> Int
d (B i) = i
d _     = 0
