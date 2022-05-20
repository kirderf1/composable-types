{-# LANGUAGE ComposableTypes #-}

module PatternMatching4 where

piececategory A

data piece A ==> B = B Int
data piece A ==> C = C

d :: (B partof a, C partof a) => a -> Int
d (B i) = i
d C     = 0
d _     = -1
