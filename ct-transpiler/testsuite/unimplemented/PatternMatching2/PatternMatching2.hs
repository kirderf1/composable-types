{-# LANGUAGE ComposableTypes #-}

module PatternMatching2 where

piececategory A

data piece A ==> B = B Int
data piece A ==> C = C

d :: A ==> B -> Int
d a = case a of
        (B i) -> i

e :: A ==> (B | C) -> Int
e a = case a of
        (B i) -> i
        C     -> 0
