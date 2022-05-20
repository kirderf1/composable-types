{-# LANGUAGE ComposableTypes #-}

module MaybeE where

piececategory A

data piece A ==> B = B Int
data piece A ==> C = C
data piece A ==> D = D A

e :: Int -> Maybe (A ==> (B | C | D))
e = undefined
