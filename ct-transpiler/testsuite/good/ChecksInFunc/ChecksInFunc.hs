{-# LANGUAGE ComposableTypes #-}

module ChecksInFunc where

piececategory A

data piece A ==> B = B

c -: A -> ()

d :: (A ==> a, B partof a, a with c) => a -> ((), a)
d a = (c a, B)
