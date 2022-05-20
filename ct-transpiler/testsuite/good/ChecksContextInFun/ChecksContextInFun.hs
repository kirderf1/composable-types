{-# LANGUAGE ComposableTypes #-}

module ChecksContextInFun where

piececategory A

data piece A ==> B = B

c -: A -> ()

d :: (A ==> a, B partof a, a with c, Show e) => a -> e -> ((), a)
d a _ = (c a, B)
