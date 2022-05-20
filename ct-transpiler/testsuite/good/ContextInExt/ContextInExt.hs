{-# LANGUAGE ComposableTypes #-}

module ContextInExt where

piececategory A

data piece A ==> B = C A

d -: A -> ()

ext () => d for B where
    d (C a) = d a

