{-# LANGUAGE ComposableTypes #-}

module Ambiguity where

piececategory A

data A = B Bool

-- TODO Should categories really be referred to in the same way as types, without being an actual type?
-- Check for namespace clashes in our transform!
data piece A ==> C = D | E A

