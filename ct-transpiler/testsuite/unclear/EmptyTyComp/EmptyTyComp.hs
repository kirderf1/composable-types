{-# LANGUAGE ComposableTypes #-}

module EmptyTyComp where

piececategory A

-- TODO Should we have a more informative error message for this? Could there be a point in allowing an empty composed type?
type B = A ==> ()
