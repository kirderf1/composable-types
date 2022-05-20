{-# LANGUAGE ComposableTypes #-}

module AltTyVarInExt where

import Control.Applicative (Alternative, empty)

piececategory A

data piece A ==> B = C

d -: A -> e

ext (Alternative f) => d @(f g) for B where
    d C = empty

