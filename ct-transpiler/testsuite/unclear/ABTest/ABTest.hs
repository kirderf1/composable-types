{-# LANGUAGE ComposableTypes #-}

module ABTest where

piececategory A
piececategory B

-- TODO What should happen if a piece has a parameter for a different category?
data piece A ==> A = A A B
data piece A ==> Ab = Ab Bool B

data piece B ==> B = B Bool | Bi Int

type CombinedA = A ==> (A | Ab)

