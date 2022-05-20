{-# LANGUAGE ComposableTypes #-}

module CompInFunctor where 

piececategory A

data piece A ==> B = B Int

-- TODO While there is currently no ambiguity here as the category can't be "Maybe A", should we really be allowed to write like this? Should "==>" really bind to "A" stronger than "Maybe"?
e :: Maybe A ==> (B)
e = undefined
