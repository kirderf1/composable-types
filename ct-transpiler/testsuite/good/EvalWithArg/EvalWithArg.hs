{-# LANGUAGE ComposableTypes #-}

module EvalWithArg where

piececategory A

data piece A ==> B = C Int | D A

eval -: A -> Bool -> Int

ext eval for B where
    eval (D a) True  = - eval a False
    eval (D a) False = eval a True
    eval (C i) _     = i
