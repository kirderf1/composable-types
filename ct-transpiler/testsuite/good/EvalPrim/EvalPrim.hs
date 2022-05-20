{-# LANGUAGE ComposableTypes #-}

module EvalPrim where

piececategory A

eval -: A -> Bool -> String

eval2 :: (a with eval) => a -> String
eval2 a = eval a False
