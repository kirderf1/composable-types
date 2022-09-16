{-# LANGUAGE TemplateHaskell #-}

module EvalCompdata where

import ExprCompdata

import Data.Comp 
import Data.Comp.Derive

-- Term evaluation algebra
class Eval f where
  evalAlg :: Alg f Int

-- | Instances for constants and operations
instance Eval Value where
  evalAlg (Const i) = i

instance Eval Op where
  evalAlg (Add x y) = x + y
  evalAlg (Mul x y) = x * y

-- | Derive instance for coproduct using Template Haskell
$(derive [liftSum] [''Eval])

-- | Lift the evaluation algebra to a catamorphism
eval :: (Functor f, Eval f) => Term f -> Int
eval = cata evalAlg
