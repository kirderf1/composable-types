{-# LANGUAGE TemplateHaskell #-}

module EvalCompdata where

import ExprCompdata

import Data.Comp 
import Data.Comp.Derive

-- | Term evaluation algebra
class Eval f where
  evalAlg :: Alg f Int

-- | Lift the evaluation algebra to a catamorphism
eval :: (Functor f, Eval f) => Term f -> Int
eval = cata evalAlg

-- | Eval instance for constants
instance Eval Value where
  evalAlg (Const i) = i

-- | Eval instance for operations
instance Eval Op where
  evalAlg (Add e1 e2) = e1 + e2
  evalAlg (Mul e1 e2) = e1 * e2

-- | Derive Eval instance for coproduct using Template Haskell
$(derive [liftSum] [''Eval])