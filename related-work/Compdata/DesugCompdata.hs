{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses, 
    FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module DesugCompdata where

import ExprCompdata
import NegationCompdata

import Data.Comp
import Data.Comp.Derive

-- | Transformative function desug using algebra
class Desug f v where
  desugAlg :: Alg f (Term v)

-- | Lift the desugaring algebra to a catamorphism
desug :: (Functor f, Desug f v) => Term f -> Term v
desug = cata desugAlg

-- | Default instance of Desug
instance {-# OVERLAPPABLE #-} (f :<: v) => Desug f v where
    desugAlg = inject 

-- | Desug instance for negation 
instance {-# OVERLAPPABLE #-} (Value :<: v, Op :<: v) => Desug Neg v where
    desugAlg (Neg e) = iConst (-1) `iMul` e

-- | Derive Desug instance for coproduct using Template Haskell 
$(derive [liftSum] [''Desug])
