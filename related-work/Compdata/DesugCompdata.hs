{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses, 
    FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module DesugCompdata where

import ExprCompdata
import NegationCompdata

import Data.Comp
import Data.Comp.Derive

-- | Transformative function desug using algebra
class Desug f g where
  desugAlg :: Alg f (Term g)

-- | Lift the desugaring algebra to a catamorphism
desug :: (Functor f, Desug f g) => Term f -> Term g
desug = cata desugAlg

-- | Default instance of Desug
instance {-# OVERLAPPABLE #-} (f :<: g) => Desug f g where
    desugAlg = inject 

-- | Desug instance for negation 
instance {-# OVERLAPPABLE #-} (Value :<: g, Op :<: g) => Desug Neg g where
    desugAlg (Neg e) = iConst (-1) `iMul` e

-- | Derive Desug instance for coproduct using Template Haskell 
$(derive [liftSum] [''Desug])