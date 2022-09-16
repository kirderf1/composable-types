{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses, 
    FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module DesugCompdata where

import ExprCompdata
import NegationCompdata

import Data.Comp
import Data.Comp.Derive

-- Term desugating algebra
class Desug f v where
  desugAlg :: Alg f (Term v)

$(derive [liftSum] [''Desug])

-- Lift the desugaring algebra to a catamorphism
desug :: (Functor f, Desug f v) => Term f -> Term v
desug = cata desugAlg

instance {-# OVERLAPPABLE #-} (f :<: v) => Desug f v where
    desugAlg = inject -- default instance

instance {-# OVERLAPPABLE #-} (Value :<: v, Op :<: v) => Desug Neg v where
    desugAlg (Neg e) = iConst (-1) `iMul` e
