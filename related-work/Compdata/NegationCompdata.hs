{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, UndecidableInstances,
  ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module NegationDTalC where

import EvalCompdata 
import RenderCompdata

import Data.Comp 
import Data.Comp.Show ()
import Data.Comp.Derive
import Data.Comp.Equality

-- | Data type for negation
data Neg a = Neg a
    deriving Functor 
    
$(derive [makeTraversable, makeFoldable,
          makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''Neg])

-- | Instance of the eval function
instance Eval Neg where
    evalAlg (Neg a) = (-1) * a
    
-- | Instance of the render function
instance Render Neg where
    render (Neg e) = "(-" ++ pretty e ++ ")"
