{-# LANGUAGE TemplateHaskell, FlexibleContexts, DeriveFunctor #-}

module NegationCompdata where

import EvalCompdata 
import AsStringCompdata

import Data.Comp 
import Data.Comp.Derive

-- | Data type for negation
data Neg a = Neg a
    deriving Functor 
    
-- | Derivation of smart constructor etc using Template Haskell
$(derive [makeEqF, makeShowF, smartConstructors]
         [''Neg])

-- | Eval instance for negation
instance Eval Neg where
    evalAlg (Neg e) = (-1) * e
    
-- | AsString instance for negation
instance AsString Neg where
    asString' (Neg e) = "(-" ++ asString e ++ ")"
