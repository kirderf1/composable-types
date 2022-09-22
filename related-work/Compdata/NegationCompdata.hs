{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module NegationCompdata where

import EvalCompdata 
import RenderCompdata

import Data.Comp 
import Data.Comp.Derive

-- | Data type for negation
data Neg a = Neg a
    deriving Functor 
    
-- | Derivation of smart constructor etc using Template Haskell
$(derive [makeTraversable, makeFoldable,
          makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''Neg])

-- | Eval instance for negation
instance Eval Neg where
    evalAlg (Neg e) = (-1) * e
    
-- | Render instance for negation
instance Render Neg where
    render' (Neg e) = "(-" ++ render e ++ ")"