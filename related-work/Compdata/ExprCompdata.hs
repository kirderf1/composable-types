{-# LANGUAGE TemplateHaskell, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module ExprCompdata where

import Data.Comp 
import Data.Comp.Derive

-- | Data type for expression language

-- | Data type variants for Const, Add and Mul
data Value a = Const Int
    deriving Functor

data Op a = Add a a | Mul a a 
    deriving Functor

-- | Composed type Expr
type Expr = Term (Value :+: Op)
    
-- | Derivation of smart constructors etc using Template Haskell
$(derive [makeTraversable, makeFoldable,
          makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''Value, ''Op])
