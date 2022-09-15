{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, UndecidableInstances,
  ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module ExprCompdata where

import Data.Comp 
import Data.Comp.Show ()
import Data.Comp.Derive
import Data.Comp.Equality

-- | Data type for expression language

-- | Data type variants for Const, Add and Mul
data Value a = Const Int
    deriving Functor

data Op a = Add a a | Mul a a 
    deriving Functor

-- | Composed type Expr
type Expr = Term (Value :+: Op)
    
$(derive [makeTraversable, makeFoldable,
          makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''Value, ''Op])
