{-# LANGUAGE ComposableTypes #-}

module Desug where

import Expr
import Negation

-- | Transformative function desug
desug -: (ExprCat ==> e) => ExprCat -> e

-- | Desug extension for constants
ext (Const partof a) => desug @a for Const where
    desug (Const i) = Const i

-- | Desug extension for operations
ext (Op partof a) => desug @a for Op where
    desug (Add e1 e2) = Add (desug e1) (desug e2)
    desug (Mul e1 e2) = Mul (desug e1) (desug e2)

-- | Desug extension for negation
ext (Const partof a, Op partof a) => desug @a for Neg where
    desug (Neg e) = Const (-1) `Mul` (desug e)
