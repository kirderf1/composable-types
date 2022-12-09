{-# LANGUAGE TemplateHaskell #-}

module AsStringCompdata where

import ExprCompdata

import Data.Comp hiding (Const)
import Data.Comp.Derive

-- | Function asString, written without an algebra
class AsString f where
  asString' :: AsString g => f (Term g) -> String

-- | Unpack the Term and call asString'
asString :: (AsString f) => Term f -> String
asString = asString' . unTerm 

-- | AsString instance for constants
instance AsString Const where
  asString' (Const i) = show i

-- | AsString instance for operations
instance AsString Op where
  asString' (Add e1 e2) = "(" ++ asString e1 ++ " + " 
                              ++ asString e2 ++ ")"
  asString' (Mul e1 e2) = "(" ++ asString e1 ++ " * " 
                              ++ asString e2 ++ ")"

-- | Derive AsString instance for coproduct using Template Haskell
$(derive [liftSum] [''AsString])
