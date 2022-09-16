{-# LANGUAGE TemplateHaskell #-}

module RenderCompdata where

import ExprCompdata

import Data.Comp 
import Data.Comp.Derive

-- | Rendering function, written without an algebra
class Render f where
  render :: Render g => f (Term g) -> String

-- | Instances for constants and operations
instance Render Value where
  render (Const i) = show i

instance Render Op where
  render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
  render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"

-- | Derive instance for coproduct using Template Haskell
$(derive [liftSum] [''Render])

-- | Unpack the Term and call render
pretty :: (Render f) => Term f -> String
pretty = render . unTerm 
