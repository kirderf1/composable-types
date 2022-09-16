{-# LANGUAGE TemplateHaskell #-}

module RenderCompdata where

import ExprCompdata

import Data.Comp 
import Data.Comp.Derive

-- Term rendering
class Render f where
  render :: Render g => f (Term g) -> String

$(derive [liftSum] [''Render])

pretty :: (Render f) => Term f -> String
pretty = render . unTerm 

instance Render Value where
  render (Const i) = show i

instance Render Op where
  render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
  render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"

