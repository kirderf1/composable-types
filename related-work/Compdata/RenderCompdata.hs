{-# LANGUAGE TemplateHaskell #-}

module RenderCompdata where

import ExprCompdata

import Data.Comp hiding (Const)
import Data.Comp.Derive

-- | Rendering function, written without an algebra
class Render f where
  render' :: Render g => f (Term g) -> String

-- | Unpack the Term and call render'
render :: (Render f) => Term f -> String
render = render' . unTerm 

-- | Render instance for constants
instance Render Const where
  render' (Const i) = show i

-- | Render instance for operations
instance Render Op where
  render' (Add e1 e2) = "(" ++ render e1 ++ " + " ++ render e2 ++ ")"
  render' (Mul e1 e2) = "(" ++ render e1 ++ " * " ++ render e2 ++ ")"

-- | Derive Render instance for coproduct using Template Haskell
$(derive [liftSum] [''Render])
