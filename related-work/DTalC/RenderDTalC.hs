{-# LANGUAGE TypeOperators #-}

module RenderDTalC where

import DTalC
import ExprDTalC

-- | Render function, written without an algebra
class Render f where
    render :: Render g => f (Term g) -> String

-- | Unpack a Term and call render
pretty :: Render f => Term f -> String
pretty (In t) = render t

-- | Render instance for constants
instance Render Const where
    render (Const i) = show i

-- | Render instance for operations
instance Render Op where
    render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
    render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"

-- | Render instance for coproduct
instance (Render f, Render g) => Render (f :+: g) where
    render (Inl x) = render x
    render (Inr y) = render y
