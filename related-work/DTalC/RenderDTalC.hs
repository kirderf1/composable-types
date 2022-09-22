{-# LANGUAGE TypeOperators #-}

module RenderDTalC where

import DTalC
import ExprDTalC

-- | Render function, written without an algebra
class Render f where
    render' :: Render g => f (Term g) -> String

-- | Unpack a Term and call render'
render :: Render f => Term f -> String
render (In t) = render' t

-- | Render instance for constants
instance Render Const where
    render' (Const i) = show i

-- | Render instance for operations
instance Render Op where
    render' (Add e1 e2) = "(" ++ render e1 ++ " + " ++ render e2 ++ ")"
    render' (Mul e1 e2) = "(" ++ render e1 ++ " * " ++ render e2 ++ ")"

-- | Render instance for coproduct
instance (Render f, Render g) => Render (f :+: g) where
    render' (Inl a) = render' a
    render' (Inr b) = render' b