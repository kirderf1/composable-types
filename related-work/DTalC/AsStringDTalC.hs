{-# LANGUAGE TypeOperators #-}

module AsStringDTalC where

import DTalC
import ExprDTalC

-- | AsString function, written without an algebra
class AsString f where
    asString' :: AsString g => f (Term g) -> String

-- | Unpack a Term and call asString'
asString :: AsString f => Term f -> String
asString (In t) = asString' t

-- | AsString instance for constants
instance AsString Const where
    asString' (Const i) = show i

-- | AsString instance for operations
instance AsString Op where
    asString' (Add e1 e2) = "(" ++ asString e1 ++ " + " 
                                ++ asString e2 ++ ")"
    asString' (Mul e1 e2) = "(" ++ asString e1 ++ " * " 
                                ++ asString e2 ++ ")"

-- | AsString instance for coproduct
instance (AsString f, AsString g) => AsString (f :+: g) where
    asString' (Inl a) = asString' a
    asString' (Inr b) = asString' b
