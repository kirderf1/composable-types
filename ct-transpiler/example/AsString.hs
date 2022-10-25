{-# LANGUAGE ComposableTypes #-}

module AsString where

import Expr

-- | AsString function
asString -: ExprCat -> String

-- | AsString extension for constants
ext asString for Const where
    asString (Const i) = show i
    
-- | AsString extension for operations
ext asString for Op where
    asString (Add e1 e2) = "(" ++ asString e1 ++ " + " ++  asString e2 ++ ")"
    asString (Mul e1 e2) = "(" ++ asString e1 ++ " * " ++  asString e2 ++ ")"