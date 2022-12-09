module AsStringVariations where

import VariationsOnVariants
import ExprVariations

-- | asString of constants
asStringConst :: Const e -> r -> String
asStringConst (Const i) r = show i

-- | asString of operations
asStringOp :: Op e -> (e -> String) -> String
asStringOp (Add e1 e2) r = "(" ++ r e1 ++ " + " ++ r e2 ++ ")"
asStringOp (Mul e1 e2) r = "(" ++ r e1 ++ " * " ++ r e2 ++ ")"

-- | asString function that can handle expressions containing 
-- constants and operations
asString :: Expr -> String
asString = cases (asStringConst ? asStringOp)
