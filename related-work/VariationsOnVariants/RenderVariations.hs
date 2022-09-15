module RenderVariations where

import VariationsOnVariants
import ExprVariations

-- TODO: Type signatures
-- | Rendering of constants
renderConst (Const i) r = show i

-- | Rendering of operations
renderOp (Add e1 e2) r = "(" ++ r e1 ++ " + " ++ r e2 ++ ")"
renderOp (Mul e1 e2) r = "(" ++ r e1 ++ " * " ++ r e2 ++ ")"

-- | Rendering function that can render expressions containing constants and operations
render :: Expr -> String
render = cases (renderConst ? renderOp)
