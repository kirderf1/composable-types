module EvalVariations where

import VariationsOnVariants
import ExprVariations

-- | Evaluation of constants
-- TODO: Type signatures
evalConst (Const i) r = i

-- | Evaluation of operations Add and Mul
evalOp (Add e1 e2) r = r e1 + r e2
evalOp (Mul e1 e2) r = r e1 * r e2

-- | Evaluation function that can evaluate expressions containing constants and operations
eval :: Expr -> Int
eval = cases (evalConst ? evalOp)
