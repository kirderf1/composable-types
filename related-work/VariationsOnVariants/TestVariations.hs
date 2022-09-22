module TestVariations where

import VariationsOnVariants
import ExprVariations
import NegationVariations
import EvalVariations
import RenderVariations
import DesugVariations



exOp :: Expr
exOp = inj' (inj' (Const 1) `Add` inj' (Const 2)) 

exOp2 :: Expr
exOp2 = inj' (inj' (Const 3) `Mul` exOp)
