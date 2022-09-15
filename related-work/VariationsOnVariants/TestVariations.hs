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

data Double e = Double e
    
desugar e = cases ((\(Double e) r -> In (inj (Add (r e) (r e)))) ? (const . In . fmap desugar)) e
