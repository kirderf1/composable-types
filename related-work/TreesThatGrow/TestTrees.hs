module TestTrees where

import TreesThatGrow
import ExprTrees
import EvalTrees
import RenderTrees
import NegationTrees
import DesugTrees

-- example
incLitX :: Expr UD -> Expr UD
incLitX (Const_UD i) = Const_UD (i + 1) 
incLitX e = e

ex :: Expr UD
ex = Const_UD 5 `Add_UD` Const_UD 3 `Add_UD` Const_UD 3


ex_withoutPattern :: Expr UD
ex_withoutPattern = Add void (Const void 5) (Const void 3)


-- neg example

negEx :: Expr S
negEx = ExprExt (Neg_S (Const_S 5))
