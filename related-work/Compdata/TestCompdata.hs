module TestCompdata where

import ExprCompdata
import EvalCompdata
import RenderCompdata
import NegationCompdata
import DesugCompdata

import Data.Comp 

-- Example: evalEx = iConst 5
evalEx :: Int
evalEx = eval (iConst 1 `iAdd` (iConst 2 `iMul` iConst 2) :: Expr)
