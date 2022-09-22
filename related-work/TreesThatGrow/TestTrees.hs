module Main where

import TreesThatGrow
import ExprTrees
import EvalTrees
import RenderTrees
import NegationTrees
import DesugTrees

-- | Examples of type Expr, containing constants, addition and multiplication
addExample :: Expr UD
addExample = Add_UD (Const_UD 3) (Const_UD 5)

addMulExample :: Expr UD
addMulExample = Mul_UD (Const_UD 2) addExample

-- | Example with negation
negAddExample :: Expr S
negAddExample = Const_S 3 `Add_S` (Neg_S (Const_S 5))

-- | Evaluation examples
evalAddMul = evalUD addMulExample

evalNegAdd = evalS negAddExample

-- | Render example
renderNegAdd = renderS negAddExample

-- | Desugar example
desugNegAdd = renderUD (desugS negAddExample)

-- | Main, printing results of above examples
main :: IO ()
main = do
    putStrLn "Evaluation examples:"
    print evalAddMul
    print evalNegAdd
    putStrLn "Render example:"
    putStrLn renderNegAdd
    putStrLn "Desugar example:"
    putStrLn desugNegAdd

