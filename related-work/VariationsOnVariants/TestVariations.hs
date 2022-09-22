{-# LANGUAGE TypeOperators #-}

module Main where

import VariationsOnVariants
import ExprVariations
import NegationVariations
import EvalVariations
import RenderVariations
import DesugVariations

-- | Examples of type Expr, containing constants, addition and multiplication
addExample :: Expr
addExample = iAdd (iConst 3) (iConst 5)

addMulExample :: Expr
addMulExample = iMul (iConst 2) addExample
                      
-- | Composed type Expr', also containing negation
type Expr' = Term (Const :+: Op :+: Neg)

-- | Evaluation function also considering negation
evalWithNeg :: Expr' -> Int
evalWithNeg = cases (evalNeg ? (evalConst ? evalOp))

-- | Rendering function also considering negation
renderWithNeg :: Expr' -> String
renderWithNeg = cases (renderNeg ? (renderConst ? renderOp))

-- | Example with negation
negAddExample :: Expr'
negAddExample = iConst 3 `iAdd` (iNeg (iConst 5))

-- | Evaluation examples
evalAddMul = eval addMulExample

evalNegAdd = evalWithNeg negAddExample

-- | Render example
renderNegAdd = renderWithNeg negAddExample

-- | Desugar example
desugNegAdd = render (desug negAddExample :: Expr)

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

