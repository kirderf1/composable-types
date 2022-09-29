{-# LANGUAGE TypeOperators #-}

module Main where

import ExprCompdata
import EvalCompdata
import RenderCompdata
import NegationCompdata
import DesugCompdata

import Data.Comp hiding (Const)

-- | Examples of type Expr, containing constants, addition and multiplication
addExample :: Expr
addExample = iAdd (iConst 3) (iConst 5)

addMulExample :: Expr
addMulExample = iMul (iConst 2) addExample
                      
-- | Composed type Expr', also containing negation
type Expr' = Term (Const :+: Op :+: Neg)

-- | Example with negation
negAddExample :: Expr'
negAddExample = iConst 3 `iAdd` (iNeg (iConst 5))

-- | Evaluation examples
evalAddMul = eval addMulExample

evalNegAdd = eval negAddExample

-- | Render example
renderNegAdd = render negAddExample

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
