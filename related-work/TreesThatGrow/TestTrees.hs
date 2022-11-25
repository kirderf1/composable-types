module Main where

import ExprTrees
import EvalTrees
import AsStringTrees
import NegationTrees
import DesugTrees

-- | Examples of type Expr, containing constants, addition and 
-- multiplication
threePlusFive :: Expr UD
threePlusFive = Add (Const 3) (Const 5)

twoMulThreePlusFive :: Expr UD
twoMulThreePlusFive = Mul (Const 2) threePlusFive

-- | Example with negation
threePlusNegFive :: Expr WithNeg
threePlusNegFive = Const 3 `Add` 
                        (NegP (Const 5))

-- | Evaluation examples
evalAddMul = evalUD twoMulThreePlusFive

evalAddNeg = evalWithNeg threePlusNegFive

-- | AsString example
asStringAddNeg = asStringWithNeg threePlusNegFive

-- | Desugar example
desugAddNeg = asStringUD (desugWithNeg threePlusNegFive)

-- | Main, printing results of above examples
main :: IO ()
main = do
    putStrLn "Evaluation examples:"
    print evalAddMul
    print evalAddNeg
    putStrLn "AsString example:"
    putStrLn asStringAddNeg
    putStrLn "Desugar example:"
    putStrLn desugAddNeg
