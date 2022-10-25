{-# LANGUAGE ComposableTypes #-}

module Main where

import Expr
import Eval
import AsString
import Negation
import Desug

-- | Examples of type Expr, containing constants, addition and multiplication
threePlusFive :: Expr
threePlusFive = Add (Const 3) (Const 5)

twoMulThreePlusFive :: Expr
twoMulThreePlusFive = Mul (Const 2) threePlusFive
                      
-- | Composed type ExprWithNeg, also containing negation
type ExprWithNeg = ExprCat ==> (Const | Op | Neg)

-- | Example with negation
threePlusNegFive :: ExprWithNeg
threePlusNegFive = Const 3 `Add` (Neg (Const 5))

-- | Evaluation examples
evalAddMul = eval twoMulThreePlusFive

evalAddNeg = eval threePlusNegFive

-- | AsString example
asStringAddNeg = asString threePlusNegFive

-- | Desugar example
desugAddNeg = asString (desug threePlusNegFive :: Expr)

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