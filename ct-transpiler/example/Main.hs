{-# LANGUAGE ComposableTypes #-}

module Main where

import Expr
import Eval
import AsString
import Negation
import Desug

-- | Examples of type ExprComp, containing constants, addition and 
-- multiplication
threePlusFive :: ExprComp
threePlusFive = Add (Const 3) (Const 5)

twoMulThreePlusFive :: ExprComp
twoMulThreePlusFive = Mul (Const 2) threePlusFive
                      
-- | Composed type ExprCompWithNeg, also containing negation
type ExprCompWithNeg = ExprCat ==> (Const | Op | Neg)

-- | Example with negation
threePlusNegFive :: ExprCompWithNeg
threePlusNegFive = Const 3 `Add` (Neg (Const 5))

-- | Evaluation examples
evalAddMul = eval twoMulThreePlusFive

evalAddNeg = eval threePlusNegFive

-- | AsString example
asStringAddNeg = asString threePlusNegFive

-- | Desugar example
desugAddNeg = asString (desug threePlusNegFive :: ExprComp)

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
