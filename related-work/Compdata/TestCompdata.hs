{-# LANGUAGE TypeOperators #-}

module Main where

import ExprCompdata
import EvalCompdata
import AsStringCompdata
import NegationCompdata
import DesugCompdata

import Data.Comp hiding (Const)

-- | Examples of type Expr, containing constants, addition and 
-- multiplication
threePlusFive :: Expr
threePlusFive = iAdd (iConst 3) (iConst 5)

twoMulThreePlusFive :: Expr
twoMulThreePlusFive = iMul (iConst 2) threePlusFive
                      
-- | Composed type ExprWithNeg, also containing negation
type ExprWithNeg = Term (Const :+: Op :+: Neg)

-- | Example with negation
threePlusNegFive :: ExprWithNeg
threePlusNegFive = iConst 3 `iAdd` (iNeg (iConst 5))

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
