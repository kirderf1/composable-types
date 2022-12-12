{-# LANGUAGE TypeFamilies #-}

module Main where

import ExprTrees
import EvalTrees
import AsStringTrees
import NegationTrees
import DesugTrees
import Data.Void

-- | Examples of type Expr, containing constants, addition and 
-- multiplication
threePlusFive :: Expr e
threePlusFive = Add (Const 3) (Const 5)

twoMulThreePlusFive :: Expr UD
twoMulThreePlusFive = Mul (Const 2) threePlusFive

-- | Composition of Expr and Neg

data WithNeg

-- | Type instance for the original Expr type, now containing 
-- Neg as as its extension variant
type instance X_ExprExt WithNeg = Neg WithNeg

-- | Type instance for the composition of Neg that is not extended
type instance X_NegExt WithNeg = Void

-- | Evaluation of the composition of Expr where it is extended with Neg,
-- where Neg has no extensions
evalWithNeg :: Expr WithNeg -> Int
evalWithNeg e = eval (evalNeg evalWithNeg absurd) e

-- | asString of the composition of Expr where it is extended with Neg,
-- where Neg has no extensions
asStringWithNeg :: Expr WithNeg -> String
asStringWithNeg e = asString (asStringNeg asStringWithNeg absurd) e

-- | Desugaring of the composition of Expr where it is extended 
-- with Neg, where Neg has no extensions
desugWithNeg :: Expr WithNeg -> Expr UD
desugWithNeg e = desug (desugNeg desugWithNeg absurd) e

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
