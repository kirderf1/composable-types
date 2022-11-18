module EvalTrees where

import ExprTrees
import Data.Void

-- | Evaluation function for Expr
eval :: (X_ExprExt e -> Int) -> Expr e -> Int
eval _ (Const i) = i
eval f (Add e1 e2) = eval f e1 + eval f e2
eval f (Mul e1 e2) = eval f e1 * eval f e2
eval f (ExprExt e) = f e

-- | Evaluation of the undecorated composition of Expr
evalUD :: Expr UD -> Int
evalUD = eval absurd
