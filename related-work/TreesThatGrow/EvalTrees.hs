module EvalTrees where

import ExprTrees
import Data.Void

-- | Evaluation function for Expr
eval :: (X_ExprExt e -> Int) -> Expr e -> Int
eval _ (Const _ i) = i
eval f (Add _ e1 e2) = eval f e1 + eval f e2
eval f (Mul _ e1 e2) = eval f e1 * eval f e2
eval f (ExprExt e) = f e

-- | Evaluation of the undecorated version of Expr
evalUD :: Expr UD -> Int
evalUD = eval absurd
