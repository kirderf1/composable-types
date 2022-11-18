module AsStringTrees where

import ExprTrees
import Data.Void

-- | Function asString for Expr
asString :: (X_ExprExt e -> String) -> Expr e -> String
asString _ (Const i) = show i
asString f (Add e1 e2) = "(" ++ asString f e1 ++ " + " 
                             ++ asString f e2 ++ ")"
asString f (Mul e1 e2) = "(" ++ asString f e1 ++ " * " 
                             ++ asString f e2 ++ ")"
asString f (ExprExt e) = f e
          
-- | asString of the undecorated composition of Expr
asStringUD :: Expr UD -> String
asStringUD = asString absurd
