module RenderTrees where

import ExprTrees
import Data.Void

-- | Rendering function for Expr
render :: (X_ExprExt e -> String) -> Expr e -> String
render _ (Const _ i) = show i
render f (Add _ e1 e2) = "(" ++ render f e1 ++ " + " ++ render f e2 ++ ")"
render f (Mul _ e1 e2) = "(" ++ render f e1 ++ " * " ++ render f e2 ++ ")"
render f (ExprExt e) = f e
          
-- | Rendering of the undecorated version of Expr
renderUD :: Expr UD -> String
renderUD = render absurd
