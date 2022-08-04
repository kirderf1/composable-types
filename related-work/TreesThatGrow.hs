{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module TreesThatGrow where

import GHC.Types (Constraint)
import Data.Void

data Expr e = Const (X_Const e) Int | Add (X_Add e) (Expr e) (Expr e)
            | Mul (X_Mul e) (Expr e) (Expr e) | ExprExt (X_ExprExt e)
            
type family X_Const e
type family X_Add e
type family X_Mul e
type family X_ExprExt e

-- undecorated variant
type Expr_UD = Expr UD
data UD

type instance X_Const UD = Void
type instance X_Add UD = Void
type instance X_Mul UD = Void
type instance X_ExprExt UD = Void

-- void
void :: Void
void = error "Attempt to evaluate void"


-- patterns 
pattern Const_UD :: Int -> Expr_UD
pattern Const_UD i <- Const _ i
    where Const_UD i = Const void i
          
pattern Add_UD :: Expr_UD -> Expr_UD -> Expr_UD
pattern Add_UD e1 e2 <- Add _ e1 e2
    where Add_UD e1 e2 = Add void e1 e2
          
pattern Mul_UD :: Expr_UD -> Expr_UD -> Expr_UD
pattern Mul_UD e1 e2 <- Mul _ e1 e2
    where Mul_UD e1 e2 = Mul void e1 e2

-- evaluation 
eval :: (X_ExprExt e -> Int) -> Expr e -> Int
eval _ (Const _ i) = i
eval f (Add _ e1 e2) = eval f e1 + eval f e2
eval f (Mul _ e1 e2) = eval f e1 * eval f e2
eval f (ExprExt e) = f e
          
          
-- example
incLitX :: Expr_UD -> Expr_UD
incLitX (Const_UD i) = Const_UD (i + 1) 
incLitX e = e

ex :: Expr_UD
ex = Const_UD 5 `Add_UD` Const_UD 3 `Add_UD` Const_UD 3

evalUD :: Expr_UD -> Int
evalUD e = eval (\_ -> 0) e

ex_withoutPattern :: Expr_UD
ex_withoutPattern = Add void (Const void 5) (Const void 3)




-- | Sugar

type instance X_Const S = Void
type instance X_Add S = Void
type instance X_Mul S = Void
type instance X_ExprExt S = Sug'



type Expr_S = Expr S

data S

type Sug' = Sug S

data Sug e = Neg (X_Neg e) (Expr e) | SugExt (X_SugExt e)

type family X_Neg e
type family X_SugExt e

type instance X_Neg S = Void
type instance X_SugExt S = Void


pattern Sug_S :: Sug' -> Expr_S
pattern Sug_S x = ExprExt x

pattern Const_S :: Int -> Expr_S
pattern Const_S i <- Const _ i
    where Const_S i = Const void i
          
pattern Neg_S :: Expr_S -> Sug'
pattern Neg_S e <- Neg _ e
    where Neg_S e = Neg void e


-- neg example

negEx :: Expr_S
negEx = ExprExt (Neg_S (Const_S 5))

evalSug :: (X_SugExt S -> Int) -> X_ExprExt S -> Int
evalSug f (Neg_S e) = (-1) * eval (evalSug f) e
evalSug f (SugExt e) = f e

evalS :: Expr_S -> Int
evalS e = eval (evalSug (\_ -> 0)) e
