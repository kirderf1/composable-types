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




-- | Sugar

type instance X_Const S = Void
type instance X_Add S = Void
type instance X_Mul S = Void
type instance X_ExprExt S = Sug'



type Expr_S = Expr S

data S

type Sug' = Sug Expr_UD

data Sug e = Neg (X_Neg e) (Sug e) | NegExt (X_NegExt e)

type family X_Neg e
type family X_NegExt e


pattern Sug_S :: Sug' -> Expr_S
pattern Sug_S x = ExprExt x

pattern Const_S :: Int -> Expr_S
pattern Const_S i <- Const _ i
    where Const_S i = Const void i
          
{-pattern Neg_S :: Expr_S -> Expr_S
pattern Neg_S e <- Neg _ e
    where Neg_S e = Neg void e -}         
          
--type Sug_S = Sug' S

-- pattern Neg_S :: Sug_S -> Sug_S
-- pattern Neg_S e <- Neg _ e
--     where Neg_S e = Neg void e

-- neg example

-- negEx :: Expr_S
-- negEx = Neg_S (Const_S 5)
