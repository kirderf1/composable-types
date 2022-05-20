{-# LANGUAGE ComposableTypes #-}

module ExpInStmt where

piececategory Stmt Expr
piececategory Expr

data piece Stmt Expr ==> Assign = Assign String Expr
data piece Stmt Expr ==> If = If Expr Stmt
data piece Expr ==> Literal = BoolLit Bool | StringLit String
data piece Expr ==> BoolOp = And Expr Expr | Or Expr Expr

type AExp = Expr ==> (Literal | BoolOp)
type AStmt = Stmt AExp ==> (Assign | If)

stringifyS -: Stmt e -> String
stringifyE -: Expr -> String

--instance (StringifyS stmt, StringifyE exp) => StringifyS (Assign exp stmt) where
ext e with stringifyE => stringifyS @e for Assign where
    stringifyS (Assign var exp) = var ++ " = " ++ stringifyE exp
ext e with stringifyE => stringifyS @e for If where
    stringifyS (If exp stmt) = "if (" ++ stringifyE exp ++ ") " ++ stringifyS stmt

ext stringifyE for Literal where
    stringifyE (BoolLit b) = show b
    stringifyE (StringLit s) = s

ext stringifyE for Literal where
    stringifyE (And left right) = stringifyE left ++ " && " ++ stringifyE right
    stringifyE (Or left right) = stringifyE left ++ " || " ++ stringifyE right

