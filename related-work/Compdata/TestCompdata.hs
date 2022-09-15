-- Example: evalEx = iConst 5
evalEx :: Term Value
evalEx = eval (iConst 1 `iAdd` (iConst 2 `iMult` iConst 2) :: Term Sig)
