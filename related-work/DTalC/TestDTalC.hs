module TestDTalC where

import DTalC
import ExprDTalC
import EvalDTalC
import RenderDTalC
import NegationDTalC
import DesugDTalC

addExample :: Expr 
addExample = In (Inr (Add (In (Inl (Const 3)))
                              (In (Inl (Const 5)))))
                                                          
type Expr' = Term (Const :+: Op :+: Neg)      

negExample :: Expr' 
negExample = In (Inr (Inr (Neg (In (Inl (Const 5))))))

negAddExample :: Expr' 
negAddExample = In (Inr (Inl (Add (In (Inl (Const 3)))
                                  (In (Inr (Inr (Neg (In (Inl (Const 5))))))))))
    
negAddExample' :: Expr'
negAddExample' = iConst 3 `iAdd` (iNeg (iConst 5))
