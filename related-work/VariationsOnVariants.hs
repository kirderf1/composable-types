{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module VariationsOnVariants where


-- | Below definitions are from:  
-- J Garrett Morris. Variations on Variants. ACM SIGPLAN Notices, 50(12):71–81, 2015

data Term e = In (e (Term e))
data (f :+: g) e = Inl (f e) | Inr (g e)

(<?>) :: (f e -> a) -> (g e -> a) -> (f :+: g) e -> a
(f <?> g) (Inl x) = f x
(f <?> g) (Inr x) = g x



data Yep
data Nope

type family IsIn f g where
    IsIn f f = Yep
    IsIn f (g :+: h) = Or (IsIn f g) (IsIn f h)
    IsIn f g = Nope

type family Or b c where
    Or Nope Nope = Nope
    Or b c = Yep

data Refl
data L x
data R x

type family Into f g where
    Into f f = Refl
    Into f (g :+: h) = Ifi (Into f g) (IsIn f h) (Into f h) (IsIn f g)
    Into f g = Nope
    
type family Ifi lp inr rp inl where
    Ifi Nope inr Nope inl = Nope
    Ifi Nope inr rp Nope = R rp
    Ifi lp Nope rp inl = L lp
    Ifi lp inr rp inl = Nope
    
    
class Inj f g p where
    injp :: p -> f e -> g e
    
instance Inj f f Refl where
    injp _ = id
    
instance Inj f g p => Inj f (g :+: h) (L p) where
    injp (_ :: L p) = Inl . injp (undefined :: p)
    
instance Inj f h p => Inj f (g :+: h) (R p) where
    injp (_ :: R p) = Inr . injp (undefined :: p)
    
inj :: forall f g e. (Inj f g (Into f g)) => f e -> g e
inj = injp (undefined :: Into f g)


data Onl (h :: * -> *)
data Onr (h :: * -> *)
data Le (g :: * -> *) p
data Ri (f :: * -> *) p

type family Minus f g where
    Minus f f = Nope
    Minus (f :+: g) f = Onl g
    Minus (f :+: g) g = Onr f
    Minus (f :+: g) h = Ifm g (Minus f h) (IsIn f g)
                            f (Minus g h) (IsIn f h)
--    Minus f g = Found f
    
type family Ifm g lp inr f rp inl where
    Ifm g Nope inr f Nope inl = Nope
    Ifm g Nope inr f rp Nope = Ri f rp
    Ifm g lp Nope f rp inl = Le g lp
    
type family OutOf p where
    OutOf (Onl x) = x
    OutOf (Onr x) = x
    OutOf (Le f p) = OutOf p :+: f
    OutOf (Ri f p) = f :+: OutOf p
    
    
    
class Without f g p where
    (??) :: (g e -> r) -> (OutOf p e -> r) -> p -> f e -> r
    
instance Without (f :+: g) f (Onl g) where
    (m ?? n) _ = m <?> n
    
instance Without (f :+: g) g (Onr f) where
    (m ?? n) _ = n <?> m
    
instance Without f h p => Without (f :+: g) h (Le g p) where
    (m ?? n) (_ :: Le g p) = (m ?? (n . Inl)) (undefined :: p) <?> (n . Inr)
    
instance Without g h p => Without (f :+: g) h (Ri f p) where
    (m ?? n) (_ :: Ri f p) = (n . Inl) <?> (m ?? (n . Inr)) (undefined :: p)
    
(?) :: forall f g e r. Without f g (Minus f g) => (g e -> r) -> (OutOf (Minus f g) e -> r) -> f e -> r
m ? n = (m ?? n) (undefined :: Minus f g)
    

inj' :: (Inj f e (Into f e)) => f (Term e) -> Term e   
inj' = In . inj
    
    
-- Examples

data Const e = Const Int
data Op e = Add e e | Mul e e
data Plus e = Plus e e
data Times e = Times e e

type Expr = Term (Const :+: Op)
type Exp = Term (Const :+: Plus)
type Exp2 = Term (Const :+: Plus :+: Times)

ex :: Exp
ex = inj' (inj' (Const 1) `Plus` inj' (Const 2))

exOp :: Expr
exOp = inj' (inj' (Const 1) `Add` inj' (Const 2)) 

exOp2 :: Expr
exOp2 = inj' (inj' (Const 3) `Mul` exOp)

evalConst (Const i) r = i
evalPlus (Plus e1 e2) r = r e1 + r e2
evalTimes (Times e1 e2) r = r e1 * r e2

evalOp (Add e1 e2) r = r e1 + r e2
evalOp (Mul e1 e2) r = r e1 * r e2

cases :: (e (Term e) -> (Term e -> t) -> t) -> Term e -> t
cases cs = f where f (In e) = cs e f
      
eval1 :: Exp -> Int
eval1 = cases (evalConst ? evalPlus)

eval2 :: Expr -> Int
eval2 = cases (evalConst ? evalOp)




instance Functor Const
    where fmap _ (Const x) = Const x
          
instance Functor Op
    where fmap f (Add e1 e2) = Add (f e1) (f e2)
          fmap f (Mul e1 e2) = Mul (f e1) (f e2)


data Neg e = Neg e

-- OutOf (Minus f Neg) = g
-- f :-: Neg = g
-- desugNeg :: (Without f Neg (Minus f Neg)
--             , Inj Op (OutOf (Minus f Neg)) (Into Op (OutOf (Minus f Neg)))
--             , Inj Const (OutOf (Minus f Neg)) (Into Const (OutOf (Minus f Neg)))
--             , Functor (OutOf (Minus f Neg))) 
--             => Term f -> Term (OutOf (Minus f Neg))
desugNeg :: (Without f Neg (Minus f Neg)
            , Op :<: (f :-: Neg)
            , Const :<: (f :-: Neg)
            , Functor (f :-: Neg)) 
            => Term f -> Term (f :-: Neg)
desugNeg = cases (neg ? def) where
    neg (Neg e) r = inj' (Mul (inj' (Const (-1))) (r e))
    def e r = In (fmap desugNeg e)
    
data Double e = Double e
    
desugar e = cases ((\(Double e) r -> In (inj (Plus (r e) (r e)))) ? (const . In . fmap desugar)) e
    
    
type f :-: g = OutOf (Minus f g)
type f :<: g = Inj f g (Into f g)
    

-- from compdata:
-- type f :<: g = (Subsume (ComprEmb (Elem f g)) f g)