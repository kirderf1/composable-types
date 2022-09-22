{-# LANGUAGE TypeFamilies, UndecidableInstances, ExplicitForAll,
    TypeOperators, ScopedTypeVariables, ConstraintKinds,
    FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module VariationsOnVariants where


-- | Below definitions are from:  
-- J Garrett Morris. Variations on Variants. ACM SIGPLAN Notices, 50(12):71–81, 2015

-- | Term, the fixed point recursive knot
data Term e = In (e (Term e))


-- | Coproduct
data (f :+: g) e = Inl (f e) | Inr (g e)

-- | Functor instance of the coproduct
instance (Functor f , Functor g) => Functor (f :+: g) where
    fmap f (Inl e1) = Inl (fmap f e1)
    fmap f (Inr e2) = Inr (fmap f e2) 


-- | Branching operator
-- Given functions for f and g respectively, return function for their coproduct
(<?>) :: (f e -> a) -> (g e -> a) -> (f :+: g) e -> a
(f <?> g) (Inl x) = f x
(f <?> g) (Inr x) = g x


-- | Auxillary empty data types
data Yep
data Nope

-- | Membership test
type family IsIn f g where
    IsIn f f = Yep
    IsIn (f :+: f') (g :+: g') = Or (Or (IsIn (f :+: f') g) (IsIn (f :+: f') g')) 
                                    (And (IsIn f (g :+: g')) (IsIn f' (g :+: g')))
    IsIn f (g :+: h) = Or (IsIn f g) (IsIn f h)
    IsIn f g = Nope

-- | Auxillary type family for disjunction
type family Or b c where
    Or Nope Nope = Nope
    Or b c = Yep

-- | Auxillary type family for conjunction
type family And b c  where
    And Yep Yep = Yep
    And b c     = Nope


-- | More auxillary empty data types
data Refl
data L x
data R x

-- | Proof search for type-level witness for subsumption
type family Into f g where
    Into f f = Refl
    Into f (g :+: h) = Ifi (Into f g) (IsIn f h) (Into f h) (IsIn f g)
    Into f g = Nope
    
-- | Auxillary witnesess
type family Ifi lp inr rp inl where
    Ifi Nope inr Nope inl = Nope
    Ifi Nope inr rp Nope = R rp
    Ifi lp Nope rp inl = L lp
    Ifi lp inr rp inl = Nope
    
-- | Injection, using witness p of the proof that f is summand of g
class Inj f g p where
    injp :: p -> f e -> g e
    
instance Inj f f Refl where
    injp _ = id
    
instance Inj f g p => Inj f (g :+: h) (L p) where
    injp (_ :: L p) = Inl . injp (undefined :: p)
    
instance Inj f h p => Inj f (g :+: h) (R p) where
    injp (_ :: R p) = Inr . injp (undefined :: p)
    
-- | Injection function, hiding the type-level witness Into
inj :: forall f g e. (Inj f g (Into f g)) => f e -> g e
inj = injp (undefined :: Into f g)

-- | Auxillary function to simplify injection
inject :: (Inj f e (Into f e)) => f (Term e) -> Term e   
inject = In . inj


-- | More auxillary data types
data Onl (h :: * -> *)
data Onr (h :: * -> *)
data Le (g :: * -> *) p
data Ri (f :: * -> *) p
data Found (f :: * -> *)

-- | Proof search for witness for subtracting types in coproducts
type family Minus f g where
    Minus f f = Nope
    Minus (f :+: g) f = Onl g
    Minus (f :+: g) g = Onr f
    Minus (f :+: g) h = Ifm g (Minus f h) (IsIn f g)
                            f (Minus g h) (IsIn f h)
    Minus f g = Found f
    
-- | Auxillary witnesses
type family Ifm g lp inr f rp inl where
    Ifm g Nope inr f Nope inl = Nope
    Ifm g Nope inr f rp Nope = Ri f rp
    Ifm g lp Nope f rp inl = Le g lp
    
-- | Extract the type from a proof-search done with Minus
type family OutOf p where
    OutOf (Onl x) = x
    OutOf (Onr x) = x
    OutOf (Le f p) = OutOf p :+: f
    OutOf (Ri f p) = f :+: OutOf p
    
-- | Branching operator, using witness p of the proof that g can be removed from f
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
    
-- | Wrapper branching operator, hiding the Minus witness
(?) :: forall f g e r. Without f g (Minus f g) => (g e -> r) -> (OutOf (Minus f g) e -> r) -> f e -> r
m ? n = (m ?? n) (undefined :: Minus f g)
    

-- | Shorthands for subtraction and subsumption
type f :-: g = OutOf (Minus f g)
type f :<: g = Inj f g (Into f g)


-- | Cases, used to evaluate using different function variants
-- e.g. cases (evalConst ? evalOp)
cases :: (e (Term e) -> (Term e -> t) -> t) -> Term e -> t
cases cs = f where f (In e) = cs e f
