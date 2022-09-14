{-#LANGUAGE UndecidableInstances#-}

module DTalC where

-- | Definitions from:
--  Wouter Swierstra. Data types à la carte. Journal of functional programming, 18(4):423–436, 2008
-- Basis for desug etc from:
--  Patrick Bahr and Tom Hvitved. Compositional Data Types. In Proceedings of the seventh ACM SIGPLAN workshop on Generic programming, pages 83–94, 2011
-- Patrick Bahr. Composing and Decomposing Data types: A Closed Type Families Implementation of Data Types à la Carte. In Proceedings of the 10th ACM SIGPLAN workshop on Generic programming, pages 71–82, 2014.
--  Patrick Bahr and Tom Hvitved. compdata: Compositional Data Types. https://hackage.haskell.org/package/compdata, 2021

-- |  Parameterization of a data type using Term
data Term f = In (f (Term f ))

-- | Coproduct
data (f :+: g) e = Inl (f e) | Inr (g e)
infixr :+:

instance (Functor f , Functor g) => Functor (f :+: g) where
    fmap f (Inl e1) = Inl (fmap f e1)
    fmap f (Inr e2) = Inr (fmap f e2) 

-- | Fold for Terms
foldTerm :: Functor f => (f a -> a) -> Term f -> a
foldTerm f (In t) = f (fmap (foldTerm f ) t)

-- | Subsumption and injection
class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a

instance Functor f => f :<: f where
    inj = id

instance (Functor f , Functor g) => f :<: (f :+: g) where
    inj = Inl

instance {-# OVERLAPPABLE #-} (Functor f , Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
    inj = Inr . inj

inject :: (g :<: f ) => g (Term f ) -> Term f
inject = In . inj
