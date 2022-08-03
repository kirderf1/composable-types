module DTalC where

-- | Necessary definitions

data Term f = In (f (Term f ))

data (f :+: g) e = Inl (f e) | Inr (g e)
infixr :+:


-- | Data type for expression language

data Const a = Const Int

data Op a = Add a a | Mul a a 

type Expr = Term (Const :+: Op)


addExample :: Expr 
addExample = In (Inr (Add (In (Inl (Const 3)))
                              (In (Inl (Const 5)))))

                              
instance Functor Const where
    fmap f (Const x ) = Const x

instance Functor Op where
    fmap f (Add e1 e2) = Add (f e1) (f e2)
    fmap f (Mul e1 e2) = Mul (f e1) (f e2) 

instance (Functor f , Functor g) => Functor (f :+: g) where
    fmap f (Inl e1) = Inl (fmap f e1)
    fmap f (Inr e2) = Inr (fmap f e2)
                              
foldTerm :: Functor f => (f a -> a) -> Term f -> a
foldTerm f (In t) = f (fmap (foldTerm f ) t)


class Functor f => Eval f where
    evalAlgebra :: f Int -> Int
    
instance Eval Const where
    evalAlgebra (Const x) = x

instance Eval Op where
    evalAlgebra (Add x y) = x + y
    evalAlgebra (Mul x y) = x * y
    
instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlgebra (Inl x) = evalAlgebra x
    evalAlgebra (Inr y) = evalAlgebra y
    
eval :: Eval f => Term f -> Int
eval = foldTerm evalAlgebra


-- | Extension
data Neg a = Neg a

instance Functor Neg where
    fmap f (Neg e) = Neg (f e)

type Expr' = Term (Const :+: Op :+: Neg)

instance Eval Neg where
    evalAlgebra (Neg a) = (-1) * a
    
negExample :: Expr' 
negExample = In (Inr (Inr (Neg (In (Inl (Const 5))))))

negAddExample :: Expr' 
negAddExample = In (Inr (Inl (Add (In (Inl (Const 3)))
                                  (In (Inr (Inr (Neg (In (Inl (Const 5))))))))))
                                  
                                  
                                  
-- | Subsumption, injection and smart constructors

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

iConst :: (Const :<: f ) => Int -> Term f
iConst x = inject (Const x )

(|+|) :: (Op :<: f ) => Term f -> Term f -> Term f
x |+| y = inject (Add x y)
infixl 6 |+|

(|*|) :: (Op :<: f ) => Term f -> Term f -> Term f
x |*| y = inject (Mul x y)
infixl 7 |*|

neg :: (Neg :<: f) => Term f -> Term f
neg e = inject (Neg e)


negAddExample' :: Expr'
negAddExample' = iConst 3 |+| (neg (iConst 5))


