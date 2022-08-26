{-#LANGUAGE UndecidableInstances#-}

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
                                  

-- | Render

class Render f where
    render :: Render g => f (Term g) -> String
    
pretty :: Render f => Term f -> String
pretty (In t) = render t

instance Render Const where
    render (Const i) = show i
    
instance Render Op where
    render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
    render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"
    
instance (Render f, Render g) => Render (f :+: g) where
    render (Inl x) = render x
    render (Inr y) = render y


  
-- |  Desug
  
class (Functor f, Functor g) => Desug f g where
    desugAlgebra :: f (Term g) -> (Term g)
    
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, f :<: g) => Desug f g where
    desugAlgebra = inject
    
instance (Functor g, Const :<: g, Op :<: g) => Desug Neg g where
    desugAlgebra (Neg e) = iConst (-1) |*| e
    
instance (Desug f h, Desug g h) => Desug (f :+: g) h where
    desugAlgebra (Inl x) = desugAlgebra x
    desugAlgebra (Inr y) = desugAlgebra y
    
desug :: (Desug f g, Functor f) => Term f -> Term g
desug = foldTerm desugAlgebra
                   
                 
         
         
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


