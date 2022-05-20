{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module ComposableTypes where

import qualified Data.Comp

class PartOf a b where
    inject' :: a b -> b
    project' :: b -> Maybe (a b)
    
instance (a Data.Comp.:<: b) => PartOf a (Data.Comp.Term b) where
    inject' = Data.Comp.inject
    project' = Data.Comp.project
