{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where
import qualified Data.Comp
import qualified Data.Comp.Derive
import qualified ComposableTypes
import A

data B composable_types_recursive_var = C

composable_types_constructor_C :: ComposableTypes.PartOf B g => g
composable_types_constructor_C = ComposableTypes.inject' C
