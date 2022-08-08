{-# LANGUAGE FlexibleContexts #-}

module GeneratedNames (smartCon, qSmartCon, recursiveVar, innerClass, qInnerClass, classFunction, outerClass, qOuterClass) where

import Language.Haskell.Exts

import Data.Default

import Control.Monad.Except (MonadError, throwError)

smartCon :: (MonadError String m) => Name l -> m (Name l)
smartCon = id_prefixed "composable_types_constructor_"

qSmartCon :: (MonadError String m) => QName l -> m (QName l)
qSmartCon = mapQName smartCon

recursiveVar :: Default l => Name l
recursiveVar = Ident def "composable_types_recursive_var"

innerClass :: (MonadError String m) => Name l -> m (Name l)
innerClass = id_prefixed "Composable_types_inner_class_"

qInnerClass :: (MonadError String m) => QName l -> m (QName l)
qInnerClass = mapQName innerClass

classFunction :: (MonadError String m) => Name l -> m (Name l)
classFunction = id_prefixed "composable_types_class_function_"

outerClass :: (MonadError String m) => Name l -> m (Name l)
outerClass = id_prefixed "Composable_types_outer_class_"

qOuterClass :: (MonadError String m) => QName l -> m (QName l)
qOuterClass = mapQName outerClass



id_prefixed :: (MonadError String m) => String -> Name l -> m (Name l)
id_prefixed prefix (Ident  l ident) = return $ Ident l (prefix ++ ident)
id_prefixed prefix (Symbol _ symbol) = throwError $ "Cannot apply prefix \"" ++ prefix ++ "\" to symbol \"" ++ symbol ++ "\""

mapQName :: (MonadError String m) => (Name l -> m (Name l)) -> QName l -> m (QName l)
mapQName f (Qual  l m n) = Qual l m <$> f n
mapQName f (UnQual  l n) = UnQual l <$> f n
mapQName _ (Special _ s) = throwError $ "Cannot map special name \"" ++ prettyPrint s ++ "\""
