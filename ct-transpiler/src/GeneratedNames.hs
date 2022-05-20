{-# LANGUAGE FlexibleContexts #-}

module GeneratedNames (smartCon, qSmartCon, recursiveVar, innerClass, qInnerClass, classFunction, outerClass, qOuterClass) where

import Language.Haskell.Exts

import Control.Monad.Except (MonadError, throwError)

smartCon :: (MonadError String m) => Name () -> m (Name ())
smartCon = id_prefixed "composable_types_constructor_"

qSmartCon :: (MonadError String m) => QName () -> m (QName ())
qSmartCon = mapQName smartCon

recursiveVar :: Name ()
recursiveVar = name "composable_types_recursive_var"

innerClass :: (MonadError String m) => Name () -> m (Name ())
innerClass = id_prefixed "Composable_types_inner_class_"

qInnerClass :: (MonadError String m) => QName () -> m (QName ())
qInnerClass = mapQName innerClass

classFunction :: (MonadError String m) => Name () -> m (Name ())
classFunction = id_prefixed "composable_types_class_function_"

outerClass :: (MonadError String m) => Name () -> m (Name ())
outerClass = id_prefixed "Composable_types_outer_class_"

qOuterClass :: (MonadError String m) => QName () -> m (QName ())
qOuterClass = mapQName outerClass



id_prefixed :: (MonadError String m) => String -> Name () -> m (Name ())
id_prefixed prefix (Ident  _ ident) = return $ name (prefix ++ ident)
id_prefixed prefix (Symbol _ symbol) = throwError $ "Cannot apply prefix \"" ++ prefix ++ "\" to symbol \"" ++ symbol ++ "\""

mapQName :: (MonadError String m) => (Name () -> m (Name ())) -> QName () -> m (QName ())
mapQName f (Qual  _ m n) = Qual () m <$> f n
mapQName f (UnQual  _ n) = UnQual () <$> f n
mapQName _ (Special _ s) = throwError $ "Cannot map special name \"" ++ prettyPrint s ++ "\""
