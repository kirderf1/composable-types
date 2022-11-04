{-# LANGUAGE FlexibleContexts #-}

module Utils.Exports(mapExports) where

import Control.Monad.Except (MonadError, throwError, void)
import Language.Haskell.Exts.Syntax


class ExportMap a where
    mapExports :: (MonadError String m) => (ExportSpec l -> m [ExportSpec l]) -> a l -> m (a l)


instance ExportMap Module where
    mapExports f (Module l mmh ops iss dcls) = do
          Module l <$> mapExports f `mapM` mmh <*> return ops <*> return iss <*> return dcls
    mapExports f (XmlPage l mn os xn xas me es) =
          return $ XmlPage l mn os xn xas me es
    mapExports f (XmlHybrid l mmh ops iss dcls xn xas me es) =
          XmlHybrid l <$> mapExports f `mapM` mmh <*> return ops <*> return iss <*> return dcls <*> return xn <*> return xas <*> return me <*> return es

instance ExportMap ModuleHead where
    mapExports f (ModuleHead l n mwt mesl) = ModuleHead l n mwt <$> mapExports f `mapM` mesl

instance ExportMap ExportSpecList where
    mapExports f (ExportSpecList l ess) = do
        esss <- mapM f ess
        return $ ExportSpecList l (concat esss)
