{-# LANGUAGE FlexibleContexts #-}

module TransformUtils(Transform, compdata, termApp, coprodOp, partOfName, injectExp, deriveTHListElem, transformContext, getModuleName, fromExcept, extendContext, tyVarName, pieceRefAsType, qNameFromRef, def) where

import Language.Haskell.Exts
import Language.Haskell.Names (Environment, Scoped(..), NameInfo(None))

import qualified GeneratedNames as Names

import           Data.Maybe (catMaybes)
import Data.Functor.Identity(runIdentity)
import Data.Default

import Control.Monad.Reader
import Control.Monad.Except

-- | Transform monad containing signature of categories and handles error messages as Strings.
type Transform = ReaderT Environment (Except String)

compdata :: Default l => ModuleName l
compdata = ModuleName def "Data.Comp"

termApp :: Default l => Type l -> Type l
termApp = TyApp def (def <$ termType)

termType :: Type ()
termType = TyCon () (Qual () compdata (name "Term"))

coprodOp :: Default l => Type l -> Type l -> Type l
coprodOp t1 t2 = TyInfix def t1 (UnpromotedName def coprodName) t2

coprodName :: Default l => QName l
coprodName = Qual def compdata (Symbol def ":+:")

lib :: Default l => ModuleName l
lib = ModuleName def "ComposableTypes"

partOfName :: Default l => QName l
partOfName = Qual def lib (Ident def "PartOf")

injectExp :: Default l => Exp l
injectExp = Var def $ Qual def lib (Ident def "inject'")

-- | Template Haskell derive for a certain data type from a list of things to derive
deriveTH :: Name () -> [String] -> Decl () 
deriveTH targetName list = SpliceDecl () 
        (SpliceExp ()
            (ParenSplice ()
                (app
                    (app
                        (qvar (ModuleName () "Data.Comp.Derive") (name "derive")) 
                        (List () (map deriveTHListElem list))
                    ) 
                    (List () [TypQuote () (UnQual () targetName)])
                )
            )
        )

-- | Element for a thing to derive with Template Haskell
deriveTHListElem :: Default l => String -> Exp l
deriveTHListElem nam = Var def (Qual def (ModuleName def "Data.Comp.Derive") (Ident def nam))

-- | Create context from list of assertions
contextFromList :: Default l => [Asst l] -> Context l
contextFromList [] = CxEmpty def
contextFromList [a] = CxSingle def a
contextFromList as = CxTuple def as

-- | Transform constraint to assertion
constraintToAsst :: Default l => Constraint l -> Transform (Maybe (Asst l))
constraintToAsst (FunConstraint _ fun v) = do
    cname <- Names.qOuterClass fun
    return $ (Just (TypeA def (TyApp def (TyCon def cname) (TyVar def v)))) 
constraintToAsst (PieceConstraint _ pieceref v) = return $ (Just (TypeA def (TyApp def 
    (TyApp def (TyCon def partOfName) (pieceRefAsType pieceref)) (TyVar def v))))
constraintToAsst (CategoryConstraint _ _category _v) = return (Nothing)


transformContext :: Default l => Context l -> Transform (Context l)
transformContext (CxEmpty _) = return (CxEmpty def)
transformContext (CxSingle _ asst) = transformContext' [asst]     
transformContext (CxTuple _ assts) = transformContext' assts

transformContext' :: Default l => [Asst l] -> Transform (Context l)
transformContext' assts = do
    assts' <- mapM transformAsst assts 
    return (contextFromList (catMaybes assts'))

transformAsst :: Default l => Asst l -> Transform (Maybe (Asst l))
transformAsst (CompCont _ constraint) = constraintToAsst constraint
transformAsst (ParenA _ asst) = do 
    masst' <- transformAsst asst
    case masst' of
         Just asst' -> return (Just (ParenA def asst'))
         Nothing -> return Nothing
transformAsst asst = return (Just asst)

getModuleName :: Module l -> ModuleName ()
getModuleName (Module _ (Just (ModuleHead _ name _ _)) _ _ _) = void name
getModuleName (XmlPage _ name _ _ _ _ _) = void name
getModuleName (XmlHybrid _ (Just (ModuleHead _ name _ _)) _ _ _ _ _ _ _) = void name
getModuleName m = main_mod ()

-- | Wraps an Except to an Except transformer
fromExcept :: (Monad m) => Except e a -> ExceptT e m a
fromExcept = mapExceptT (return . runIdentity)

extendContext :: Default l => Asst l -> Maybe (Context l) -> Maybe (Context l)
extendContext asst Nothing = Just (CxSingle def asst)
extendContext asst (Just (CxEmpty l)) = Just (CxSingle l asst)
extendContext asst (Just (CxSingle l asst2)) = Just (CxTuple l (asst:[asst2]))
extendContext asst (Just (CxTuple l assts)) = Just (CxTuple l (asst:assts))

tyVarName :: TyVarBind l -> Name l
tyVarName (KindedVar _ name _) = name
tyVarName (UnkindedVar _ name) = name

pieceRefAsType :: Default l => PieceRef l -> Type l
pieceRefAsType (IHCon l piece) = TyCon l piece
pieceRefAsType pieceref = TyParen def (pieceRefAsType' pieceref)

pieceRefAsType' :: PieceRef l -> Type l
pieceRefAsType' (IHCon l piece) = TyCon l piece
pieceRefAsType' (IHInfix _ _ty _piece) = undefined
pieceRefAsType' (IHApp l pieceref ty) = TyApp l (pieceRefAsType' pieceref) ty
pieceRefAsType' (IHParen l pieceref) = TyParen l (pieceRefAsType' pieceref)

qNameFromRef :: PieceRef l -> QName l
qNameFromRef (IHCon _ piece) = piece
qNameFromRef (IHInfix _ _ piece) = piece
qNameFromRef (IHApp _ pieceref _) = qNameFromRef pieceref
qNameFromRef (IHParen _ pieceref) = qNameFromRef pieceref

instance Default l => Default (Scoped l) where
    def = Scoped None def
