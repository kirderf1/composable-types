{-# LANGUAGE FlexibleContexts #-}

module TransformUtils where

import Language.Haskell.Exts

import qualified GeneratedNames as Names

import           Data.Map   (Map)
import           Data.Set   (Set)
import           Data.Maybe (catMaybes)

import Control.Monad.Reader
import Control.Monad.Except

-- | Map of category names to pieces
type Sig = Map (QName ()) (Set (QName ()))

-- | Set of all piece constructors
type Constrs = Set (QName ())

type Env = (Sig, Constrs)

-- | Transform monad containing signature of categories and handles error messages as Strings.
type Transform = ReaderT Env (Except String)

compdata :: ModuleName ()
compdata = ModuleName () "Data.Comp"

termApp :: Type () -> Type ()
termApp = TyApp () (TyCon () (Qual () compdata (name "Term")))

-- subName :: QName ()
-- subName = Qual () compdata (Symbol () ":<:")

lib :: ModuleName ()
lib = ModuleName () "ComposableTypes"

partOfName :: QName ()
partOfName = Qual () lib (Ident () "PartOf")

injectExp :: Exp ()
injectExp = qvar lib (name "inject'")

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
deriveTHListElem :: String -> Exp ()
deriveTHListElem nam = qvar (ModuleName () "Data.Comp.Derive") (name nam)

-- | Create context from list of assertions
contextFromList :: [Asst ()] -> Context ()
contextFromList [] = CxEmpty ()
contextFromList [a] = CxSingle () a
contextFromList as = CxTuple () as

-- | Transform constraint to assertion
constraintToAsst :: Constraint () -> Transform (Maybe (Asst ()))
constraintToAsst (FunConstraint _ fun v) = do
    cname <- Names.qOuterClass fun
    return (Just (TypeA () (TyApp () (TyCon () cname) (TyVar () v)))) 
constraintToAsst (PieceConstraint _ piece v) = return (Just (TypeA () (TyApp () 
    (TyApp () (TyCon () partOfName)  (TyCon () piece)) (TyVar () v))))
constraintToAsst (CategoryConstraint _ _category _v) = return (Nothing)


transformContext :: Context () -> Transform (Context ())
transformContext (CxEmpty _) = return (CxEmpty ())
transformContext (CxSingle _ asst) = transformContext' [asst]     
transformContext (CxTuple _ assts) = transformContext' assts

transformContext' :: [Asst ()] -> Transform (Context ())
transformContext' assts = do
    assts' <- mapM transformAsst assts 
    return (contextFromList (catMaybes assts'))

transformAsst :: Asst () -> Transform (Maybe (Asst ()))
transformAsst (CompCont _ constraint) = constraintToAsst constraint
transformAsst (ParenA _ asst) = do 
    masst' <- transformAsst asst
    case masst' of
         Just asst' -> return (Just (ParenA () asst'))
         Nothing -> return Nothing
transformAsst asst = return (Just asst) 
