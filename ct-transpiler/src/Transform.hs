{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Transform (transform, transformTest) where

import Language.Haskell.Exts

import qualified GeneratedNames as Names
import FunctionTransform
import PieceTransform
import TransformUtils
import Utils.Types
import Utils.Decls
import Utils.Exps
import Utils.Contexts

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Reader
import Control.Monad.Except

transform :: Module () -> Except String (Module ())
transform m = do
    (mod, _) <- transform' m (Map.empty, Set.empty)
    return mod

transformTest :: Module () -> Except String (Module (), Env)
transformTest m = transform' m (Map.empty, Set.empty)

-- | Transform a module by building signature of categories and then transforming the content of the module
transform' :: Module () -> Env -> Except String (Module (), Env)
transform' m@(Module _ _mhead _pragmas _imports decls) (importSig, importConstrs) = do
    sigCat <- buildSigCat decls
    sig    <- buildSigPiece decls sigCat
    constrs <- buildConstrs decls
    let sig' = Map.unionWith Set.union importSig sig
        constrs' = Set.union importConstrs constrs
        env = (sig', constrs')
    mod <- runReaderT (transformModule m) env
    return (mod, env)
transform' _xml _ = throwError "transform not defined for xml formats" 
-- ^ XmlPage and XmlHybrid formats not handled (yet)

-- | Transform a module to remove syntax for composable types if the pragma is present
transformModule :: Module () -> Transform (Module ())
transformModule m@(Module _ mhead pragmas imports decls) =
    if pragmasContain pragmaName pragmas
        then do
            let pragmas' = modifyPragmas pragmas
                imports' = modifyImports imports
            mapDecl transformFunDecl 
                =<< mapDecl transformPieceDecl
                =<< mapExp transformExp
                =<< mapContext transformContext
                =<< mapType transformCompType (Module () mhead pragmas' imports' decls)
        else return m
transformModule _xml = throwError "transformModule not defined for xml formats" 
-- ^ XmlPage and XmlHybrid formats not handled (yet)

-- | Transform an expression
transformExp :: Exp () -> Transform (Exp ())
transformExp e@(Con _ qcon) = do
    (_, constrs) <- ask
    if Set.member qcon constrs
        then do
             smartCon <- Names.qSmartCon qcon
             return $ Var () smartCon
        else return e
transformExp e@(InfixApp _ expr1 (QConOp _ qcon) expr2) = do
    (_, constrs) <- ask
    if Set.member qcon constrs
        then do
             smartCon <- Names.qSmartCon qcon
             return $ InfixApp () expr1 (QVarOp () smartCon) expr2
        else return e
transformExp e@(LeftSection _ expr (QConOp _ qcon)) = do
    (_, constrs) <- ask
    if Set.member qcon constrs
        then do
             smartCon <- Names.qSmartCon qcon
             return $ LeftSection () expr (QVarOp () smartCon)
        else return e
transformExp e@(RightSection _ (QConOp _ qcon) expr) = do
    (_, constrs) <- ask
    if Set.member qcon constrs
        then do
             smartCon <- Names.qSmartCon qcon
             return $ RightSection () (QVarOp () smartCon) expr
        else return e
transformExp e@(RecConstr _ qcon _) = do
    (_, constrs) <- ask
    return $ if Set.member qcon constrs
        then app injectExp e
        else e
transformExp e = return e

-- | Modify a list of pragmas to remove ComposableTypes and add the ones needed for compdata
modifyPragmas :: [ModulePragma ()] -> [ModulePragma ()]
modifyPragmas ps =  foldr addPragma (removeCompTypes ps)
                                ["TemplateHaskell","TypeOperators"
                                ,"FlexibleContexts","FlexibleInstances","MultiParamTypeClasses"
                                ,"UndecidableInstances"] 
    where  
        addPragma :: String -> [ModulePragma ()] -> [ModulePragma ()]
        addPragma nam prs = if pragmasContain nam prs 
                                 then prs
                                 else LanguagePragma () [name nam] : prs
        removeCompTypes = filter (not . matchPragma pragmaName)

-- | Check if the list of pragmas contain a certain one
pragmasContain :: String -> [ModulePragma ()] -> Bool
pragmasContain nam = any (matchPragma nam)
        
-- | Check if a pragma match the given String
matchPragma :: String -> ModulePragma () -> Bool
matchPragma s (LanguagePragma _ [Ident _ nam]) = nam == s
matchPragma _ _ = False

-- | Build signature of categories with empty maps
buildSigCat :: [Decl ()] -> Except String Sig
buildSigCat [] = return Map.empty
buildSigCat ((PieceCatDecl _ category):decls) = do
    sig <- buildSigCat decls
    let category' = UnQual () category

    case Map.lookup category' sig of
         Just _ -> throwError $ "buildSigCat: category " ++ show category' ++ " already declared"
         Nothing -> return $ Map.insert category' Set.empty sig
buildSigCat (_:decls) = buildSigCat decls
    
-- | Build signature, add pieces to map of categories
buildSigPiece :: [Decl ()] -> Sig -> Except String Sig
buildSigPiece [] sig = return sig
buildSigPiece  ((PieceDecl _ category headName _cons _derives):decls) sig = do
    sig' <- buildSigPiece decls sig
    case Map.lookup category sig' of
        Just oldCons -> return $ Map.insert category (Set.insert (UnQual () headName) oldCons) sig'
        Nothing -> throwError $ "Category \"" ++ prettyPrint category ++ "\" not declared."
buildSigPiece (_:decls) sig = buildSigPiece decls sig

-- | Build set of all piece constructors
buildConstrs :: [Decl ()] -> Except String Constrs
buildConstrs [] = return Set.empty
buildConstrs ((PieceDecl _ _category _headName cons _derives):decls) = do
    constrs <- buildConstrs decls
    return $ foldr Set.insert constrs (qualConName <$> cons)
    where qualConName (QualConDecl _ _mForAll _mContext conDecl) = UnQual () (conName conDecl)
          conName (ConDecl _ nam _types) = nam
          conName (InfixConDecl _ _type nam _types) = nam
          conName (RecDecl _ nam _fdecls) = nam
buildConstrs (_:decls) = buildConstrs decls

-- | Modify a list of import declarations to add the ones needed for compdata
modifyImports :: [ImportDecl ()] -> [ImportDecl ()]
modifyImports is =  foldr addImport is
                                ["Data.Comp", "Data.Comp.Derive",
                                 libraryModule] 
    where  
        addImport :: String -> [ImportDecl ()] -> [ImportDecl ()]
        addImport nam is1 = if importsContain nam is1 
                                 then is1
                                 else (ImportDecl
                                 { importAnn = ()                     -- ^ annotation, used by parser for position of the @import@ keyword.
                                 , importModule = ModuleName () nam   -- ^ name of the module imported.
                                 , importQualified = True            -- ^ imported @qualified@?
                                 , importSrc = False                  -- ^ imported with @{-\# SOURCE \#-}@?
                                 , importSafe = False                 -- ^ Import @safe@?
                                 , importPkg = Nothing                -- ^ imported with explicit package name
                                 , importAs = Nothing                 -- ^ optional alias name in an @as@ clause.
                                 , importSpecs = Nothing              -- ^ optional list of import specifications.
                                 }):is1


-- | Check if the list of import declarations contain a certain one
importsContain :: String -> [ImportDecl ()] -> Bool
importsContain nam = any (matchImport nam)
        
-- | Check if an import declaration match the given String
matchImport :: String -> ImportDecl () -> Bool
matchImport s (ImportDecl {importModule = ModuleName _ nam}) = nam == s

-- | String constants relating to this language extension
pragmaName, libraryModule :: String
pragmaName    = "ComposableTypes"
libraryModule = "ComposableTypes"

