{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Transform (transform, transform') where

import Language.Haskell.Exts
import Language.Haskell.Names

import qualified GeneratedNames as Names
import FunctionTransform
import PieceTransform
import TransformUtils
import TempEnv
import Utils.Types
import Utils.Decls
import Utils.Exps
import Utils.Contexts

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Reader
import Control.Monad.Except

transform :: Module SrcSpanInfo -> ExceptT String IO (Module ())
transform m = do
    baseEnv <- lift $ loadBase
    fromExcept $ transform' (resolve [m] baseEnv) m

-- | Transform a module by building signature of categories and then transforming the content of the module
transform' :: Environment -> Module SrcSpanInfo -> Except String (Module ())
transform' env m = do
    checkForDupCat symbols
    ensureCategoryIsDeclared env `mapM_` decls
    runReaderT (transformModule $ void m) (toEnv env)
  where
    moduleName = getModuleName m
    symbols = env Map.! moduleName
    Module _ _ _ _ decls = annotate env m
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
transformExp e@(Con _ (UnQual _ con)) = do
    (_, constrs) <- ask
    if Set.member con constrs
        then do
             smartCon <- Names.qSmartCon (UnQual () con)
             return $ Var () smartCon
        else return e
transformExp e@(InfixApp _ expr1 (QConOp _ (UnQual _ con)) expr2) = do
    (_, constrs) <- ask
    if Set.member con constrs
        then do
             smartCon <- Names.qSmartCon (UnQual () con)
             return $ InfixApp () expr1 (QVarOp () smartCon) expr2
        else return e
transformExp e@(LeftSection _ expr (QConOp _ (UnQual _ con))) = do
    (_, constrs) <- ask
    if Set.member con constrs
        then do
             smartCon <- Names.qSmartCon (UnQual () con)
             return $ LeftSection () expr (QVarOp () smartCon)
        else return e
transformExp e@(RightSection _ (QConOp _ (UnQual _ con)) expr) = do
    (_, constrs) <- ask
    if Set.member con constrs
        then do
             smartCon <- Names.qSmartCon (UnQual () con)
             return $ RightSection () (QVarOp () smartCon) expr
        else return e
transformExp e@(RecConstr _ (UnQual _ con) _) = do
    (_, constrs) <- ask
    return $ if Set.member con constrs
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

checkForDupCat :: [Symbol] -> Except String ()
checkForDupCat symbols = foldM_ duplicateCheck Set.empty symbols
  where
    duplicateCheck set s@PieceCategory{symbolName = category} =
        if Set.member s set
        then throwError $ "buildSigCat: category " ++ prettyPrint category ++ " already declared"
        else return $ Set.insert s set
    duplicateCheck set _ = return set

ensureCategoryIsDeclared :: Environment -> Decl (Scoped l) -> Except String ()
ensureCategoryIsDeclared sig (PieceDecl _ category _ _) = do
    let (Scoped info _) = ann category
    case info of
        GlobalSymbol PieceCategory{} _ -> return ()
        _                              -> throwError $ "Category \"" ++ prettyPrint category ++ "\" not declared."
ensureCategoryIsDeclared _ _ = return ()

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

