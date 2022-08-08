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
transform' env m = runReaderT (transformModule (void <$> annotate env m)) env

-- | Transform a module to remove syntax for composable types if the pragma is present
transformModule :: Module (Scoped ()) -> Transform (Module ())
transformModule m@(Module l mhead pragmas imports decls) =
    if pragmasContain pragmaName pragmas
        then do
            env <- ask
            checkForDupCat $ env Map.! getModuleName m
            ensureCategoryIsDeclared `mapM_` decls
            
            let pragmas' = modifyPragmas pragmas
                imports' = modifyImports imports
            void <$> (mapDecl transformFunDecl
                =<< mapDecl transformPieceDecl
                =<< mapExp transformExp
                =<< mapContext transformContext
                =<< mapType transformCompType (Module l mhead pragmas' imports' decls))
        else return $ void m
transformModule _xml = throwError "transformModule not defined for xml formats" 
-- ^ XmlPage and XmlHybrid formats not handled (yet)

-- | Transform an expression
transformExp :: Exp (Scoped ()) -> Transform (Exp (Scoped ()))
transformExp e@(Con l qcon@(UnQual _ con)) = do
    (_, constrs) <- toEnv <$> ask
    if Set.member (void con) constrs
        then do
             smartCon <- Names.qSmartCon qcon
             return $ Var l smartCon
        else return e
transformExp e@(InfixApp l1 expr1 (QConOp l2 qcon@(UnQual _ con)) expr2) = do
    (_, constrs) <- toEnv <$> ask
    if Set.member (void con) constrs
        then do
             smartCon <- Names.qSmartCon qcon
             return $ InfixApp l1 expr1 (QVarOp l2 smartCon) expr2
        else return e
transformExp e@(LeftSection l1 expr (QConOp l2 qcon@(UnQual _ con))) = do
    (_, constrs) <- toEnv <$> ask
    if Set.member (void con) constrs
        then do
             smartCon <- Names.qSmartCon qcon
             return $ LeftSection l1 expr (QVarOp l2 smartCon)
        else return e
transformExp e@(RightSection l1 (QConOp l2 qcon@(UnQual _ con)) expr) = do
    (_, constrs) <- toEnv <$> ask
    if Set.member (void con) constrs
        then do
             smartCon <- Names.qSmartCon qcon
             return $ RightSection l1 (QVarOp l2 smartCon) expr
        else return e
transformExp e@(RecConstr l (UnQual _ con) _) = do
    (_, constrs) <- toEnv <$> ask
    return $ if Set.member (void con) constrs
        then App l injectExp e
        else e
transformExp e = return e

-- | Modify a list of pragmas to remove ComposableTypes and add the ones needed for compdata
modifyPragmas :: [ModulePragma (Scoped ())] -> [ModulePragma (Scoped ())]
modifyPragmas ps =  foldr addPragma (removeCompTypes ps)
                                ["TemplateHaskell","TypeOperators"
                                ,"FlexibleContexts","FlexibleInstances","MultiParamTypeClasses"
                                ,"UndecidableInstances"] 
    where  
        addPragma :: String -> [ModulePragma (Scoped ())] -> [ModulePragma (Scoped ())]
        addPragma nam prs = if pragmasContain nam prs 
                                 then prs
                                 else LanguagePragma def [Ident def nam] : prs
        removeCompTypes = filter (not . matchPragma pragmaName)

-- | Check if the list of pragmas contain a certain one
pragmasContain :: String -> [ModulePragma l] -> Bool
pragmasContain nam = any (matchPragma nam)
        
-- | Check if a pragma match the given String
matchPragma :: String -> ModulePragma l -> Bool
matchPragma s (LanguagePragma _ [Ident _ nam]) = nam == s
matchPragma _ _ = False

checkForDupCat :: [Symbol] -> Transform ()
checkForDupCat symbols = foldM_ duplicateCheck Set.empty symbols
  where
    duplicateCheck set s@PieceCategory{symbolName = category} =
        if Set.member s set
        then throwError $ "buildSigCat: category " ++ prettyPrint category ++ " already declared"
        else return $ Set.insert s set
    duplicateCheck set _ = return set

ensureCategoryIsDeclared :: Decl (Scoped l) -> Transform ()
ensureCategoryIsDeclared (PieceDecl _ category _ _) = do
    let (Scoped info _) = ann category
    case info of
        GlobalSymbol PieceCategory{} _ -> return ()
        _                              -> throwError $ "Category \"" ++ prettyPrint category ++ "\" not declared."
ensureCategoryIsDeclared _ = return ()

-- | Modify a list of import declarations to add the ones needed for compdata
modifyImports :: [ImportDecl (Scoped ())] -> [ImportDecl (Scoped ())]
modifyImports is =  foldr addImport is
                                ["Data.Comp", "Data.Comp.Derive",
                                 libraryModule] 
    where  
        addImport :: String -> [ImportDecl (Scoped ())] -> [ImportDecl (Scoped ())]
        addImport nam is1 = if importsContain nam is1
                                 then is1
                                 else (ImportDecl
                                 { importAnn = def                    -- ^ annotation, used by parser for position of the @import@ keyword.
                                 , importModule = ModuleName def nam  -- ^ name of the module imported.
                                 , importQualified = True             -- ^ imported @qualified@?
                                 , importSrc = False                  -- ^ imported with @{-\# SOURCE \#-}@?
                                 , importSafe = False                 -- ^ Import @safe@?
                                 , importPkg = Nothing                -- ^ imported with explicit package name
                                 , importAs = Nothing                 -- ^ optional alias name in an @as@ clause.
                                 , importSpecs = Nothing              -- ^ optional list of import specifications.
                                 }):is1


-- | Check if the list of import declarations contain a certain one
importsContain :: String -> [ImportDecl l] -> Bool
importsContain nam = any (matchImport nam)
        
-- | Check if an import declaration match the given String
matchImport :: String -> ImportDecl l -> Bool
matchImport s (ImportDecl {importModule = ModuleName _ nam}) = nam == s

-- | String constants relating to this language extension
pragmaName, libraryModule :: String
pragmaName    = "ComposableTypes"
libraryModule = "ComposableTypes"

