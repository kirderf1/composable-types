module FunctionTransform (transformFunDecl) where

import Language.Haskell.Exts

import qualified GeneratedNames as Names
import TransformUtils
import Utils.Names

import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.Except

-- | Transform a top level declaration to one or more new declarations
transformFunDecl :: Decl () -> Transform [Decl ()]
transformFunDecl (CompFunDecl _ names mcx category t) = do
    (sig, _) <- ask
    if Map.member category sig
      then concat <$> (declsForName `mapM` names)
      else do
          throwError $ "Expected first argument to be a piece category, was: \"" ++ prettyPrint category ++ "\""
  where
    declsForName :: Name () -> Transform [Decl ()]
    declsForName nam = do
        className <- Names.innerClass nam
        outerClassName <- Names.outerClass nam
        funcName <- Names.classFunction nam
        let tyvarNames = collectUniqueVars t
        classDecl <- functionClass mcx className funcName t tyvarNames
        return
          [ classDecl
          , liftSum className
          , outerClass outerClassName nam t tyvarNames
          , outerInstance className outerClassName funcName nam tyvarNames
          ]
transformFunDecl (CompFunExt _ mcx funName types pieceName Nothing) = do
    instHead <- createInstHead Nothing mcx funName types pieceName
    return [InstDecl () Nothing instHead Nothing]
transformFunDecl (CompFunExt _ mcx funName types pieceName (Just instDecls)) = do 
    instHead <- createInstHead Nothing mcx funName types pieceName
    instDecls' <- mapM transformInstDecl instDecls
    return [InstDecl () Nothing instHead (Just instDecls')]
transformFunDecl d = return [d]

-- | Build a declaration of a class corresponding to a function
functionClass :: Maybe (Context ()) -> Name () -> Name () -> Type () -> [Name ()] -> Transform (Decl ())
functionClass mcx className functionName t classVars = do
    funType <- transformFunType className (TyApp () (TyVar () (name "f")) (TyParen () term)) t classVars
    return $ ClassDecl () mcx
        declHeader []
        (Just [classFunctionDecl functionName funType])
  where
    declHeader = foldl (DHApp ()) (DHead () className) (map (UnkindedVar ()) (name "f" : classVars))

-- | Build the inner class declaration
classFunctionDecl :: Name () -> Type () -> ClassDecl ()
classFunctionDecl functionName t = ClsDecl () (TypeSig () [functionName] t)

-- | Build function type
transformFunType :: Name () -> Type () -> Type () -> [Name ()] -> Transform (Type ())
transformFunType cname replType ty classVars = do
    let resT = TyFun () replType ty
    return (TyForall () Nothing (Just (CxSingle () (ParenA () (TypeA () constraintType)))) resT)
  where
    constraintType = foldl (TyApp ()) (TyCon () (UnQual () cname)) (map (TyVar ()) (name "g" : classVars))

-- | Build type for term with parametric part "g"
term :: Type ()
term = termApp (TyVar () (name "g"))

-- | Derives liftSum for the function class
liftSum :: Name () -> Decl ()
liftSum className = SpliceDecl () (SpliceExp () (ParenSplice () (app (app (deriveTHListElem "derive") (List () [deriveTHListElem "liftSum"])) (List () [TypQuote () (UnQual () className)]))))

-- | Create instance head (roughly the first line of an instance declaration)
createInstHead :: Maybe [TyVarBind ()] -> Maybe (Context ()) -> Name () -> [Type ()] -> QName () -> Transform (InstRule ())
createInstHead mtvs mcx funName types pieceName = do
    className <- Names.innerClass funName
    return $ irule className mcx
  where
    irule className mcx' = IRule () mtvs mcx' (ihead className types)
    ihead className [] = IHApp () (IHCon () (UnQual () className)) (TyCon () pieceName)
    ihead className (t:ts) = IHApp () (ihead className ts) t

-- | Transform an instance declaration to have the function with a prime
transformInstDecl :: InstDecl () -> Transform (InstDecl ())
transformInstDecl (InsDecl _ (FunBind () matches)) = do 
    matches' <- mapM transformMatch matches
    return $ InsDecl () (FunBind () matches')
transformInstDecl _ = throwError "Unexpected type of instance declaration"
-- TODO: Possibly other constructs for InstDecl

-- | Transform function part of the instance declaration to have a prime on function name
transformMatch :: Match () -> Transform (Match ())
transformMatch (Match _ funName patterns rhs maybeBinds) = do
    funName' <- Names.classFunction funName
    return (Match () funName' patterns rhs maybeBinds)
transformMatch (InfixMatch _ pat funName patterns rhs maybeBinds) = do
    funName' <- Names.classFunction funName
    return (InfixMatch () pat funName' patterns rhs maybeBinds)

outerClass :: Name () -> Name () -> Type () -> [Name ()] -> Decl ()
outerClass className funName ty classVars = ClassDecl () Nothing declHead [] (Just [classDecl])
  where
    termvar = name "t"
    declHead = foldl (DHApp ()) (DHead () className) (map (UnkindedVar ()) (termvar : classVars))
    classDecl = ClsDecl () (TypeSig () [funName] funType)
    funType = TyFun () (TyVar () termvar) ty

outerInstance :: Name () -> Name () -> Name () -> Name () -> [Name ()] -> Decl ()
outerInstance innerCName outerCName innerFName outerFName classVars = InstDecl () Nothing instRule (Just [instDecl])
  where
    coprodvar = TyVar () (name "g")
    tyvars = map (TyVar ()) classVars
    instRule = IRule () Nothing (Just (CxSingle () assertion)) instHead
    instHead = foldl (IHApp ()) (IHCon () (UnQual () outerCName)) (TyParen () (termApp coprodvar) : tyvars)
    assertion = TypeA () (foldl (TyApp ()) (TyCon () (UnQual () innerCName)) (coprodvar : tyvars))
    instDecl = InsDecl () (FunBind () [Match () outerFName [] (UnGuardedRhs () funExp) Nothing])
    funExp = infixApp (var innerFName) (op (sym ".")) (qvar compdata (name "unTerm"))

