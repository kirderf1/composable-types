module FunctionTransform (transformFunDecl) where

import Language.Haskell.Exts
import Language.Haskell.Names (Scoped(Scoped), NameInfo(GlobalSymbol), Symbol(..))

import qualified GeneratedNames as Names
import TransformUtils
import Utils.Names

import qualified Data.Map as Map
import Data.Default

import Control.Monad.Reader
import Control.Monad.Except

-- | Transform a top level declaration to one or more new declarations
transformFunDecl :: Decl (Scoped ()) -> Transform [Decl (Scoped ())]
transformFunDecl (CompFunDecl _ names mcx category t) = do
    case catInfo of
        GlobalSymbol PieceCategory{} _ -> concat <$> (declsForName `mapM` names)
        _ -> throwError $ "Expected first argument to be a piece category, was: \"" ++ prettyPrint category ++ "\""
  where
    Scoped catInfo _ = ann category
    declsForName :: Name (Scoped ()) -> Transform [Decl (Scoped ())]
    declsForName nam = do
        className <- Names.innerClass nam
        outerClassName <- Names.outerClass nam
        funcName <- Names.classFunction nam
        let tyvarNames = collectUniqueVars t
        classDecl <- functionClass mcx className funcName t tyvarNames
        return
          [ classDecl
          , liftSum className
          , outerClass mcx outerClassName nam t tyvarNames
          , outerInstance className outerClassName funcName nam tyvarNames
          ]
transformFunDecl (CompFunExt _ mcx funName types pieceRef Nothing) = do
    instHead <- createInstHead Nothing mcx funName types pieceRef
    return [InstDecl def Nothing instHead Nothing]
transformFunDecl (CompFunExt _ mcx funName types pieceRef (Just instDecls)) = do 
    instHead <- createInstHead Nothing mcx funName types pieceRef
    instDecls' <- mapM transformInstDecl instDecls
    return [InstDecl def Nothing instHead (Just instDecls')]
transformFunDecl d = return [d]

-- | Build a declaration of a class corresponding to a function
functionClass :: Maybe (Context (Scoped ())) -> Name (Scoped ()) -> Name (Scoped ()) -> Type (Scoped ()) -> [Name (Scoped ())] -> Transform (Decl (Scoped ()))
functionClass mcx className functionName t classVars = do
    funType <- transformFunType className (TyApp def (TyVar def Names.functorPiece) (TyParen def term)) t classVars
    return $ ClassDecl def mcx
        declHeader []
        (Just [classFunctionDecl functionName funType])
  where
    declHeader = foldl (DHApp def) (DHead def className) (map (UnkindedVar def) (Names.functorPiece : classVars))

-- | Build the inner class declaration
classFunctionDecl :: Name (Scoped ()) -> Type (Scoped ()) -> ClassDecl (Scoped ())
classFunctionDecl functionName t = ClsDecl def (TypeSig def [functionName] t)

-- | Build function type
transformFunType :: Name (Scoped ()) -> Type (Scoped ()) -> Type (Scoped ()) -> [Name (Scoped ())] -> Transform (Type (Scoped ()))
transformFunType cname replType ty classVars = do
    let resT = TyFun def replType ty
    return (TyForall def Nothing (Just (CxSingle def (ParenA def (TypeA def constraintType)))) resT)
  where
    constraintType = foldl (TyApp def) (TyCon def (UnQual def cname)) (map (TyVar def) (Names.functorComp : classVars))

-- | Build type for term with parametric part
term :: Default l => Type l
term = termApp (TyVar def Names.functorComp)

-- | Derives liftSum for the function class
liftSum :: Name (Scoped ()) -> Decl (Scoped ())
liftSum className = SpliceDecl def (SpliceExp def (ParenSplice def (App def (App def (deriveTHListElem "derive") (List def [deriveTHListElem "liftSum"])) (List def [TypQuote def (UnQual def className)]))))

-- | Create instance head (roughly the first line of an instance declaration)
createInstHead :: Maybe [TyVarBind (Scoped ())] -> Maybe (Context (Scoped ())) -> QName (Scoped ()) -> [Type (Scoped ())] -> PieceRef (Scoped ()) -> Transform (InstRule (Scoped ()))
createInstHead mtvs mcx funName types pieceRef = do
    checkExtRefs funName (qNameFromRef pieceRef)
    className <- Names.qInnerClass funName
    return $ irule className mcx
  where
    irule className mcx' = IRule def mtvs mcx' (ihead className types)
    ihead className [] = IHApp def (IHCon def className) (pieceRefAsType pieceRef)
    ihead className (t:ts) = IHApp def (ihead className ts) t

checkExtRefs :: QName (Scoped ()) -> QName (Scoped ()) -> Transform ()
checkExtRefs funName pieceName = do
    funCat <- funCatM
    pieceCat <- pieceCatM
    if funCat == pieceCat
    then return ()
    else throwError $ "Piece: " ++ prettyPrint pieceName ++ " not found in category: " ++ prettyPrint (snd funCat)
  where
    Scoped funInfo _ = ann funName
    Scoped pieceInfo _ = ann pieceName
    funCatM = case funInfo of
        GlobalSymbol (ExtFunction {categoryModule = mod, categoryName = nam}) _ -> return (mod, nam)
        s -> throwError $ "Unknown function: " ++ prettyPrint funName ++ ", " ++ show s
    pieceCatM = case pieceInfo of
        GlobalSymbol (Piece {categoryModule = mod, categoryName = nam}) _ -> return (mod, nam)
        s -> throwError $ "Unknown piece: " ++ prettyPrint pieceName ++ ", " ++ show s

-- | Transform an instance declaration to have the function with a prime
transformInstDecl :: InstDecl (Scoped ()) -> Transform (InstDecl (Scoped ()))
transformInstDecl (InsDecl l1 (FunBind l2 matches)) = do 
    matches' <- mapM transformMatch matches
    return $ InsDecl l1 (FunBind l2 matches')
transformInstDecl _ = throwError "Unexpected type of instance declaration"
-- TODO: Possibly other constructs for InstDecl

-- | Transform function part of the instance declaration to have a prime on function name
transformMatch :: Match (Scoped ()) -> Transform (Match (Scoped ()))
transformMatch (Match l funName patterns rhs maybeBinds) = do
    funName' <- Names.classFunction funName
    return (Match l funName' patterns rhs maybeBinds)
transformMatch (InfixMatch l pat funName patterns rhs maybeBinds) = do
    funName' <- Names.classFunction funName
    return (InfixMatch l pat funName' patterns rhs maybeBinds)

outerClass :: Maybe (Context (Scoped ())) -> Name (Scoped ()) -> Name (Scoped ()) -> Type (Scoped ()) -> [Name (Scoped ())] -> Decl (Scoped ())
outerClass mcx className funName ty classVars = ClassDecl def mcx declHead [] (Just [classDecl])
  where
    termvar = Ident def "t"
    declHead = foldl (DHApp def) (DHead def className) (map (UnkindedVar def) (termvar : classVars))
    classDecl = ClsDecl def (TypeSig def [funName] funType)
    funType = TyFun def (TyVar def termvar) ty

outerInstance :: Name (Scoped ()) -> Name (Scoped ()) -> Name (Scoped ()) -> Name (Scoped ()) -> [Name (Scoped ())] -> Decl (Scoped ())
outerInstance innerCName outerCName innerFName outerFName classVars = InstDecl def Nothing instRule (Just [instDecl])
  where
    coprodvar = TyVar def Names.functorComp
    tyvars = map (TyVar def) classVars
    instRule = IRule def Nothing (Just (CxSingle def assertion)) instHead
    instHead = foldl (IHApp def) (IHCon def (UnQual def outerCName)) (TyParen def (termApp coprodvar) : tyvars)
    assertion = TypeA def (foldl (TyApp def) (TyCon def (UnQual def innerCName)) (coprodvar : tyvars))
    instDecl = InsDecl def (FunBind def [Match def outerFName [] (UnGuardedRhs def funExp) Nothing])
    funExp = InfixApp def (Var def $ UnQual def innerFName) (QVarOp def $ UnQual def (Symbol def ".")) (Var def $ Qual def compdata (Ident def "unTerm"))

