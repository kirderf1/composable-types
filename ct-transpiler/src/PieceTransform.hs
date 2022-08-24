
module PieceTransform (transformPieceDecl, transformCompType) where

import Language.Haskell.Exts
import Language.Haskell.Names (Scoped(Scoped), NameInfo(GlobalSymbol), Symbol(..))

import qualified GeneratedNames as Names
import TransformUtils

import qualified Data.Map as Map
import           Data.Set   (Set)
import qualified Data.Set as Set

import Control.Monad.Reader
import Control.Monad.Except

-- | Transform a top level declaration to one or more new declarations
transformPieceDecl :: Decl (Scoped ()) -> Transform [Decl (Scoped ())]
transformPieceDecl (PieceDecl _ category pieceName cons) = 
    let cspar = map (parametConstructor Names.recursiveVar (void category)) cons
        in do
    smartCons <- concat <$> mapM (smartCon (void category) pieceName) cons
    return (DataDecl 
        def
        (DataType def)
        Nothing 
        (DHApp def (DHead def pieceName) (UnkindedVar def Names.recursiveVar))
        cspar
        []
        : smartCons)
transformPieceDecl (PieceCatDecl _ _) = return []
transformPieceDecl d = return [d]

-- | Transform a type
transformCompType :: Type (Scoped ()) -> Transform (Type (Scoped ()))
transformCompType (TyComp _ category types) =
    case catInfo of
        GlobalSymbol catSymbol@PieceCategory{} _ -> do 
            lift $ verifyPieceCategory category catSymbol `mapM_` types
            coprodtype <- coprod $ types
            return $ termApp (TyParen def coprodtype)
        _ -> throwError $ "Trying to form type of unknown category: " ++ prettyPrint category
    where
        Scoped catInfo _ = ann category
transformCompType t = return t

-- | Form coproduct type from a list of pieces
coprod :: [QName (Scoped ())] -> Transform (Type (Scoped ()))
coprod [nam] = return $ TyCon def nam
coprod (nam:ns) = do
    rest <- coprod ns
    return $ (TyCon def nam) `coprodOp` rest
coprod _ = throwError "Trying to form coproduct of no arguments"

verifyPieceCategory :: QName m -> Symbol -> QName (Scoped l) -> Except String ()
verifyPieceCategory category catSymbol piece =
    case pieceInfo of
        GlobalSymbol (Piece {categoryModule = mod, categoryName = nam}) _
            | mod == symbolModule catSymbol && nam == symbolName catSymbol -> return ()
        _ -> throwError $ "Piece: " ++ prettyPrint piece ++ " not found in category: " ++ prettyPrint category
    where
        Scoped pieceInfo _ = ann piece

{- | Parametrize a piece constructor to have a parametrized variable as recursive 
    parameter instead of the name of the category it belongs to.
-}
parametConstructor :: Name (Scoped ()) -> QName () -> QualConDecl (Scoped ()) -> QualConDecl (Scoped ())
parametConstructor parname category (QualConDecl l v c conDecl) = 
    QualConDecl l v c (parametCon conDecl)
    where 
        parametCon (ConDecl      l cname types)       = ConDecl      l cname (parametType <$> types)
        parametCon (InfixConDecl l type1 cname type2) = InfixConDecl l (parametType type1) cname (parametType type2)
        parametCon (RecDecl      l cname fields)      = RecDecl      l cname (parametField <$> fields)
        
        parametField (FieldDecl l names ty) = FieldDecl l names (parametType ty)
        
        parametType (TyCon l recu) | void recu == category = TyVar l parname
        parametType t = t

collectTypes :: QualConDecl (Scoped ()) -> (Name (Scoped ()), [Type (Scoped ())])
collectTypes (QualConDecl _ _ _ conDecl) = conDeclArgs conDecl
  where
    conDeclArgs :: ConDecl (Scoped ()) -> (Name (Scoped ()), [Type (Scoped ())])
    conDeclArgs (ConDecl _ nam types) = (nam, types)
    conDeclArgs (InfixConDecl _ t1 nam t2) = (nam, [t1, t2])
    conDeclArgs (RecDecl _ nam fields) = (nam, concat $ map fieldTypes fields)
      where fieldTypes (FieldDecl _ names ty) = replicate (length names) ty

smartCon :: QName () -> Name (Scoped ()) -> QualConDecl (Scoped ()) -> Transform [Decl (Scoped ())]
smartCon cat piece con = do
    funName <- Names.smartCon conName
    let pattern = PApp def (UnQual def funName) (PVar def <$> argNames)
    return $ [typeBinding funName, PatBind def pattern (UnGuardedRhs def expr) Nothing]
  where
    (conName, argTypes) = collectTypes con
    args = length argTypes
    argNames = (\arg -> Ident def $ "arg_" ++ show (arg)) <$> [1..args]
    internalType = TyVar def (Ident def "g")    
    replaceCat (TyCon def tycon) | void tycon == cat = internalType
    replaceCat t                               = t
    funType = foldr (TyFun def) internalType (replaceCat <$> argTypes)
    subAssertion = TypeA def (TyApp def (TyApp def (TyCon def partOfName) (TyCon def (UnQual def piece))) internalType)
    typeBinding funName = TypeSig def [funName] (TyForall def Nothing (Just (CxSingle def subAssertion)) funType)
    
    expr = App def injectExp (foldl (App def) (Con def $ UnQual def conName) (Var def . UnQual def <$> argNames))

