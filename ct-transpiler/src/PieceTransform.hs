
module PieceTransform (transformPieceDecl, transformCompType) where

import Language.Haskell.Exts
import Language.Haskell.Names (Scoped(Scoped), NameInfo(GlobalSymbol), Symbol(..))

import qualified GeneratedNames as Names
import TransformUtils

import qualified Data.Map as Map
import           Data.Set   (Set)
import qualified Data.Set as Set
import Data.Default

import Control.Monad.Reader
import Control.Monad.Except

-- | Transform a top level declaration to one or more new declarations
transformPieceDecl :: Decl (Scoped ()) -> Transform [Decl (Scoped ())]
transformPieceDecl (PieceDecl _ category context declHead cons) = 
    let cspar = map (parametConstructor Names.recursiveVar (void category)) cons
        in do
    smartCons <- concat <$> mapM (smartCon (void category) context declHead) cons
    return (DataDecl 
        def
        (DataType def)
        Nothing 
        (DHApp def (declHead) (UnkindedVar def Names.recursiveVar))
        cspar
        []
        : smartCons)
transformPieceDecl (PieceCatDecl _ _) = return []
transformPieceDecl d = return [d]

-- | Transform a type
transformCompType :: Type (Scoped ()) -> Transform (Type (Scoped ()))
transformCompType (TyComp _ category piecerefs) =
    case catInfo of
        GlobalSymbol catSymbol@PieceCategory{} _ -> do 
            lift $ verifyPieceCategory category catSymbol `mapM_` piecerefs
            coprodtype <- coprod piecerefs
            return $ termApp (TyParen def coprodtype)
        _ -> throwError $ "Trying to form type of unknown category: " ++ prettyPrint category
    where
        Scoped catInfo _ = ann category
transformCompType t = return t

-- | Form coproduct type from a list of pieces
coprod :: [PieceRef (Scoped ())] -> Transform (Type (Scoped ()))
coprod [ref] = return $ pieceRefAsType ref
coprod (ref:rs) = do
    rest <- coprod rs
    return $ (pieceRefAsType ref) `coprodOp` rest
coprod _ = throwError "Trying to form coproduct of no arguments"

verifyPieceCategory :: QName m -> Symbol -> PieceRef (Scoped l) -> Except String ()
verifyPieceCategory category catSymbol piece =
    case pieceInfo of
        GlobalSymbol (Piece {categoryModule = mod, categoryName = nam}) _
            | mod == symbolModule catSymbol && nam == symbolName catSymbol -> return ()
        _ -> throwError $ "Piece: " ++ prettyPrint piece ++ " not found in category: " ++ prettyPrint category
    where
        Scoped pieceInfo _ = ann (qNameFromRef piece)

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

smartCon :: QName () -> Maybe (Context (Scoped ())) -> DeclHead (Scoped ()) -> QualConDecl (Scoped ()) -> Transform [Decl (Scoped ())]
smartCon cat ctx dh con = do
    funName <- Names.smartCon conName
    let pattern = PApp def (UnQual def funName) (PVar def <$> argNames)
    let typeBinding = TypeSig def [funName] (TyForall def Nothing (extendContext subAssertion ctx) funType)
    return $ [typeBinding, PatBind def pattern (UnGuardedRhs def expr) Nothing]
  where
    (conName, argTypes) = collectTypes con
    args = length argTypes
    argNames = (\arg -> Ident def $ "arg_" ++ show (arg)) <$> [1..args]
    internalType = TyVar def (Ident def "g")    
    replaceCat (TyCon def tycon) | void tycon == cat = internalType --TODO use name annotation CatReference instead
    replaceCat t                               = t
    funType = foldr (TyFun def) internalType (replaceCat <$> argTypes)
    buildPieceType (DHead _ name) = TyCon def (UnQual def name)
    buildPieceType (DHInfix _ typeVar name) = TyApp def (TyCon def (UnQual def name)) (TyVar def (tyVarName typeVar))
    buildPieceType (DHParen _ dh) = TyParen def (buildPieceType dh)
    buildPieceType (DHApp _ dh typeVar) = TyApp def (buildPieceType dh) (TyVar def (tyVarName typeVar))
    subAssertion = TypeA def (TyApp def (TyApp def (TyCon def partOfName) (buildPieceType dh)) internalType)
    expr = App def injectExp (foldl (App def) (Con def $ UnQual def conName) (Var def . UnQual def <$> argNames))

