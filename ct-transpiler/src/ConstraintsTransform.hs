

module ConstraintsTransform(transformContext) where

import Language.Haskell.Exts
import TransformUtils

import Data.Maybe (catMaybes)
import Data.Default

import qualified GeneratedNames as Names

transformContext :: Default l => Context l -> Transform (Context l)
transformContext (CxEmpty _) = return (CxEmpty def)
transformContext (CxSingle _ asst) = transformContext' [asst]     
transformContext (CxTuple _ assts) = transformContext' assts

transformContext' :: Default l => [Asst l] -> Transform (Context l)
transformContext' assts = do
    assts' <- mapM transformAsst assts 
    return (contextFromList (catMaybes assts'))

-- | Transform constraint to assertion
constraintToAsst :: Default l => Constraint l -> Transform (Maybe (Asst l))
constraintToAsst (FunConstraint _ fun types v) = do
    cname <- Names.qOuterClass fun
    let asstType = foldl (TyApp def) (TyCon def cname) (TyVar def v : types)
    return $ (Just (TypeA def asstType))
constraintToAsst (PieceConstraint _ pieceref v) = return $ (Just (TypeA def (TyApp def 
    (TyApp def (TyCon def partOfName) (pieceRefAsType pieceref)) (TyVar def v))))
constraintToAsst (CategoryConstraint _ _category _v) = return (Nothing)

transformAsst :: Default l => Asst l -> Transform (Maybe (Asst l))
transformAsst (CompCont _ constraint) = constraintToAsst constraint
transformAsst (ParenA _ asst) = do 
    masst' <- transformAsst asst
    case masst' of
         Just asst' -> return (Just (ParenA def asst'))
         Nothing -> return Nothing
transformAsst asst = return (Just asst)

