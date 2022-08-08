module Utils.Names (collectUniqueVars) where

import Language.Haskell.Exts

import           Data.Set   (Set)
import qualified Data.Set as Set

import Control.Monad (void)

collectUniqueVars :: Type l -> [Name l]
collectUniqueVars = removeDups Set.empty . collectVars
  where
    removeDups _   []         = []
    removeDups set (v:vars) =
      if Set.member (void v) set
        then removeDups set vars
        else v : removeDups (Set.insert (void v) set) vars

class WithVar a where
    collectVars :: a l -> [Name l]

instance WithVar Type where
    collectVars t1 = case t1 of
          TyForall _ mtvs      mcx t    -> concatMap (concatMap collectVars) mtvs 
                                            ++ concatMap collectVars mcx ++ collectVars t
          TyStar  _                     -> []
          TyFun   _ t1' t2              -> collectVars t1' ++ collectVars t2
          TyTuple _ _ ts                -> concatMap collectVars ts
          TyUnboxedSum _ s              -> concatMap collectVars s
          TyList  _ t                   -> collectVars t
          TyParArray  _ t               -> collectVars t
          TyApp   _ t1' t2              -> collectVars t1' ++ collectVars t2
          TyVar   _ n                   -> [n]
          TyCon   _ qn                  -> []
          TyParen _ t                   -> collectVars t
          TyInfix _ ta _ tb             -> collectVars ta ++ collectVars tb
          TyKind  _ t k                 -> undefined
          TyPromoted _   p              -> undefined
          TyEquals _ a b                -> undefined
          TySplice _ s                  -> undefined
          TyBang _ b u t                -> undefined
          TyWildCard _ n                -> undefined
          TyQuasiQuote _ n s            -> undefined
          TyComp _ c t                  -> []


instance WithVar TyVarBind where
    collectVars (KindedVar   _ n _k) = [n]
    collectVars (UnkindedVar _ n)    = [n]

instance WithVar Context where
    collectVars (CxSingle _ asst ) = collectVars asst
    collectVars (CxTuple  _ assts) = concatMap collectVars assts
    collectVars (CxEmpty _) = []

instance WithVar Asst where
    collectVars asst = case asst of
        TypeA _ t           -> collectVars t
        IParam _ ipn t      -> undefined
        ParenA _ a          -> collectVars a
        CompCont _ c        -> []
    
instance WithVar Constraint where
    collectVars _ = undefined
