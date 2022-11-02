{-# LANGUAGE FlexibleContexts #-}

module Utils.Decls(mapDecl) where

import Control.Monad.Except (MonadError, throwError, void)
import Language.Haskell.Exts.Syntax

class DeclMap a where
    mapDecl :: (MonadError String m) => (Decl l -> m [Decl l]) -> a l -> m (a l)

instance DeclMap Module where
    mapDecl f (Module l mmh ops iss dcls) = do
          Module l mmh <$> (mapDecl f `mapM` ops) <*> return iss <*> mapDecls f dcls
    mapDecl f (XmlPage l mn os xn xas me es) =
          XmlPage l mn <$> (mapDecl f `mapM` os) <*> return xn <*> (mapDecl f `mapM` xas) <*> (mapDecl f `mapM` me) <*> (mapDecl f `mapM` es)
    mapDecl f (XmlHybrid l mmh ops iss dcls xn xas me es) =
          XmlHybrid l mmh <$> (mapDecl f `mapM` ops) <*> return iss <*> mapDecls f dcls <*> return xn <*> (mapDecl f `mapM` xas) <*> (mapDecl f `mapM` me) <*> (mapDecl f `mapM` es)

mapDecl' :: (MonadError String m) => (Decl l -> m [Decl l]) -> Decl l -> m [Decl l]
mapDecl' f decl = f =<< case decl of
        TypeDecl     l dh t      -> TypeDecl    l <$> mapDecl f dh <*> mapDecl f t
        TypeFamDecl  l dh mk mi  -> TypeFamDecl l <$> mapDecl f dh <*> (mapDecl f `mapM` mk) <*> return mi
        ClosedTypeFamDecl  l dh mk mi eqns  -> ClosedTypeFamDecl l <$> mapDecl f dh <*> (mapDecl f `mapM` mk) <*> return mi <*> (mapDecl f `mapM` eqns)
        DataDecl     l dn mcx dh cds ders ->
            DataDecl l dn <$> (mapDecl f `mapM` mcx) <*> mapDecl f dh <*> (mapDecl f `mapM` cds) <*> (mapDecl f `mapM` ders)
        GDataDecl    l dn mcx dh mk gds ders ->
            GDataDecl l dn <$> (mapDecl f `mapM` mcx) <*> mapDecl f dh <*> (mapDecl f `mapM` mk) <*> (mapDecl f `mapM` gds) <*> (mapDecl f `mapM` ders)
        DataFamDecl  l mcx dh mk         -> DataFamDecl l <$> (mapDecl f `mapM` mcx) <*> mapDecl f dh <*> (mapDecl f `mapM` mk)
        TypeInsDecl  l t1 t2             -> TypeInsDecl l <$> mapDecl f t1 <*> mapDecl f t2
        DataInsDecl  l dn t cds ders     -> DataInsDecl l dn <$> mapDecl f t <*> (mapDecl f `mapM` cds) <*> (mapDecl f `mapM` ders)
        GDataInsDecl l dn t mk gds ders  -> GDataInsDecl l dn <$> mapDecl f t <*> (mapDecl f `mapM` mk) <*> (mapDecl f `mapM` gds) <*> (mapDecl f `mapM` ders)
        ClassDecl    l mcx dh fds cds    -> ClassDecl l <$> (mapDecl f `mapM` mcx) <*> mapDecl f dh <*> return fds <*> ((mapDecl f `mapM`) `mapM` cds)
        InstDecl     l mo ih ids         -> InstDecl l mo <$> mapDecl f ih <*> ((mapDecl f `mapM`) `mapM` ids)
        DerivDecl    l mds mo ih         -> DerivDecl l <$> (mapDecl f `mapM` mds) <*> return mo <*> mapDecl f ih
        InfixDecl    l a k ops           -> return $ InfixDecl l a k ops
        DefaultDecl  l ts                -> DefaultDecl l <$> (mapDecl f `mapM` ts)
        SpliceDecl   l sp                -> SpliceDecl l <$> mapDecl f sp
        TSpliceDecl  l sp                -> TSpliceDecl l <$> mapDecl f sp
        TypeSig      l ns t              -> TypeSig l ns <$> mapDecl f t
        PatSynSig    l n dh c1 dh2 c2 t  -> PatSynSig l n <$> ((mapDecl f `mapM`) `mapM` dh) <*> (mapDecl f `mapM` c1) <*> ((mapDecl f `mapM`) `mapM` dh2) <*> (mapDecl f `mapM` c2) <*> mapDecl f t
        FunBind      l ms                -> FunBind l <$> (mapDecl f `mapM` ms)
        PatBind      l p rhs bs          -> PatBind l <$> mapDecl f p <*> mapDecl f rhs <*> (mapDecl f `mapM` bs)
        PatSyn           l p r d         -> PatSyn l <$> mapDecl f p <*> mapDecl f r <*> mapDecl f d
        ForImp       l cc msf s n t      -> ForImp l cc msf s n <$> mapDecl f t
        ForExp       l cc     s n t      -> ForExp l cc     s n <$> mapDecl f t
        RulePragmaDecl   l rs            -> RulePragmaDecl l <$> (mapDecl f `mapM` rs)
        DeprPragmaDecl   l nss           -> return $ DeprPragmaDecl l nss
        WarnPragmaDecl   l nss           -> return $ WarnPragmaDecl l nss
        InlineSig        l b act qn      -> return $ InlineSig l b act qn
        InlineConlikeSig l   act qn      -> return $ InlineConlikeSig l act qn
        SpecSig          l   act qn ts   -> SpecSig       l   act qn <$> (mapDecl f `mapM` ts)
        SpecInlineSig    l b act qn ts   -> SpecInlineSig l b act qn <$> (mapDecl f `mapM` ts)
        InstSig          l ih            -> InstSig l <$> mapDecl f ih
        AnnPragma        l ann'          -> AnnPragma l <$> mapDecl f ann'
        MinimalPragma    l b             -> return $ MinimalPragma l b
        RoleAnnotDecl    l t rs          -> return $ RoleAnnotDecl l t rs
        CompletePragma   l cs ty         -> return $ CompletePragma l cs ty
        PieceDecl   l ca mcx dh cds      -> PieceDecl l ca <$> (mapDecl f `mapM` mcx) <*> mapDecl f dh <*> (mapDecl f `mapM` cds)
        PieceCatDecl l ca                -> return $ PieceCatDecl l ca
        CompFunDecl  l ns mcx ca t       -> CompFunDecl l ns <$> (mapDecl f `mapM` mcx) <*> return ca <*> mapDecl f t
        CompFunExt   l mcx fn ts pn ids  -> CompFunExt l <$> (mapDecl f `mapM` mcx) <*> return fn <*> mapDecl f `mapM` ts <*> mapDecl f pn <*> ((mapDecl f `mapM`) `mapM` ids)


mapDecls :: (MonadError String m) => (Decl l -> m [Decl l]) -> [Decl l] -> m [Decl l]
mapDecls f dcls = concat <$> (mapDecl' f `mapM` dcls)

instance DeclMap PatternSynDirection where
    mapDecl _ Unidirectional                 = return Unidirectional
    mapDecl _ ImplicitBidirectional          = return ImplicitBidirectional
    mapDecl f (ExplicitBidirectional l dcls) = ExplicitBidirectional l <$> mapDecls f dcls

instance DeclMap TypeEqn where
    mapDecl f (TypeEqn l a b) = TypeEqn l <$> mapDecl f a <*> mapDecl f b

instance DeclMap Annotation where
    mapDecl f (Ann     l n e) = Ann     l n <$> mapDecl f e
    mapDecl f (TypeAnn l n e) = TypeAnn l n <$> mapDecl f e
    mapDecl f (ModuleAnn l e) = ModuleAnn l <$> mapDecl f e

instance DeclMap ResultSig where
    mapDecl f (KindSig l k) = KindSig l <$> mapDecl f k
    mapDecl f (TyVarSig l tv) = TyVarSig l <$> mapDecl f tv

instance DeclMap DeclHead where
    mapDecl _ (DHead l n)           = return $ DHead l n
    mapDecl f (DHInfix l tva n)     = DHInfix l <$> mapDecl f tva <*> return n
    mapDecl f (DHParen l dh)        = DHParen l <$> mapDecl f dh
    mapDecl f (DHApp l dh t)        = DHApp l <$> mapDecl f dh <*> mapDecl f t

instance DeclMap InstRule where
    mapDecl f (IRule l mtv cxt qn)  = IRule l <$> ((mapDecl f `mapM`) `mapM` mtv) <*> (mapDecl f `mapM` cxt) <*> mapDecl f qn
    mapDecl f (IParen l ih)         = IParen l <$> mapDecl f ih

instance DeclMap InstHead where
    mapDecl _ (IHCon l n)           = return $ IHCon l n
    mapDecl f (IHInfix l tva n)     = IHInfix l <$> mapDecl f tva <*> return n
    mapDecl f (IHParen l dh)        = IHParen l <$> mapDecl f dh
    mapDecl f (IHApp l dh t)        = IHApp l <$> mapDecl f dh <*> mapDecl f t

instance DeclMap Deriving where
    mapDecl f (Deriving l mds ihs) = Deriving l <$> (mapDecl f `mapM` mds) <*> (mapDecl f `mapM` ihs)

instance DeclMap DerivStrategy where
    mapDecl _ (DerivStock l)    = return $ DerivStock l
    mapDecl _ (DerivAnyclass l) = return $ DerivAnyclass l
    mapDecl _ (DerivNewtype l)  = return $ DerivNewtype l
    mapDecl f (DerivVia l t)    = DerivVia l <$> mapDecl f t

instance DeclMap Binds where
    mapDecl f (BDecls  l decls) = BDecls l <$> mapDecls f decls
    mapDecl f (IPBinds l ibs)   = IPBinds l <$> (mapDecl f `mapM` ibs)

instance DeclMap IPBind where
    mapDecl f (IPBind l ipn e) = IPBind l ipn <$> mapDecl f e

instance DeclMap Match where
    mapDecl f (Match l n ps rhs bs) = Match l n <$> (mapDecl f `mapM` ps) <*> mapDecl f rhs <*> (mapDecl f `mapM` bs)
    mapDecl f (InfixMatch l a n b rhs bs) = InfixMatch l <$> mapDecl f a <*> return n <*> (mapDecl f `mapM` b) <*> mapDecl f rhs <*> (mapDecl f `mapM` bs)

instance DeclMap QualConDecl where
    mapDecl f (QualConDecl l tvs cx cd) = QualConDecl l <$> ((mapDecl f `mapM`) `mapM` tvs) <*> (mapDecl f `mapM` cx) <*> mapDecl f cd

instance DeclMap ConDecl where
    mapDecl f (ConDecl l n bts) = ConDecl l n <$> (mapDecl f `mapM` bts)
    mapDecl f (InfixConDecl l ta n tb) = InfixConDecl l <$> mapDecl f ta <*> return n <*> mapDecl f tb
    mapDecl f (RecDecl l n fds) = RecDecl l n <$> (mapDecl f `mapM` fds)

instance DeclMap FieldDecl where
    mapDecl f (FieldDecl l ns t) = FieldDecl l ns <$> mapDecl f t

instance DeclMap GadtDecl where
    mapDecl f (GadtDecl l n t1 t2 t3 t4) = GadtDecl l n <$> ((mapDecl f `mapM`) `mapM` t1) <*> (mapDecl f `mapM` t2) <*> ((mapDecl f `mapM`) `mapM` t3) <*> mapDecl f t4

instance DeclMap ClassDecl where
    mapDecl f (ClsDecl    l d)         = do
            dcls <- mapDecl' f d
            case dcls of
                [dcl] -> return $ ClsDecl l dcl
                _     -> throwError ("Did not get a singular declaration back when transforming class declaration " ++ show (void d))
    mapDecl f (ClsDataFam l mcx dh mk) = ClsDataFam l <$> (mapDecl f `mapM` mcx) <*> mapDecl f dh <*> (mapDecl f `mapM` mk)
    mapDecl f (ClsTyFam   l dh mk mi) = ClsTyFam l <$> mapDecl f dh <*> (mapDecl f `mapM` mk) <*> return mi
    mapDecl f (ClsTyDef   l t ) = ClsTyDef l <$> mapDecl f t
    mapDecl f (ClsDefSig  l n t) = ClsDefSig l n <$> mapDecl f t

instance DeclMap InstDecl where
    mapDecl f idecl = case idecl of
        InsDecl   l d           -> do
            dcls <- mapDecl' f d
            case dcls of
                [dcl] -> return $ InsDecl l dcl
                _     -> throwError ("Did not get a singular declaration back when transforming instance declaration " ++ show (void d))
        InsType   l t1 t2       -> InsType l <$> mapDecl f t1 <*> mapDecl f t2
        InsData   l dn t    cds ders -> InsData  l dn <$> mapDecl f t <*> (mapDecl f `mapM` cds) <*> (mapDecl f `mapM` ders)
        InsGData  l dn t mk gds ders -> InsGData l dn <$> mapDecl f t <*> (mapDecl f `mapM` mk) <*> (mapDecl f `mapM` gds) <*> (mapDecl f `mapM` ders)

instance DeclMap Rhs where
     mapDecl f (UnGuardedRhs l e)     = UnGuardedRhs l <$> mapDecl f e
     mapDecl f (GuardedRhss  l grhss) = GuardedRhss  l <$> (mapDecl f `mapM` grhss)

instance DeclMap GuardedRhs where
     mapDecl f (GuardedRhs l ss e) = GuardedRhs l <$> (mapDecl f `mapM` ss) <*> mapDecl f e

instance DeclMap Type where
    mapDecl f t1 = case t1 of
          TyForall l mtvs mcx t         -> TyForall l <$> ((mapDecl f `mapM`) `mapM` mtvs) <*> (mapDecl f `mapM` mcx) <*> mapDecl f t
          TyStar  l                     -> return $ TyStar l
          TyFun   l t1' t2              -> TyFun l <$> mapDecl f t1' <*> mapDecl f t2
          TyTuple l b ts                -> TyTuple l b <$> (mapDecl f `mapM` ts)
          TyUnboxedSum l s              -> TyUnboxedSum l <$> (mapDecl f `mapM` s)
          TyList  l t                   -> TyList l <$> mapDecl f t
          TyParArray  l t               -> TyParArray l <$> mapDecl f t
          TyApp   l t1' t2              -> TyApp l <$> mapDecl f t1' <*> mapDecl f t2
          TyVar   l n                   -> return $ TyVar l n
          TyCon   l qn                  -> return $ TyCon l qn
          TyParen l t                   -> TyParen l <$> mapDecl f t
          TyInfix l ta qn tb            -> TyInfix l <$> mapDecl f ta <*> return qn <*> mapDecl f tb
          TyKind  l t k                 -> TyKind l <$> mapDecl f t <*> mapDecl f k
          TyPromoted l   p              -> TyPromoted l <$> mapDecl f p
          TyEquals l a b                -> TyEquals l <$> mapDecl f a <*> mapDecl f b
          TySplice l s                  -> TySplice l <$> mapDecl f s
          TyBang l b u t                  -> TyBang l b u <$> mapDecl f t
          TyWildCard l n                -> return $ TyWildCard l n
          TyQuasiQuote l n s            -> return $ TyQuasiQuote l n s
          TyComp l c t                  -> TyComp l c <$> mapDecl f `mapM` t

instance DeclMap Constraint where
    mapDecl f c = case c of
        FunConstraint l qn t          -> return $ FunConstraint l qn t
        PieceConstraint l qn t        -> PieceConstraint l <$> mapDecl f qn <*> return t
        CategoryConstraint l qn t     -> return $ CategoryConstraint l qn t


instance DeclMap Promoted where
    mapDecl _ (PromotedInteger l int raw) = return $ PromotedInteger l int raw
    mapDecl _ (PromotedString l str raw) = return $ PromotedString l str raw
    mapDecl _ (PromotedCon l b qn)   = return $ PromotedCon l b qn
    mapDecl f (PromotedList l b ps)  = PromotedList  l b <$> (mapDecl f `mapM` ps)
    mapDecl f (PromotedTuple l ps) = PromotedTuple l <$> (mapDecl f `mapM` ps)
    mapDecl _ (PromotedUnit l)     = return $ PromotedUnit l

instance DeclMap TyVarBind where
    mapDecl f (KindedVar   l n k) = KindedVar   l n <$> mapDecl f k
    mapDecl _ (UnkindedVar l n)   = return $ UnkindedVar l n

instance DeclMap Context where
    mapDecl f (CxSingle l asst ) = CxSingle l <$> mapDecl f asst
    mapDecl f (CxTuple  l assts) = CxTuple  l <$> (mapDecl f `mapM` assts)
    mapDecl _ (CxEmpty l) = return $ CxEmpty l

instance DeclMap Asst where
    mapDecl f asst = case asst of
        TypeA l t           -> TypeA l <$> mapDecl f t
        IParam l ipn t      -> IParam l ipn <$> mapDecl f t
        ParenA l a          -> ParenA l <$> mapDecl f a
        CompCont l c        -> return $ CompCont l c

instance DeclMap Exp where
    mapDecl f e1 = case e1 of
        Var l qn        -> return $ Var l qn
        OverloadedLabel l qn -> return $ OverloadedLabel l qn
        IPVar l ipn     -> return $ IPVar l ipn
        Con l qn        -> return $ Con l qn
        Lit l lit       -> return $ Lit l lit
        InfixApp l e1' qop e2    -> InfixApp l <$> mapDecl f e1' <*> return qop <*> mapDecl f e2
        App l e1' e2    -> App l <$> mapDecl f e1' <*> mapDecl f e2
        NegApp l e      -> NegApp l <$> mapDecl f e
        Lambda l ps e   -> Lambda l <$> (mapDecl f `mapM` ps) <*> mapDecl f e
        Let l bs e      -> Let l <$> mapDecl f bs <*> mapDecl f e
        If l ec et ee   -> If l <$> mapDecl f ec <*> mapDecl f et <*> mapDecl f ee
        MultiIf l alts -> MultiIf l <$> (mapDecl f `mapM` alts)
        Case l e alts   -> Case l <$> mapDecl f e <*> (mapDecl f `mapM` alts)
        Do l ss         -> Do l <$> (mapDecl f `mapM` ss)
        MDo l ss        -> MDo l <$> (mapDecl f `mapM` ss)
        Tuple l bx es   -> Tuple l bx <$> (mapDecl f `mapM` es)
        UnboxedSum l b a es -> UnboxedSum l b a <$> mapDecl f es
        TupleSection l bx mes -> TupleSection l bx <$> ((mapDecl f `mapM`) `mapM` mes)
        List l es       -> List l <$> (mapDecl f `mapM` es)
        ParArray l es   -> ParArray l <$> (mapDecl f `mapM` es)
        Paren l e       -> Paren l <$> mapDecl f e
        LeftSection l e qop     -> LeftSection l <$> mapDecl f e <*> return qop
        RightSection l qop e    -> RightSection l qop <$> mapDecl f e
        RecConstr l qn fups     -> RecConstr l qn <$> (mapDecl f `mapM` fups)
        RecUpdate l e  fups     -> RecUpdate l <$> mapDecl f e <*> (mapDecl f `mapM` fups)
        EnumFrom l e            -> EnumFrom l <$> mapDecl f e
        EnumFromTo l ef et      -> EnumFromTo l <$> mapDecl f ef <*> mapDecl f et
        EnumFromThen l ef et    -> EnumFromThen l <$> mapDecl f ef <*> mapDecl f et
        EnumFromThenTo l ef eth eto -> EnumFromThenTo l <$> mapDecl f ef <*> mapDecl f eth <*> mapDecl f eto
        ParArrayFromTo l ef et  -> ParArrayFromTo l <$> mapDecl f ef <*> mapDecl f et
        ParArrayFromThenTo l ef eth eto -> ParArrayFromThenTo l <$> mapDecl f ef <*> mapDecl f eth <*> mapDecl f eto
        ListComp l e qss        -> ListComp l <$> mapDecl f e <*> (mapDecl f `mapM` qss)
        ParComp  l e qsss       -> ParComp  l <$> mapDecl f e <*> ((mapDecl f `mapM`) `mapM` qsss)
        ParArrayComp  l e qsss  -> ParArrayComp  l <$> mapDecl f e <*> ((mapDecl f `mapM`) `mapM` qsss)
        ExpTypeSig l e t        -> ExpTypeSig l <$> mapDecl f e <*> mapDecl f t
        VarQuote l qn           -> return $ VarQuote l qn
        TypQuote l qn           -> return $ TypQuote l qn
        BracketExp l br         -> BracketExp l <$> mapDecl f br
        SpliceExp l sp          -> SpliceExp l <$> mapDecl f sp
        QuasiQuote l sn se      -> return $ QuasiQuote l sn se
        TypeApp l t             -> TypeApp l <$> mapDecl f t

        XTag  l xn xas me es     -> XTag  l xn <$> (mapDecl f `mapM` xas) <*> (mapDecl f `mapM` me) <*> (mapDecl f `mapM` es)
        XETag l xn xas me        -> XETag l xn <$> (mapDecl f `mapM` xas) <*> (mapDecl f `mapM` me)
        XPcdata l s              -> return $ XPcdata l s
        XExpTag l e              -> XExpTag l <$> mapDecl f e
        XChildTag l es           -> XChildTag l <$> (mapDecl f `mapM` es)

        CorePragma l s e   -> CorePragma l s <$> mapDecl f e
        SCCPragma  l s e   -> SCCPragma l s <$> mapDecl f e
        GenPragma  l s n12 n34 e -> GenPragma l s n12 n34 <$> mapDecl f e

        Proc            l p e  -> Proc l <$> mapDecl f p <*> mapDecl f e
        LeftArrApp      l e1' e2 -> LeftArrApp      l <$> mapDecl f e1' <*> mapDecl f e2
        RightArrApp     l e1' e2 -> RightArrApp     l <$> mapDecl f e1' <*> mapDecl f e2
        LeftArrHighApp  l e1' e2 -> LeftArrHighApp  l <$> mapDecl f e1' <*> mapDecl f e2
        RightArrHighApp l e1' e2 -> RightArrHighApp l <$> mapDecl f e1' <*> mapDecl f e2
        ArrOp           l e      -> ArrOp           l <$> mapDecl f e

        LCase l alts -> LCase l <$> (mapDecl f `mapM` alts)

instance DeclMap XAttr where
    mapDecl f (XAttr l xn e) = XAttr l xn <$> mapDecl f e

instance DeclMap ModulePragma where
    mapDecl _ (LanguagePragma   l ns) = return $ LanguagePragma l ns
    mapDecl _ (OptionsPragma    l mt s) = return $ OptionsPragma l mt s
    mapDecl f (AnnModulePragma  l a) = AnnModulePragma l <$> mapDecl f a

instance DeclMap Bracket where
    mapDecl f (ExpBracket l e) = ExpBracket l <$> mapDecl f e
    mapDecl f (TExpBracket l e) = TExpBracket l <$> mapDecl f e
    mapDecl f (PatBracket l p) = PatBracket l <$> mapDecl f p
    mapDecl f (TypeBracket l t) = TypeBracket l <$> mapDecl f t
    mapDecl f (DeclBracket l ds) = DeclBracket l <$> mapDecls f ds

instance DeclMap Splice where
    mapDecl _ (IdSplice l s) = return $ IdSplice l s
    mapDecl _ (TIdSplice l s) = return $ TIdSplice l s
    mapDecl f (ParenSplice l e) = ParenSplice l <$> mapDecl f e
    mapDecl f (TParenSplice l e) = TParenSplice l <$> mapDecl f e

instance DeclMap Rule where
    mapDecl f (Rule l s act mrvs e1 e2) = Rule l s act <$> ((mapDecl f `mapM`) `mapM` mrvs) <*> mapDecl f e1 <*> mapDecl f e2

instance DeclMap RuleVar where
    mapDecl _ (RuleVar l n) = return $ RuleVar l n
    mapDecl f (TypedRuleVar l n t) = TypedRuleVar l n <$> mapDecl f t

instance DeclMap Pat where
    mapDecl f p1 = case p1 of
      PVar l n          -> return $ PVar l n
      PLit l sg lit     -> return $ PLit l sg lit
      PNPlusK l n k     -> return $ PNPlusK l n k
      PInfixApp l pa qn pb  -> PInfixApp l <$> mapDecl f pa <*> return qn <*> mapDecl f pb
      PApp l qn ps      -> PApp l qn <$> (mapDecl f `mapM` ps)
      PTuple l bx ps    -> PTuple l bx <$> (mapDecl f `mapM` ps)
      PUnboxedSum l b a ps -> PUnboxedSum l b a <$> mapDecl f ps
      PList l ps        -> PList l <$> (mapDecl f `mapM` ps)
      PParen l p        -> PParen l <$> mapDecl f p
      PRec l qn pfs     -> PRec l qn <$> (mapDecl f `mapM` pfs)
      PAsPat l n p      -> PAsPat l n <$> mapDecl f p
      PWildCard l       -> return $ PWildCard l
      PIrrPat l p       -> PIrrPat l <$> mapDecl f p
      PatTypeSig l p t  -> PatTypeSig l <$> mapDecl f p <*> mapDecl f t
      PViewPat l e p    -> PViewPat l <$> mapDecl f e <*> mapDecl f p
      PRPat l rps       -> PRPat l <$> (mapDecl f `mapM` rps)
      PXTag l xn pxas mp ps -> PXTag  l xn <$> (mapDecl f `mapM` pxas) <*> (mapDecl f `mapM` mp) <*> (mapDecl f `mapM` ps)
      PXETag l xn pxas mp   -> PXETag l xn <$> (mapDecl f `mapM` pxas) <*> (mapDecl f `mapM` mp)
      PXPcdata l s      -> return $ PXPcdata l s
      PXPatTag l p      -> PXPatTag l <$> mapDecl f p
      PXRPats  l rps    -> PXRPats  l <$> (mapDecl f `mapM` rps)
      PSplice l sp      -> PSplice l <$> mapDecl f sp
      PQuasiQuote l sn st   -> return $ PQuasiQuote l sn st
      PBangPat l p          -> PBangPat l <$> mapDecl f p

instance DeclMap PXAttr where
    mapDecl f (PXAttr l xn p) = PXAttr l xn <$> mapDecl f p

instance DeclMap RPat where
    mapDecl f rp1 = case rp1 of
      RPOp l rp rop         -> RPOp l <$> mapDecl f rp <*> return rop
      RPEither l rp1' rp2   -> RPEither l <$> mapDecl f rp1' <*> mapDecl f rp2
      RPSeq l rps           -> RPSeq l <$> (mapDecl f `mapM` rps)
      RPGuard l p ss        -> RPGuard l <$> mapDecl f p <*> (mapDecl f `mapM` ss)
      RPCAs l n rp          -> RPCAs l n <$> mapDecl f rp
      RPAs l n rp           -> RPAs l n <$> mapDecl f rp
      RPParen l rp          -> RPParen l <$> mapDecl f rp
      RPPat l p             -> RPPat l <$> mapDecl f p

instance DeclMap PatField where
    mapDecl f (PFieldPat l qn p) = PFieldPat l qn <$> mapDecl f p
    mapDecl _ (PFieldPun l n) = return $ PFieldPun l n
    mapDecl _ (PFieldWildcard l) = return $ PFieldWildcard l

instance DeclMap Stmt where
    mapDecl f (Generator l p e) = Generator l <$> mapDecl f p <*> mapDecl f e
    mapDecl f (Qualifier l e)   = Qualifier l <$> mapDecl f e
    mapDecl f (LetStmt l bs)    = LetStmt l <$> mapDecl f bs
    mapDecl f (RecStmt l ss)    = RecStmt l <$> (mapDecl f `mapM` ss)

instance DeclMap QualStmt where
    mapDecl f (QualStmt     l s) = QualStmt l <$> mapDecl f s
    mapDecl f (ThenTrans    l e) = ThenTrans l <$> mapDecl f e
    mapDecl f (ThenBy       l e1 e2) = ThenBy l <$> mapDecl f e1 <*> mapDecl f e2
    mapDecl f (GroupBy      l e) = GroupBy l <$> mapDecl f e
    mapDecl f (GroupUsing   l e) = GroupUsing l <$> mapDecl f e
    mapDecl f (GroupByUsing l e1 e2) = GroupByUsing l <$> mapDecl f e1 <*> mapDecl f e2

instance DeclMap FieldUpdate where
    mapDecl f (FieldUpdate l qn e) = FieldUpdate l qn <$> mapDecl f e
    mapDecl _ (FieldPun l n)       = return $ FieldPun l n
    mapDecl _ (FieldWildcard l)    = return $ FieldWildcard l

instance DeclMap Alt where
    mapDecl f (Alt l p gs bs) = Alt l <$> mapDecl f p <*> mapDecl f gs <*> (mapDecl f `mapM` bs)
