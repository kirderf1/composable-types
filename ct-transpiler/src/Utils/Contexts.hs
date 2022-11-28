module Utils.Contexts(mapContext) where

import Language.Haskell.Exts.Syntax

type ContextTransform l m = Context l -> m (Context l)

class ContextMap a where
    mapContext :: (Monad m) => ContextTransform l m -> a l -> m (a l)

instance ContextMap Module where
    mapContext f (Module l mmh ops iss dcls) =
          Module l mmh <$> (mapContext f `mapM` ops) <*> return iss <*> (mapContext f `mapM` dcls)
    mapContext f (XmlPage l mn os xn xas me es) =
          XmlPage l mn <$> (mapContext f `mapM` os) <*> return xn <*> (mapContext f `mapM` xas) <*> (mapContext f `mapM` me) <*> (mapContext f `mapM` es)
    mapContext f (XmlHybrid l mmh ops iss dcls xn xas me es) =
          XmlHybrid l mmh <$> (mapContext f `mapM` ops) <*> return iss <*> (mapContext f `mapM` dcls) <*> return xn <*> (mapContext f `mapM` xas) <*> (mapContext f `mapM` me) <*> (mapContext f `mapM` es)

instance ContextMap Decl where
    mapContext f decl = case decl of
            TypeDecl     l dh t      -> TypeDecl    l <$> mapContext f dh <*> mapContext f t
            TypeFamDecl  l dh mk mi  -> TypeFamDecl l <$> mapContext f dh <*> (mapContext f `mapM` mk) <*> return mi
            ClosedTypeFamDecl  l dh mk mi eqns  -> ClosedTypeFamDecl l <$> mapContext f dh <*> (mapContext f `mapM` mk) <*> return mi <*> (mapContext f `mapM` eqns)
            DataDecl     l dn mcx dh cds ders ->
                DataDecl l dn <$> (mapContext f `mapM` mcx) <*> mapContext f dh <*> (mapContext f `mapM` cds) <*> (mapContext f `mapM` ders)
            GDataDecl    l dn mcx dh mk gds ders ->
                GDataDecl l dn <$> (mapContext f `mapM` mcx) <*> mapContext f dh <*> (mapContext f `mapM` mk) <*> (mapContext f `mapM` gds) <*> (mapContext f `mapM` ders)
            DataFamDecl  l mcx dh mk         -> DataFamDecl l <$> (mapContext f `mapM` mcx) <*> mapContext f dh <*> (mapContext f `mapM` mk)
            TypeInsDecl  l t1 t2             -> TypeInsDecl l <$> mapContext f t1 <*> mapContext f t2
            DataInsDecl  l dn t cds ders     -> DataInsDecl l dn <$> mapContext f t <*> (mapContext f `mapM` cds) <*> (mapContext f `mapM` ders)
            GDataInsDecl l dn t mk gds ders  -> GDataInsDecl l dn <$> mapContext f t <*> (mapContext f `mapM` mk) <*> (mapContext f `mapM` gds) <*> (mapContext f `mapM` ders)
            ClassDecl    l mcx dh fds cds    -> ClassDecl l <$> (mapContext f `mapM` mcx) <*> mapContext f dh <*> return fds <*> ((mapContext f `mapM`) `mapM` cds)
            InstDecl     l mo ih ids         -> InstDecl l mo <$> mapContext f ih <*> ((mapContext f `mapM`) `mapM` ids)
            DerivDecl    l mds mo ih         -> DerivDecl l <$> (mapContext f `mapM` mds) <*> return mo <*> mapContext f ih
            InfixDecl    l a k ops           -> return $ InfixDecl l a k ops
            DefaultDecl  l ts                -> DefaultDecl l <$> (mapContext f `mapM` ts)
            SpliceDecl   l sp                -> SpliceDecl l <$> mapContext f sp
            TSpliceDecl  l sp                -> TSpliceDecl l <$> mapContext f sp
            TypeSig      l ns t              -> TypeSig l ns <$> mapContext f t
            PatSynSig    l n dh c1 dh2 c2 t  -> PatSynSig l n <$> ((mapContext f `mapM`) `mapM` dh) <*> (mapContext f `mapM` c1) <*> ((mapContext f `mapM`) `mapM` dh2) <*> (mapContext f `mapM` c2) <*> mapContext f t
            FunBind      l ms                -> FunBind l <$> (mapContext f `mapM` ms)
            PatBind      l p rhs bs          -> PatBind l <$> mapContext f p <*> mapContext f rhs <*> (mapContext f `mapM` bs)
            PatSyn           l p r d         -> PatSyn l <$> mapContext f p <*> mapContext f r <*> mapContext f d
            ForImp       l cc msf s n t      -> ForImp l cc msf s n <$> mapContext f t
            ForExp       l cc     s n t      -> ForExp l cc     s n <$> mapContext f t
            RulePragmaDecl   l rs            -> RulePragmaDecl l <$> (mapContext f `mapM` rs)
            DeprPragmaDecl   l nss           -> return $ DeprPragmaDecl l nss
            WarnPragmaDecl   l nss           -> return $ WarnPragmaDecl l nss
            InlineSig        l b act qn      -> return $ InlineSig l b act qn
            InlineConlikeSig l   act qn      -> return $ InlineConlikeSig l act qn
            SpecSig          l   act qn ts   -> SpecSig       l   act qn <$> (mapContext f `mapM` ts)
            SpecInlineSig    l b act qn ts   -> SpecInlineSig l b act qn <$> (mapContext f `mapM` ts)
            InstSig          l ih            -> InstSig l <$> mapContext f ih
            AnnPragma        l ann'          -> AnnPragma l <$> mapContext f ann'
            MinimalPragma    l b             -> return $ MinimalPragma l b
            RoleAnnotDecl    l t rs          -> return $ RoleAnnotDecl l t rs
            CompletePragma   l cs ty         -> return $ CompletePragma l cs ty
            PieceDecl   l ca mcx dh cds      -> PieceDecl l ca <$> (mapContext f `mapM` mcx) <*> mapContext f dh <*> (mapContext f `mapM` cds)
            PieceCatDecl l ca                -> return $ PieceCatDecl l ca
            CompFunDecl  l ns mcx ca t       -> CompFunDecl l ns <$> (mapContext f `mapM` mcx) <*> return ca <*> mapContext f t
            CompFunExt   l mcx fn ts pn ids  -> CompFunExt l <$> (mapContext f `mapM` mcx) <*> return fn <*> mapContext f `mapM` ts <*> mapContext f pn <*> ((mapContext f `mapM`) `mapM` ids)
            

instance ContextMap PatternSynDirection where
    mapContext _ Unidirectional                 = return Unidirectional
    mapContext _ ImplicitBidirectional          = return ImplicitBidirectional
    mapContext f (ExplicitBidirectional l dcls) = ExplicitBidirectional l <$> (mapContext f `mapM` dcls)

instance ContextMap TypeEqn where
    mapContext f (TypeEqn l a b) = TypeEqn l <$> mapContext f a <*> mapContext f b

instance ContextMap Annotation where
    mapContext f (Ann     l n e) = Ann     l n <$> mapContext f e
    mapContext f (TypeAnn l n e) = TypeAnn l n <$> mapContext f e
    mapContext f (ModuleAnn l e) = ModuleAnn l <$> mapContext f e

instance ContextMap ResultSig where
    mapContext f (KindSig l k) = KindSig l <$> mapContext f k
    mapContext f (TyVarSig l tv) = TyVarSig l <$> mapContext f tv

instance ContextMap DeclHead where
    mapContext _ (DHead l n)           = return $ DHead l n
    mapContext f (DHInfix l tva n)     = DHInfix l <$> mapContext f tva <*> return n
    mapContext f (DHParen l dh)        = DHParen l <$> mapContext f dh
    mapContext f (DHApp l dh t)        = DHApp l <$> mapContext f dh <*> mapContext f t

instance ContextMap InstRule where
    mapContext f (IRule l mtv cxt qn)  = IRule l <$> ((mapContext f `mapM`) `mapM` mtv) <*> (mapContext f `mapM` cxt) <*> mapContext f qn
    mapContext f (IParen l ih)         = IParen l <$> mapContext f ih

instance ContextMap InstHead where
    mapContext _ (IHCon l n)           = return $ IHCon l n
    mapContext f (IHInfix l tva n)     = IHInfix l <$> mapContext f tva <*> return n
    mapContext f (IHParen l dh)        = IHParen l <$> mapContext f dh
    mapContext f (IHApp l dh t)        = IHApp l <$> mapContext f dh <*> mapContext f t

instance ContextMap Deriving where
    mapContext f (Deriving l mds ihs) = Deriving l <$> (mapContext f `mapM` mds) <*> (mapContext f `mapM` ihs)

instance ContextMap DerivStrategy where
    mapContext _ (DerivStock l)    = return $ DerivStock l
    mapContext _ (DerivAnyclass l) = return $ DerivAnyclass l
    mapContext _ (DerivNewtype l)  = return $ DerivNewtype l
    mapContext f (DerivVia l t)    = DerivVia l <$> mapContext f t

instance ContextMap Binds where
    mapContext f (BDecls  l decls) = BDecls l <$> (mapContext f `mapM` decls)
    mapContext f (IPBinds l ibs)   = IPBinds l <$> (mapContext f `mapM` ibs)

instance ContextMap IPBind where
    mapContext f (IPBind l ipn e) = IPBind l ipn <$> mapContext f e

instance ContextMap Match where
    mapContext f (Match l n ps rhs bs) = Match l n <$> (mapContext f `mapM` ps) <*> mapContext f rhs <*> (mapContext f `mapM` bs)
    mapContext f (InfixMatch l a n b rhs bs) = InfixMatch l <$> mapContext f a <*> return n <*> (mapContext f `mapM` b) <*> mapContext f rhs <*> (mapContext f `mapM` bs)

instance ContextMap QualConDecl where
    mapContext f (QualConDecl l tvs cx cd) = QualConDecl l <$> ((mapContext f `mapM`) `mapM` tvs) <*> (mapContext f `mapM` cx) <*> mapContext f cd

instance ContextMap ConDecl where
    mapContext f (ConDecl l n bts) = ConDecl l n <$> (mapContext f `mapM` bts)
    mapContext f (InfixConDecl l ta n tb) = InfixConDecl l <$> mapContext f ta <*> return n <*> mapContext f tb
    mapContext f (RecDecl l n fds) = RecDecl l n <$> (mapContext f `mapM` fds)

instance ContextMap FieldDecl where
    mapContext f (FieldDecl l ns t) = FieldDecl l ns <$> mapContext f t

instance ContextMap GadtDecl where
    mapContext f (GadtDecl l n t1 t2 t3 t4) = GadtDecl l n <$> ((mapContext f `mapM`) `mapM` t1) <*> (mapContext f `mapM` t2) <*> ((mapContext f `mapM`) `mapM` t3) <*> mapContext f t4

instance ContextMap ClassDecl where
    mapContext f (ClsDecl    l d) = ClsDecl l <$> mapContext f d
    mapContext f (ClsDataFam l mcx dh mk) = ClsDataFam l <$> (mapContext f `mapM` mcx) <*> mapContext f dh <*> (mapContext f `mapM` mk)
    mapContext f (ClsTyFam   l dh mk mi) = ClsTyFam l <$> mapContext f dh <*> (mapContext f `mapM` mk) <*> return mi
    mapContext f (ClsTyDef   l t ) = ClsTyDef l <$> mapContext f t
    mapContext f (ClsDefSig  l n t) = ClsDefSig l n <$> mapContext f t

instance ContextMap InstDecl where
    mapContext f idecl = case idecl of
        InsDecl   l d           -> InsDecl l <$> mapContext f d
        InsType   l t1 t2       -> InsType l <$> mapContext f t1 <*> mapContext f t2
        InsData   l dn t    cds ders -> InsData  l dn <$> mapContext f t <*> (mapContext f `mapM` cds) <*> (mapContext f `mapM` ders)
        InsGData  l dn t mk gds ders -> InsGData l dn <$> mapContext f t <*> (mapContext f `mapM` mk) <*> (mapContext f `mapM` gds) <*> (mapContext f `mapM` ders)

instance ContextMap Rhs where
     mapContext f (UnGuardedRhs l e)     = UnGuardedRhs l <$> mapContext f e
     mapContext f (GuardedRhss  l grhss) = GuardedRhss  l <$> (mapContext f `mapM` grhss)

instance ContextMap GuardedRhs where
     mapContext f (GuardedRhs l ss e) = GuardedRhs l <$> (mapContext f `mapM` ss) <*> mapContext f e

instance ContextMap Type where
    mapContext f t1 = case t1 of
          TyForall l mtvs mcx t         -> TyForall l <$> ((mapContext f `mapM`) `mapM` mtvs) <*> (mapContext f `mapM` mcx) <*> mapContext f t
          TyStar  l                     -> return $ TyStar l
          TyFun   l t1' t2              -> TyFun l <$> mapContext f t1' <*> mapContext f t2
          TyTuple l b ts                -> TyTuple l b <$> (mapContext f `mapM` ts)
          TyUnboxedSum l s              -> TyUnboxedSum l <$> (mapContext f `mapM` s)
          TyList  l t                   -> TyList l <$> mapContext f t
          TyParArray  l t               -> TyParArray l <$> mapContext f t
          TyApp   l t1' t2              -> TyApp l <$> mapContext f t1' <*> mapContext f t2
          TyVar   l n                   -> return $ TyVar l n
          TyCon   l qn                  -> return $ TyCon l qn
          TyParen l t                   -> TyParen l <$> mapContext f t
          TyInfix l ta qn tb            -> TyInfix l <$> mapContext f ta <*> return qn <*> mapContext f tb
          TyKind  l t k                 -> TyKind l <$> mapContext f t <*> mapContext f k
          TyPromoted l   p              -> TyPromoted l <$> mapContext f p
          TyEquals l a b                -> TyEquals l <$> mapContext f a <*> mapContext f b
          TySplice l s                  -> TySplice l <$> mapContext f s
          TyBang l b u t                  -> TyBang l b u <$> mapContext f t
          TyWildCard l n                -> return $ TyWildCard l n
          TyQuasiQuote l n s            -> return $ TyQuasiQuote l n s
          TyComp l c t                  -> TyComp l c <$>  mapContext f `mapM` t

instance ContextMap Constraint where
    mapContext f c = case c of
        FunConstraint l qn ts t       -> FunConstraint l qn <$> mapContext f `mapM` ts <*> return t
        PieceConstraint l qn t        -> PieceConstraint l <$> mapContext f qn <*> return t
        CategoryConstraint l qn t     -> return $ CategoryConstraint l qn t

instance ContextMap Promoted where
    mapContext _ (PromotedInteger l int raw) = return $ PromotedInteger l int raw
    mapContext _ (PromotedString l str raw) = return $ PromotedString l str raw
    mapContext _ (PromotedCon l b qn)   = return $ PromotedCon l b qn
    mapContext f (PromotedList l b ps)  = PromotedList  l b <$> (mapContext f `mapM` ps)
    mapContext f (PromotedTuple l ps) = PromotedTuple l <$> (mapContext f `mapM` ps)
    mapContext _ (PromotedUnit l)     = return $ PromotedUnit l

instance ContextMap TyVarBind where
    mapContext f (KindedVar   l n k) = KindedVar   l n <$> mapContext f k
    mapContext _ (UnkindedVar l n)   = return $ UnkindedVar l n

instance ContextMap Context where
    mapContext f cx = f =<< case cx of 
          CxSingle l asst  -> CxSingle l <$> mapContext f asst
          CxTuple  l assts -> CxTuple  l <$> (mapContext f `mapM` assts)
          CxEmpty l        -> return $ CxEmpty l

instance ContextMap Asst where
    mapContext f asst = case asst of
        TypeA l t           -> TypeA l <$> mapContext f t
        IParam l ipn t      -> IParam l ipn <$> mapContext f t
        ParenA l a          -> ParenA l <$> mapContext f a
        CompCont l c        -> CompCont l <$> mapContext f c

instance ContextMap Exp where
    mapContext f e1 = case e1 of
        Var l qn        -> return $ Var l qn
        OverloadedLabel l qn -> return $ OverloadedLabel l qn
        IPVar l ipn     -> return $ IPVar l ipn
        Con l qn        -> return $ Con l qn
        Lit l lit       -> return $ Lit l lit
        InfixApp l e1' qop e2    -> InfixApp l <$> mapContext f e1' <*> return qop <*> mapContext f e2
        App l e1' e2    -> App l <$> mapContext f e1' <*> mapContext f e2
        NegApp l e      -> NegApp l <$> mapContext f e
        Lambda l ps e   -> Lambda l <$> (mapContext f `mapM` ps) <*> mapContext f e
        Let l bs e      -> Let l <$> mapContext f bs <*> mapContext f e
        If l ec et ee   -> If l <$> mapContext f ec <*> mapContext f et <*> mapContext f ee
        MultiIf l alts -> MultiIf l <$> (mapContext f `mapM` alts)
        Case l e alts   -> Case l <$> mapContext f e <*> (mapContext f `mapM` alts)
        Do l ss         -> Do l <$> (mapContext f `mapM` ss)
        MDo l ss        -> MDo l <$> (mapContext f `mapM` ss)
        Tuple l bx es   -> Tuple l bx <$> (mapContext f `mapM` es)
        UnboxedSum l b a es -> UnboxedSum l b a <$> mapContext f es
        TupleSection l bx mes -> TupleSection l bx <$> ((mapContext f `mapM`) `mapM` mes)
        List l es       -> List l <$> (mapContext f `mapM` es)
        ParArray l es   -> ParArray l <$> (mapContext f `mapM` es)
        Paren l e       -> Paren l <$> mapContext f e
        LeftSection l e qop     -> LeftSection l <$> mapContext f e <*> return qop
        RightSection l qop e    -> RightSection l qop <$> mapContext f e
        RecConstr l qn fups     -> RecConstr l qn <$> (mapContext f `mapM` fups)
        RecUpdate l e  fups     -> RecUpdate l <$> mapContext f e <*> (mapContext f `mapM` fups)
        EnumFrom l e            -> EnumFrom l <$> mapContext f e
        EnumFromTo l ef et      -> EnumFromTo l <$> mapContext f ef <*> mapContext f et
        EnumFromThen l ef et    -> EnumFromThen l <$> mapContext f ef <*> mapContext f et
        EnumFromThenTo l ef eth eto -> EnumFromThenTo l <$> mapContext f ef <*> mapContext f eth <*> mapContext f eto
        ParArrayFromTo l ef et  -> ParArrayFromTo l <$> mapContext f ef <*> mapContext f et
        ParArrayFromThenTo l ef eth eto -> ParArrayFromThenTo l <$> mapContext f ef <*> mapContext f eth <*> mapContext f eto
        ListComp l e qss        -> ListComp l <$> mapContext f e <*> (mapContext f `mapM` qss)
        ParComp  l e qsss       -> ParComp  l <$> mapContext f e <*> ((mapContext f `mapM`) `mapM` qsss)
        ParArrayComp  l e qsss  -> ParArrayComp  l <$> mapContext f e <*> ((mapContext f `mapM`) `mapM` qsss)
        ExpTypeSig l e t        -> ExpTypeSig l <$> mapContext f e <*> mapContext f t
        VarQuote l qn           -> return $ VarQuote l qn
        TypQuote l qn           -> return $ TypQuote l qn
        BracketExp l br         -> BracketExp l <$> mapContext f br
        SpliceExp l sp          -> SpliceExp l <$> mapContext f sp
        QuasiQuote l sn se      -> return $ QuasiQuote l sn se
        TypeApp l t             -> TypeApp l <$> mapContext f t

        XTag  l xn xas me es     -> XTag  l xn <$> (mapContext f `mapM` xas) <*> (mapContext f `mapM` me) <*> (mapContext f `mapM` es)
        XETag l xn xas me        -> XETag l xn <$> (mapContext f `mapM` xas) <*> (mapContext f `mapM` me)
        XPcdata l s              -> return $ XPcdata l s
        XExpTag l e              -> XExpTag l <$> mapContext f e
        XChildTag l es           -> XChildTag l <$> (mapContext f `mapM` es)

        CorePragma l s e   -> CorePragma l s <$> mapContext f e
        SCCPragma  l s e   -> SCCPragma l s <$> mapContext f e
        GenPragma  l s n12 n34 e -> GenPragma l s n12 n34 <$> mapContext f e

        Proc            l p e  -> Proc l <$> mapContext f p <*> mapContext f e
        LeftArrApp      l e1' e2 -> LeftArrApp      l <$> mapContext f e1' <*> mapContext f e2
        RightArrApp     l e1' e2 -> RightArrApp     l <$> mapContext f e1' <*> mapContext f e2
        LeftArrHighApp  l e1' e2 -> LeftArrHighApp  l <$> mapContext f e1' <*> mapContext f e2
        RightArrHighApp l e1' e2 -> RightArrHighApp l <$> mapContext f e1' <*> mapContext f e2
        ArrOp           l e      -> ArrOp           l <$> mapContext f e

        LCase l alts -> LCase l <$> (mapContext f `mapM` alts)

instance ContextMap XAttr where
    mapContext f (XAttr l xn e) = XAttr l xn <$> mapContext f e

instance ContextMap ModulePragma where
    mapContext _ (LanguagePragma   l ns) = return $ LanguagePragma l ns
    mapContext _ (OptionsPragma    l mt s) = return $ OptionsPragma l mt s
    mapContext f (AnnModulePragma  l a) = AnnModulePragma l <$> mapContext f a

instance ContextMap Bracket where
    mapContext f (ExpBracket l e) = ExpBracket l <$> mapContext f e
    mapContext f (TExpBracket l e) = TExpBracket l <$> mapContext f e
    mapContext f (PatBracket l p) = PatBracket l <$> mapContext f p
    mapContext f (TypeBracket l t) = TypeBracket l <$> mapContext f t
    mapContext f (DeclBracket l ds) = DeclBracket l <$> (mapContext f `mapM` ds)

instance ContextMap Splice where
    mapContext _ (IdSplice l s) = return $ IdSplice l s
    mapContext _ (TIdSplice l s) = return $ TIdSplice l s
    mapContext f (ParenSplice l e) = ParenSplice l <$> mapContext f e
    mapContext f (TParenSplice l e) = TParenSplice l <$> mapContext f e

instance ContextMap Rule where
    mapContext f (Rule l s act mrvs e1 e2) = Rule l s act <$> ((mapContext f `mapM`) `mapM` mrvs) <*> mapContext f e1 <*> mapContext f e2

instance ContextMap RuleVar where
    mapContext _ (RuleVar l n) = return $ RuleVar l n
    mapContext f (TypedRuleVar l n t) = TypedRuleVar l n <$> mapContext f t

instance ContextMap Pat where
    mapContext f p1 = case p1 of
      PVar l n          -> return $ PVar l n
      PLit l sg lit     -> return $ PLit l sg lit
      PNPlusK l n k     -> return $ PNPlusK l n k
      PInfixApp l pa qn pb  -> PInfixApp l <$> mapContext f pa <*> return qn <*> mapContext f pb
      PApp l qn ps      -> PApp l qn <$> (mapContext f `mapM` ps)
      PTuple l bx ps    -> PTuple l bx <$> (mapContext f `mapM` ps)
      PUnboxedSum l b a ps -> PUnboxedSum l b a <$> mapContext f ps
      PList l ps        -> PList l <$> (mapContext f `mapM` ps)
      PParen l p        -> PParen l <$> mapContext f p
      PRec l qn pfs     -> PRec l qn <$> (mapContext f `mapM` pfs)
      PAsPat l n p      -> PAsPat l n <$> mapContext f p
      PWildCard l       -> return $ PWildCard l
      PIrrPat l p       -> PIrrPat l <$> mapContext f p
      PatTypeSig l p t  -> PatTypeSig l <$> mapContext f p <*> mapContext f t
      PViewPat l e p    -> PViewPat l <$> mapContext f e <*> mapContext f p
      PRPat l rps       -> PRPat l <$> (mapContext f `mapM` rps)
      PXTag l xn pxas mp ps -> PXTag  l xn <$> (mapContext f `mapM` pxas) <*> (mapContext f `mapM` mp) <*> (mapContext f `mapM` ps)
      PXETag l xn pxas mp   -> PXETag l xn <$> (mapContext f `mapM` pxas) <*> (mapContext f `mapM` mp)
      PXPcdata l s      -> return $ PXPcdata l s
      PXPatTag l p      -> PXPatTag l <$> mapContext f p
      PXRPats  l rps    -> PXRPats  l <$> (mapContext f `mapM` rps)
      PSplice l sp      -> PSplice l <$> mapContext f sp
      PQuasiQuote l sn st   -> return $ PQuasiQuote l sn st
      PBangPat l p          -> PBangPat l <$> mapContext f p

instance ContextMap PXAttr where
    mapContext f (PXAttr l xn p) = PXAttr l xn <$> mapContext f p

instance ContextMap RPat where
    mapContext f rp1 = case rp1 of
      RPOp l rp rop         -> RPOp l <$> mapContext f rp <*> return rop
      RPEither l rp1' rp2   -> RPEither l <$> mapContext f rp1' <*> mapContext f rp2
      RPSeq l rps           -> RPSeq l <$> (mapContext f `mapM` rps)
      RPGuard l p ss        -> RPGuard l <$> mapContext f p <*> (mapContext f `mapM` ss)
      RPCAs l n rp          -> RPCAs l n <$> mapContext f rp
      RPAs l n rp           -> RPAs l n <$> mapContext f rp
      RPParen l rp          -> RPParen l <$> mapContext f rp
      RPPat l p             -> RPPat l <$> mapContext f p

instance ContextMap PatField where
    mapContext f (PFieldPat l qn p) = PFieldPat l qn <$> mapContext f p
    mapContext _ (PFieldPun l n) = return $ PFieldPun l n
    mapContext _ (PFieldWildcard l) = return $ PFieldWildcard l

instance ContextMap Stmt where
    mapContext f (Generator l p e) = Generator l <$> mapContext f p <*> mapContext f e
    mapContext f (Qualifier l e)   = Qualifier l <$> mapContext f e
    mapContext f (LetStmt l bs)    = LetStmt l <$> mapContext f bs
    mapContext f (RecStmt l ss)    = RecStmt l <$> (mapContext f `mapM` ss)

instance ContextMap QualStmt where
    mapContext f (QualStmt     l s) = QualStmt l <$> mapContext f s
    mapContext f (ThenTrans    l e) = ThenTrans l <$> mapContext f e
    mapContext f (ThenBy       l e1 e2) = ThenBy l <$> mapContext f e1 <*> mapContext f e2
    mapContext f (GroupBy      l e) = GroupBy l <$> mapContext f e
    mapContext f (GroupUsing   l e) = GroupUsing l <$> mapContext f e
    mapContext f (GroupByUsing l e1 e2) = GroupByUsing l <$> mapContext f e1 <*> mapContext f e2

instance ContextMap FieldUpdate where
    mapContext f (FieldUpdate l qn e) = FieldUpdate l qn <$> mapContext f e
    mapContext _ (FieldPun l n)       = return $ FieldPun l n
    mapContext _ (FieldWildcard l)    = return $ FieldWildcard l

instance ContextMap Alt where
    mapContext f (Alt l p gs bs) = Alt l <$> mapContext f p <*> mapContext f gs <*> (mapContext f `mapM` bs)
