module Utils.Exps(mapExp) where

import Language.Haskell.Exts.Syntax

class ExpMap a where
    mapExp :: (Monad m) => (Exp l -> m (Exp l)) -> a l -> m (a l)

instance ExpMap Module where
    mapExp f (Module l mmh ops iss dcls) =
          Module l mmh <$> (mapExp f `mapM` ops) <*> return iss <*> (mapExp f `mapM` dcls)
    mapExp f (XmlPage l mn os xn xas me es) =
          XmlPage l mn <$> (mapExp f `mapM` os) <*> return xn <*> (mapExp f `mapM` xas) <*> (mapExp f `mapM` me) <*> (mapExp f `mapM` es)
    mapExp f (XmlHybrid l mmh ops iss dcls xn xas me es) =
          XmlHybrid l mmh <$> (mapExp f `mapM` ops) <*> return iss <*> (mapExp f `mapM` dcls) <*> return xn <*> (mapExp f `mapM` xas) <*> (mapExp f `mapM` me) <*> (mapExp f `mapM` es)

instance ExpMap Decl where
    mapExp f decl = case decl of
            TypeDecl     l dh t      -> TypeDecl    l <$> mapExp f dh <*> mapExp f t
            TypeFamDecl  l dh mk mi  -> TypeFamDecl l <$> mapExp f dh <*> (mapExp f `mapM` mk) <*> return mi
            ClosedTypeFamDecl  l dh mk mi eqns  -> ClosedTypeFamDecl l <$> mapExp f dh <*> (mapExp f `mapM` mk) <*> return mi <*> (mapExp f `mapM` eqns)
            DataDecl     l dn mcx dh cds ders ->
                DataDecl l dn <$> (mapExp f `mapM` mcx) <*> mapExp f dh <*> (mapExp f `mapM` cds) <*> (mapExp f `mapM` ders)
            GDataDecl    l dn mcx dh mk gds ders ->
                GDataDecl l dn <$> (mapExp f `mapM` mcx) <*> mapExp f dh <*> (mapExp f `mapM` mk) <*> (mapExp f `mapM` gds) <*> (mapExp f `mapM` ders)
            DataFamDecl  l mcx dh mk         -> DataFamDecl l <$> (mapExp f `mapM` mcx) <*> mapExp f dh <*> (mapExp f `mapM` mk)
            TypeInsDecl  l t1 t2             -> TypeInsDecl l <$> mapExp f t1 <*> mapExp f t2
            DataInsDecl  l dn t cds ders     -> DataInsDecl l dn <$> mapExp f t <*> (mapExp f `mapM` cds) <*> (mapExp f `mapM` ders)
            GDataInsDecl l dn t mk gds ders  -> GDataInsDecl l dn <$> mapExp f t <*> (mapExp f `mapM` mk) <*> (mapExp f `mapM` gds) <*> (mapExp f `mapM` ders)
            ClassDecl    l mcx dh fds cds    -> ClassDecl l <$> (mapExp f `mapM` mcx) <*> mapExp f dh <*> return fds <*> ((mapExp f `mapM`) `mapM` cds)
            InstDecl     l mo ih ids         -> InstDecl l mo <$> mapExp f ih <*> ((mapExp f `mapM`) `mapM` ids)
            DerivDecl    l mds mo ih         -> DerivDecl l <$> (mapExp f `mapM` mds) <*> return mo <*> mapExp f ih
            InfixDecl    l a k ops           -> return $ InfixDecl l a k ops
            DefaultDecl  l ts                -> DefaultDecl l <$> (mapExp f `mapM` ts)
            SpliceDecl   l sp                -> SpliceDecl l <$> mapExp f sp
            TSpliceDecl  l sp                -> TSpliceDecl l <$> mapExp f sp
            TypeSig      l ns t              -> TypeSig l ns <$> mapExp f t
            PatSynSig    l n dh c1 dh2 c2 t  -> PatSynSig l n <$> ((mapExp f `mapM`) `mapM` dh) <*> (mapExp f `mapM` c1) <*> ((mapExp f `mapM`) `mapM` dh2) <*> (mapExp f `mapM` c2) <*> mapExp f t
            FunBind      l ms                -> FunBind l <$> (mapExp f `mapM` ms)
            PatBind      l p rhs bs          -> PatBind l <$> mapExp f p <*> mapExp f rhs <*> (mapExp f `mapM` bs)
            PatSyn           l p r d         -> PatSyn l <$> mapExp f p <*> mapExp f r <*> mapExp f d
            ForImp       l cc msf s n t      -> ForImp l cc msf s n <$> mapExp f t
            ForExp       l cc     s n t      -> ForExp l cc     s n <$> mapExp f t
            RulePragmaDecl   l rs            -> RulePragmaDecl l <$> (mapExp f `mapM` rs)
            DeprPragmaDecl   l nss           -> return $ DeprPragmaDecl l nss
            WarnPragmaDecl   l nss           -> return $ WarnPragmaDecl l nss
            InlineSig        l b act qn      -> return $ InlineSig l b act qn
            InlineConlikeSig l   act qn      -> return $ InlineConlikeSig l act qn
            SpecSig          l   act qn ts   -> SpecSig       l   act qn <$> (mapExp f `mapM` ts)
            SpecInlineSig    l b act qn ts   -> SpecInlineSig l b act qn <$> (mapExp f `mapM` ts)
            InstSig          l ih            -> InstSig l <$> mapExp f ih
            AnnPragma        l ann'          -> AnnPragma l <$> mapExp f ann'
            MinimalPragma    l b             -> return $ MinimalPragma l b
            RoleAnnotDecl    l t rs          -> return $ RoleAnnotDecl l t rs
            CompletePragma   l cs ty         -> return $ CompletePragma l cs ty
            PieceDecl   l ca dh cds          -> PieceDecl l ca dh <$> (mapExp f `mapM` cds)
            PieceCatDecl l ca                -> return $ PieceCatDecl l ca
            CompFunDecl  l ns mcx ca t       -> CompFunDecl l ns <$> (mapExp f `mapM` mcx) <*> return ca <*> mapExp f t
            CompFunExt   l mcx fn ts pn ids  -> CompFunExt l <$> (mapExp f `mapM` mcx) <*> return fn <*> mapExp f `mapM` ts <*> return pn <*> ((mapExp f `mapM`) `mapM` ids)
            

instance ExpMap PatternSynDirection where
    mapExp _ Unidirectional                 = return Unidirectional
    mapExp _ ImplicitBidirectional          = return ImplicitBidirectional
    mapExp f (ExplicitBidirectional l dcls) = ExplicitBidirectional l <$> (mapExp f `mapM` dcls)

instance ExpMap TypeEqn where
    mapExp f (TypeEqn l a b) = TypeEqn l <$> mapExp f a <*> mapExp f b

instance ExpMap Annotation where
    mapExp f (Ann     l n e) = Ann     l n <$> mapExp f e
    mapExp f (TypeAnn l n e) = TypeAnn l n <$> mapExp f e
    mapExp f (ModuleAnn l e) = ModuleAnn l <$> mapExp f e

instance ExpMap ResultSig where
    mapExp f (KindSig l k) = KindSig l <$> mapExp f k
    mapExp f (TyVarSig l tv) = TyVarSig l <$> mapExp f tv

instance ExpMap DeclHead where
    mapExp _ (DHead l n)           = return $ DHead l n
    mapExp f (DHInfix l tva n)     = DHInfix l <$> mapExp f tva <*> return n
    mapExp f (DHParen l dh)        = DHParen l <$> mapExp f dh
    mapExp f (DHApp l dh t)        = DHApp l <$> mapExp f dh <*> mapExp f t

instance ExpMap InstRule where
    mapExp f (IRule l mtv cxt qn)  = IRule l <$> ((mapExp f `mapM`) `mapM` mtv) <*> (mapExp f `mapM` cxt) <*> mapExp f qn
    mapExp f (IParen l ih)         = IParen l <$> mapExp f ih

instance ExpMap InstHead where
    mapExp _ (IHCon l n)           = return $ IHCon l n
    mapExp f (IHInfix l tva n)     = IHInfix l <$> mapExp f tva <*> return n
    mapExp f (IHParen l dh)        = IHParen l <$> mapExp f dh
    mapExp f (IHApp l dh t)        = IHApp l <$> mapExp f dh <*> mapExp f t

instance ExpMap Deriving where
    mapExp f (Deriving l mds ihs) = Deriving l <$> (mapExp f `mapM` mds) <*> (mapExp f `mapM` ihs)

instance ExpMap DerivStrategy where
    mapExp _ (DerivStock l)    = return $ DerivStock l
    mapExp _ (DerivAnyclass l) = return $ DerivAnyclass l
    mapExp _ (DerivNewtype l)  = return $ DerivNewtype l
    mapExp f (DerivVia l t)    = DerivVia l <$> mapExp f t

instance ExpMap Binds where
    mapExp f (BDecls  l decls) = BDecls l <$> (mapExp f `mapM` decls)
    mapExp f (IPBinds l ibs)   = IPBinds l <$> (mapExp f `mapM` ibs)

instance ExpMap IPBind where
    mapExp f (IPBind l ipn e) = IPBind l ipn <$> mapExp f e

instance ExpMap Match where
    mapExp f (Match l n ps rhs bs) = Match l n <$> (mapExp f `mapM` ps) <*> mapExp f rhs <*> (mapExp f `mapM` bs)
    mapExp f (InfixMatch l a n b rhs bs) = InfixMatch l <$> mapExp f a <*> return n <*> (mapExp f `mapM` b) <*> mapExp f rhs <*> (mapExp f `mapM` bs)

instance ExpMap QualConDecl where
    mapExp f (QualConDecl l tvs cx cd) = QualConDecl l <$> ((mapExp f `mapM`) `mapM` tvs) <*> (mapExp f `mapM` cx) <*> mapExp f cd

instance ExpMap ConDecl where
    mapExp f (ConDecl l n bts) = ConDecl l n <$> (mapExp f `mapM` bts)
    mapExp f (InfixConDecl l ta n tb) = InfixConDecl l <$> mapExp f ta <*> return n <*> mapExp f tb
    mapExp f (RecDecl l n fds) = RecDecl l n <$> (mapExp f `mapM` fds)

instance ExpMap FieldDecl where
    mapExp f (FieldDecl l ns t) = FieldDecl l ns <$> mapExp f t

instance ExpMap GadtDecl where
    mapExp f (GadtDecl l n t1 t2 t3 t4) = GadtDecl l n <$> ((mapExp f `mapM`) `mapM` t1) <*> (mapExp f `mapM` t2) <*> ((mapExp f `mapM`) `mapM` t3) <*> mapExp f t4

instance ExpMap ClassDecl where
    mapExp f (ClsDecl    l d) = ClsDecl l <$> mapExp f d
    mapExp f (ClsDataFam l mcx dh mk) = ClsDataFam l <$> (mapExp f `mapM` mcx) <*> mapExp f dh <*> (mapExp f `mapM` mk)
    mapExp f (ClsTyFam   l dh mk mi) = ClsTyFam l <$> mapExp f dh <*> (mapExp f `mapM` mk) <*> return mi
    mapExp f (ClsTyDef   l t ) = ClsTyDef l <$> mapExp f t
    mapExp f (ClsDefSig  l n t) = ClsDefSig l n <$> mapExp f t

instance ExpMap InstDecl where
    mapExp f idecl = case idecl of
        InsDecl   l d           -> InsDecl l <$> mapExp f d
        InsType   l t1 t2       -> InsType l <$> mapExp f t1 <*> mapExp f t2
        InsData   l dn t    cds ders -> InsData  l dn <$> mapExp f t <*> (mapExp f `mapM` cds) <*> (mapExp f `mapM` ders)
        InsGData  l dn t mk gds ders -> InsGData l dn <$> mapExp f t <*> (mapExp f `mapM` mk) <*> (mapExp f `mapM` gds) <*> (mapExp f `mapM` ders)

instance ExpMap Rhs where
     mapExp f (UnGuardedRhs l e)     = UnGuardedRhs l <$> mapExp f e
     mapExp f (GuardedRhss  l grhss) = GuardedRhss  l <$> (mapExp f `mapM` grhss)

instance ExpMap GuardedRhs where
     mapExp f (GuardedRhs l ss e) = GuardedRhs l <$> (mapExp f `mapM` ss) <*> mapExp f e

instance ExpMap Type where
    mapExp f t1 = case t1 of
          TyForall l mtvs mcx t         -> TyForall l <$> ((mapExp f `mapM`) `mapM` mtvs) <*> (mapExp f `mapM` mcx) <*> mapExp f t
          TyStar  l                     -> return $ TyStar l
          TyFun   l t1' t2              -> TyFun l <$> mapExp f t1' <*> mapExp f t2
          TyTuple l b ts                -> TyTuple l b <$> (mapExp f `mapM` ts)
          TyUnboxedSum l s              -> TyUnboxedSum l <$> (mapExp f `mapM` s)
          TyList  l t                   -> TyList l <$> mapExp f t
          TyParArray  l t               -> TyParArray l <$> mapExp f t
          TyApp   l t1' t2              -> TyApp l <$> mapExp f t1' <*> mapExp f t2
          TyVar   l n                   -> return $ TyVar l n
          TyCon   l qn                  -> return $ TyCon l qn
          TyParen l t                   -> TyParen l <$> mapExp f t
          TyInfix l ta qn tb            -> TyInfix l <$> mapExp f ta <*> return qn <*> mapExp f tb
          TyKind  l t k                 -> TyKind l <$> mapExp f t <*> mapExp f k
          TyPromoted l   p              -> TyPromoted l <$> mapExp f p
          TyEquals l a b                -> TyEquals l <$> mapExp f a <*> mapExp f b
          TySplice l s                  -> TySplice l <$> mapExp f s
          TyBang l b u t                  -> TyBang l b u <$> mapExp f t
          TyWildCard l n                -> return $ TyWildCard l n
          TyQuasiQuote l n s            -> return $ TyQuasiQuote l n s
          TyComp l c t                  -> return $ TyComp l c t

instance ExpMap Promoted where
    mapExp _ (PromotedInteger l int raw) = return $ PromotedInteger l int raw
    mapExp _ (PromotedString l str raw) = return $ PromotedString l str raw
    mapExp _ (PromotedCon l b qn)   = return $ PromotedCon l b qn
    mapExp f (PromotedList l b ps)  = PromotedList  l b <$> (mapExp f `mapM` ps)
    mapExp f (PromotedTuple l ps) = PromotedTuple l <$> (mapExp f `mapM` ps)
    mapExp _ (PromotedUnit l)     = return $ PromotedUnit l

instance ExpMap TyVarBind where
    mapExp f (KindedVar   l n k) = KindedVar   l n <$> mapExp f k
    mapExp _ (UnkindedVar l n)   = return $ UnkindedVar l n

instance ExpMap Context where
    mapExp f (CxSingle l asst ) = CxSingle l <$> mapExp f asst
    mapExp f (CxTuple  l assts) = CxTuple  l <$> (mapExp f `mapM` assts)
    mapExp _ (CxEmpty l) = return $ CxEmpty l

instance ExpMap Asst where
    mapExp f asst = case asst of
        TypeA l t           -> TypeA l <$> mapExp f t
        IParam l ipn t      -> IParam l ipn <$> mapExp f t
        ParenA l a          -> ParenA l <$> mapExp f a
        CompCont l c        -> return $ CompCont l c

instance ExpMap Exp where
    mapExp f e1 = f =<< case e1 of
        Var l qn        -> return $ Var l qn
        OverloadedLabel l qn -> return $ OverloadedLabel l qn
        IPVar l ipn     -> return $ IPVar l ipn
        Con l qn        -> return $ Con l qn
        Lit l lit       -> return $ Lit l lit
        InfixApp l e1' qop e2    -> InfixApp l <$> mapExp f e1' <*> return qop <*> mapExp f e2
        App l e1' e2    -> App l <$> mapExp f e1' <*> mapExp f e2
        NegApp l e      -> NegApp l <$> mapExp f e
        Lambda l ps e   -> Lambda l <$> (mapExp f `mapM` ps) <*> mapExp f e
        Let l bs e      -> Let l <$> mapExp f bs <*> mapExp f e
        If l ec et ee   -> If l <$> mapExp f ec <*> mapExp f et <*> mapExp f ee
        MultiIf l alts -> MultiIf l <$> (mapExp f `mapM` alts)
        Case l e alts   -> Case l <$> mapExp f e <*> (mapExp f `mapM` alts)
        Do l ss         -> Do l <$> (mapExp f `mapM` ss)
        MDo l ss        -> MDo l <$> (mapExp f `mapM` ss)
        Tuple l bx es   -> Tuple l bx <$> (mapExp f `mapM` es)
        UnboxedSum l b a es -> UnboxedSum l b a <$> mapExp f es
        TupleSection l bx mes -> TupleSection l bx <$> ((mapExp f `mapM`) `mapM` mes)
        List l es       -> List l <$> (mapExp f `mapM` es)
        ParArray l es   -> ParArray l <$> (mapExp f `mapM` es)
        Paren l e       -> Paren l <$> mapExp f e
        LeftSection l e qop     -> LeftSection l <$> mapExp f e <*> return qop
        RightSection l qop e    -> RightSection l qop <$> mapExp f e
        RecConstr l qn fups     -> RecConstr l qn <$> (mapExp f `mapM` fups)
        RecUpdate l e  fups     -> RecUpdate l <$> mapExp f e <*> (mapExp f `mapM` fups)
        EnumFrom l e            -> EnumFrom l <$> mapExp f e
        EnumFromTo l ef et      -> EnumFromTo l <$> mapExp f ef <*> mapExp f et
        EnumFromThen l ef et    -> EnumFromThen l <$> mapExp f ef <*> mapExp f et
        EnumFromThenTo l ef eth eto -> EnumFromThenTo l <$> mapExp f ef <*> mapExp f eth <*> mapExp f eto
        ParArrayFromTo l ef et  -> ParArrayFromTo l <$> mapExp f ef <*> mapExp f et
        ParArrayFromThenTo l ef eth eto -> ParArrayFromThenTo l <$> mapExp f ef <*> mapExp f eth <*> mapExp f eto
        ListComp l e qss        -> ListComp l <$> mapExp f e <*> (mapExp f `mapM` qss)
        ParComp  l e qsss       -> ParComp  l <$> mapExp f e <*> ((mapExp f `mapM`) `mapM` qsss)
        ParArrayComp  l e qsss  -> ParArrayComp  l <$> mapExp f e <*> ((mapExp f `mapM`) `mapM` qsss)
        ExpTypeSig l e t        -> ExpTypeSig l <$> mapExp f e <*> mapExp f t
        VarQuote l qn           -> return $ VarQuote l qn
        TypQuote l qn           -> return $ TypQuote l qn
        BracketExp l br         -> BracketExp l <$> mapExp f br
        SpliceExp l sp          -> SpliceExp l <$> mapExp f sp
        QuasiQuote l sn se      -> return $ QuasiQuote l sn se
        TypeApp l t             -> TypeApp l <$> mapExp f t

        XTag  l xn xas me es     -> XTag  l xn <$> (mapExp f `mapM` xas) <*> (mapExp f `mapM` me) <*> (mapExp f `mapM` es)
        XETag l xn xas me        -> XETag l xn <$> (mapExp f `mapM` xas) <*> (mapExp f `mapM` me)
        XPcdata l s              -> return $ XPcdata l s
        XExpTag l e              -> XExpTag l <$> mapExp f e
        XChildTag l es           -> XChildTag l <$> (mapExp f `mapM` es)

        CorePragma l s e   -> CorePragma l s <$> mapExp f e
        SCCPragma  l s e   -> SCCPragma l s <$> mapExp f e
        GenPragma  l s n12 n34 e -> GenPragma l s n12 n34 <$> mapExp f e

        Proc            l p e  -> Proc l <$> mapExp f p <*> mapExp f e
        LeftArrApp      l e1' e2 -> LeftArrApp      l <$> mapExp f e1' <*> mapExp f e2
        RightArrApp     l e1' e2 -> RightArrApp     l <$> mapExp f e1' <*> mapExp f e2
        LeftArrHighApp  l e1' e2 -> LeftArrHighApp  l <$> mapExp f e1' <*> mapExp f e2
        RightArrHighApp l e1' e2 -> RightArrHighApp l <$> mapExp f e1' <*> mapExp f e2
        ArrOp           l e      -> ArrOp           l <$> mapExp f e

        LCase l alts -> LCase l <$> (mapExp f `mapM` alts)

instance ExpMap XAttr where
    mapExp f (XAttr l xn e) = XAttr l xn <$> mapExp f e

instance ExpMap ModulePragma where
    mapExp _ (LanguagePragma   l ns) = return $ LanguagePragma l ns
    mapExp _ (OptionsPragma    l mt s) = return $ OptionsPragma l mt s
    mapExp f (AnnModulePragma  l a) = AnnModulePragma l <$> mapExp f a

instance ExpMap Bracket where
    mapExp f (ExpBracket l e) = ExpBracket l <$> mapExp f e
    mapExp f (TExpBracket l e) = TExpBracket l <$> mapExp f e
    mapExp f (PatBracket l p) = PatBracket l <$> mapExp f p
    mapExp f (TypeBracket l t) = TypeBracket l <$> mapExp f t
    mapExp f (DeclBracket l ds) = DeclBracket l <$> (mapExp f `mapM` ds)

instance ExpMap Splice where
    mapExp _ (IdSplice l s) = return $ IdSplice l s
    mapExp _ (TIdSplice l s) = return $ TIdSplice l s
    mapExp f (ParenSplice l e) = ParenSplice l <$> mapExp f e
    mapExp f (TParenSplice l e) = TParenSplice l <$> mapExp f e

instance ExpMap Rule where
    mapExp f (Rule l s act mrvs e1 e2) = Rule l s act <$> ((mapExp f `mapM`) `mapM` mrvs) <*> mapExp f e1 <*> mapExp f e2

instance ExpMap RuleVar where
    mapExp _ (RuleVar l n) = return $ RuleVar l n
    mapExp f (TypedRuleVar l n t) = TypedRuleVar l n <$> mapExp f t

instance ExpMap Pat where
    mapExp f p1 = case p1 of
      PVar l n          -> return $ PVar l n
      PLit l sg lit     -> return $ PLit l sg lit
      PNPlusK l n k     -> return $ PNPlusK l n k
      PInfixApp l pa qn pb  -> PInfixApp l <$> mapExp f pa <*> return qn <*> mapExp f pb
      PApp l qn ps      -> PApp l qn <$> (mapExp f `mapM` ps)
      PTuple l bx ps    -> PTuple l bx <$> (mapExp f `mapM` ps)
      PUnboxedSum l b a ps -> PUnboxedSum l b a <$> mapExp f ps
      PList l ps        -> PList l <$> (mapExp f `mapM` ps)
      PParen l p        -> PParen l <$> mapExp f p
      PRec l qn pfs     -> PRec l qn <$> (mapExp f `mapM` pfs)
      PAsPat l n p      -> PAsPat l n <$> mapExp f p
      PWildCard l       -> return $ PWildCard l
      PIrrPat l p       -> PIrrPat l <$> mapExp f p
      PatTypeSig l p t  -> PatTypeSig l <$> mapExp f p <*> mapExp f t
      PViewPat l e p    -> PViewPat l <$> mapExp f e <*> mapExp f p
      PRPat l rps       -> PRPat l <$> (mapExp f `mapM` rps)
      PXTag l xn pxas mp ps -> PXTag  l xn <$> (mapExp f `mapM` pxas) <*> (mapExp f `mapM` mp) <*> (mapExp f `mapM` ps)
      PXETag l xn pxas mp   -> PXETag l xn <$> (mapExp f `mapM` pxas) <*> (mapExp f `mapM` mp)
      PXPcdata l s      -> return $ PXPcdata l s
      PXPatTag l p      -> PXPatTag l <$> mapExp f p
      PXRPats  l rps    -> PXRPats  l <$> (mapExp f `mapM` rps)
      PSplice l sp      -> PSplice l <$> mapExp f sp
      PQuasiQuote l sn st   -> return $ PQuasiQuote l sn st
      PBangPat l p          -> PBangPat l <$> mapExp f p

instance ExpMap PXAttr where
    mapExp f (PXAttr l xn p) = PXAttr l xn <$> mapExp f p

instance ExpMap RPat where
    mapExp f rp1 = case rp1 of
      RPOp l rp rop         -> RPOp l <$> mapExp f rp <*> return rop
      RPEither l rp1' rp2   -> RPEither l <$> mapExp f rp1' <*> mapExp f rp2
      RPSeq l rps           -> RPSeq l <$> (mapExp f `mapM` rps)
      RPGuard l p ss        -> RPGuard l <$> mapExp f p <*> (mapExp f `mapM` ss)
      RPCAs l n rp          -> RPCAs l n <$> mapExp f rp
      RPAs l n rp           -> RPAs l n <$> mapExp f rp
      RPParen l rp          -> RPParen l <$> mapExp f rp
      RPPat l p             -> RPPat l <$> mapExp f p

instance ExpMap PatField where
    mapExp f (PFieldPat l qn p) = PFieldPat l qn <$> mapExp f p
    mapExp _ (PFieldPun l n) = return $ PFieldPun l n
    mapExp _ (PFieldWildcard l) = return $ PFieldWildcard l

instance ExpMap Stmt where
    mapExp f (Generator l p e) = Generator l <$> mapExp f p <*> mapExp f e
    mapExp f (Qualifier l e)   = Qualifier l <$> mapExp f e
    mapExp f (LetStmt l bs)    = LetStmt l <$> mapExp f bs
    mapExp f (RecStmt l ss)    = RecStmt l <$> (mapExp f `mapM` ss)

instance ExpMap QualStmt where
    mapExp f (QualStmt     l s) = QualStmt l <$> mapExp f s
    mapExp f (ThenTrans    l e) = ThenTrans l <$> mapExp f e
    mapExp f (ThenBy       l e1 e2) = ThenBy l <$> mapExp f e1 <*> mapExp f e2
    mapExp f (GroupBy      l e) = GroupBy l <$> mapExp f e
    mapExp f (GroupUsing   l e) = GroupUsing l <$> mapExp f e
    mapExp f (GroupByUsing l e1 e2) = GroupByUsing l <$> mapExp f e1 <*> mapExp f e2

instance ExpMap FieldUpdate where
    mapExp f (FieldUpdate l qn e) = FieldUpdate l qn <$> mapExp f e
    mapExp _ (FieldPun l n)       = return $ FieldPun l n
    mapExp _ (FieldWildcard l)    = return $ FieldWildcard l

instance ExpMap Alt where
    mapExp f (Alt l p gs bs) = Alt l <$> mapExp f p <*> mapExp f gs <*> (mapExp f `mapM` bs)
