module Utils.Types(mapType, TypeTransform) where

import Language.Haskell.Exts.Syntax

type TypeTransform l m = Type l -> m (Type l)

class TypeMap a where
    mapType :: (Monad m) => TypeTransform l m -> a l -> m (a l)

instance TypeMap Module where
    mapType f (Module l mmh ops iss dcls) =
          Module l mmh <$> (mapType f `mapM` ops) <*> return iss <*> (mapType f `mapM` dcls)
    mapType f (XmlPage l mn os xn xas me es) =
          XmlPage l mn <$> (mapType f `mapM` os) <*> return xn <*> (mapType f `mapM` xas) <*> (mapType f `mapM` me) <*> (mapType f `mapM` es)
    mapType f (XmlHybrid l mmh ops iss dcls xn xas me es) =
          XmlHybrid l mmh <$> (mapType f `mapM` ops) <*> return iss <*> (mapType f `mapM` dcls) <*> return xn <*> (mapType f `mapM` xas) <*> (mapType f `mapM` me) <*> (mapType f `mapM` es)

instance TypeMap Decl where
    mapType f decl = case decl of
            TypeDecl     l dh t      -> TypeDecl    l <$> mapType f dh <*> mapType f t
            TypeFamDecl  l dh mk mi  -> TypeFamDecl l <$> mapType f dh <*> (mapType f `mapM` mk) <*> return mi
            ClosedTypeFamDecl  l dh mk mi eqns  -> ClosedTypeFamDecl l <$> mapType f dh <*> (mapType f `mapM` mk) <*> return mi <*> (mapType f `mapM` eqns)
            DataDecl     l dn mcx dh cds ders ->
                DataDecl l dn <$> (mapType f `mapM` mcx) <*> mapType f dh <*> (mapType f `mapM` cds) <*> (mapType f `mapM` ders)
            GDataDecl    l dn mcx dh mk gds ders ->
                GDataDecl l dn <$> (mapType f `mapM` mcx) <*> mapType f dh <*> (mapType f `mapM` mk) <*> (mapType f `mapM` gds) <*> (mapType f `mapM` ders)
            DataFamDecl  l mcx dh mk         -> DataFamDecl l <$> (mapType f `mapM` mcx) <*> mapType f dh <*> (mapType f `mapM` mk)
            TypeInsDecl  l t1 t2             -> TypeInsDecl l <$> mapType f t1 <*> mapType f t2
            DataInsDecl  l dn t cds ders     -> DataInsDecl l dn <$> mapType f t <*> (mapType f `mapM` cds) <*> (mapType f `mapM` ders)
            GDataInsDecl l dn t mk gds ders  -> GDataInsDecl l dn <$> mapType f t <*> (mapType f `mapM` mk) <*> (mapType f `mapM` gds) <*> (mapType f `mapM` ders)
            ClassDecl    l mcx dh fds cds    -> ClassDecl l <$> (mapType f `mapM` mcx) <*> mapType f dh <*> return fds <*> ((mapType f `mapM`) `mapM` cds)
            InstDecl     l mo ih ids         -> InstDecl l mo <$> mapType f ih <*> ((mapType f `mapM`) `mapM` ids)
            DerivDecl    l mds mo ih         -> DerivDecl l <$> (mapType f `mapM` mds) <*> return mo <*> mapType f ih
            InfixDecl    l a k ops           -> return $ InfixDecl l a k ops
            DefaultDecl  l ts                -> DefaultDecl l <$> (mapType f `mapM` ts)
            SpliceDecl   l sp                -> SpliceDecl l <$> mapType f sp
            TSpliceDecl  l sp                -> TSpliceDecl l <$> mapType f sp
            TypeSig      l ns t              -> TypeSig l ns <$> mapType f t
            PatSynSig    l n dh c1 dh2 c2 t  -> PatSynSig l n <$> ((mapType f `mapM`) `mapM` dh) <*> (mapType f `mapM` c1) <*> ((mapType f `mapM`) `mapM` dh2) <*> (mapType f `mapM` c2) <*> mapType f t
            FunBind      l ms                -> FunBind l <$> (mapType f `mapM` ms)
            PatBind      l p rhs bs          -> PatBind l <$> mapType f p <*> mapType f rhs <*> (mapType f `mapM` bs)
            PatSyn           l p r d         -> PatSyn l <$> mapType f p <*> mapType f r <*> mapType f d
            ForImp       l cc msf s n t      -> ForImp l cc msf s n <$> mapType f t
            ForExp       l cc     s n t      -> ForExp l cc     s n <$> mapType f t
            RulePragmaDecl   l rs            -> RulePragmaDecl l <$> (mapType f `mapM` rs)
            DeprPragmaDecl   l nss           -> return $ DeprPragmaDecl l nss
            WarnPragmaDecl   l nss           -> return $ WarnPragmaDecl l nss
            InlineSig        l b act qn      -> return $ InlineSig l b act qn
            InlineConlikeSig l   act qn      -> return $ InlineConlikeSig l act qn
            SpecSig          l   act qn ts   -> SpecSig       l   act qn <$> (mapType f `mapM` ts)
            SpecInlineSig    l b act qn ts   -> SpecInlineSig l b act qn <$> (mapType f `mapM` ts)
            InstSig          l ih            -> InstSig l <$> mapType f ih
            AnnPragma        l ann'          -> AnnPragma l <$> mapType f ann'
            MinimalPragma    l b             -> return $ MinimalPragma l b
            RoleAnnotDecl    l t rs          -> return $ RoleAnnotDecl l t rs
            CompletePragma   l cs ty         -> return $ CompletePragma l cs ty
            PieceDecl   l ca mcx dh cds      -> PieceDecl l ca <$> (mapType f `mapM` mcx) <*> mapType f dh <*> (mapType f `mapM` cds)
            PieceCatDecl l ca                -> return $ PieceCatDecl l ca
            CompFunDecl  l ns mcx ca t       -> CompFunDecl l ns <$> (mapType f `mapM` mcx) <*> return ca <*> mapType f t
            CompFunExt   l mcx fn ts pn ids  -> CompFunExt l <$> (mapType f `mapM` mcx) <*> return fn <*> mapType f `mapM` ts <*> mapType f pn <*> ((mapType f `mapM`) `mapM` ids)
            

instance TypeMap PatternSynDirection where
    mapType _ Unidirectional                 = return Unidirectional
    mapType _ ImplicitBidirectional          = return ImplicitBidirectional
    mapType f (ExplicitBidirectional l dcls) = ExplicitBidirectional l <$> (mapType f `mapM` dcls)

instance TypeMap TypeEqn where
    mapType f (TypeEqn l a b) = TypeEqn l <$> mapType f a <*> mapType f b

instance TypeMap Annotation where
    mapType f (Ann     l n e) = Ann     l n <$> mapType f e
    mapType f (TypeAnn l n e) = TypeAnn l n <$> mapType f e
    mapType f (ModuleAnn l e) = ModuleAnn l <$> mapType f e

instance TypeMap ResultSig where
    mapType f (KindSig l k) = KindSig l <$> mapType f k
    mapType f (TyVarSig l tv) = TyVarSig l <$> mapType f tv

instance TypeMap DeclHead where
    mapType _ (DHead l n)           = return $ DHead l n
    mapType f (DHInfix l tva n)     = DHInfix l <$> mapType f tva <*> return n
    mapType f (DHParen l dh)        = DHParen l <$> mapType f dh
    mapType f (DHApp l dh t)        = DHApp l <$> mapType f dh <*> mapType f t

instance TypeMap InstRule where
    mapType f (IRule l mtv cxt qn)  = IRule l <$> ((mapType f `mapM`) `mapM` mtv) <*> (mapType f `mapM` cxt) <*> mapType f qn
    mapType f (IParen l ih)         = IParen l <$> mapType f ih

instance TypeMap InstHead where
    mapType _ (IHCon l n)           = return $ IHCon l n
    mapType f (IHInfix l tva n)     = IHInfix l <$> mapType f tva <*> return n
    mapType f (IHParen l dh)        = IHParen l <$> mapType f dh
    mapType f (IHApp l dh t)        = IHApp l <$> mapType f dh <*> mapType f t

instance TypeMap Deriving where
    mapType f (Deriving l mds ihs) = Deriving l <$> (mapType f `mapM` mds) <*> (mapType f `mapM` ihs)

instance TypeMap DerivStrategy where
    mapType _ (DerivStock l)    = return $ DerivStock l
    mapType _ (DerivAnyclass l) = return $ DerivAnyclass l
    mapType _ (DerivNewtype l)  = return $ DerivNewtype l
    mapType f (DerivVia l t)    = DerivVia l <$> mapType f t

instance TypeMap Binds where
    mapType f (BDecls  l decls) = BDecls l <$> (mapType f `mapM` decls)
    mapType f (IPBinds l ibs)   = IPBinds l <$> (mapType f `mapM` ibs)

instance TypeMap IPBind where
    mapType f (IPBind l ipn e) = IPBind l ipn <$> mapType f e

instance TypeMap Match where
    mapType f (Match l n ps rhs bs) = Match l n <$> (mapType f `mapM` ps) <*> mapType f rhs <*> (mapType f `mapM` bs)
    mapType f (InfixMatch l a n b rhs bs) = InfixMatch l <$> mapType f a <*> return n <*> (mapType f `mapM` b) <*> mapType f rhs <*> (mapType f `mapM` bs)

instance TypeMap QualConDecl where
    mapType f (QualConDecl l tvs cx cd) = QualConDecl l <$> ((mapType f `mapM`) `mapM` tvs) <*> (mapType f `mapM` cx) <*> mapType f cd

instance TypeMap ConDecl where
    mapType f (ConDecl l n bts) = ConDecl l n <$> (mapType f `mapM` bts)
    mapType f (InfixConDecl l ta n tb) = InfixConDecl l <$> mapType f ta <*> return n <*> mapType f tb
    mapType f (RecDecl l n fds) = RecDecl l n <$> (mapType f `mapM` fds)

instance TypeMap FieldDecl where
    mapType f (FieldDecl l ns t) = FieldDecl l ns <$> mapType f t

instance TypeMap GadtDecl where
    mapType f (GadtDecl l n t1 t2 t3 t4) = GadtDecl l n <$> ((mapType f `mapM`) `mapM` t1) <*> (mapType f `mapM` t2) <*> ((mapType f `mapM`) `mapM` t3) <*> mapType f t4

instance TypeMap ClassDecl where
    mapType f (ClsDecl    l d) = ClsDecl l <$> mapType f d
    mapType f (ClsDataFam l mcx dh mk) = ClsDataFam l <$> (mapType f `mapM` mcx) <*> mapType f dh <*> (mapType f `mapM` mk)
    mapType f (ClsTyFam   l dh mk mi) = ClsTyFam l <$> mapType f dh <*> (mapType f `mapM` mk) <*> return mi
    mapType f (ClsTyDef   l t ) = ClsTyDef l <$> mapType f t
    mapType f (ClsDefSig  l n t) = ClsDefSig l n <$> mapType f t

instance TypeMap InstDecl where
    mapType f idecl = case idecl of
        InsDecl   l d           -> InsDecl l <$> mapType f d
        InsType   l t1 t2       -> InsType l <$> mapType f t1 <*> mapType f t2
        InsData   l dn t    cds ders -> InsData  l dn <$> mapType f t <*> (mapType f `mapM` cds) <*> (mapType f `mapM` ders)
        InsGData  l dn t mk gds ders -> InsGData l dn <$> mapType f t <*> (mapType f `mapM` mk) <*> (mapType f `mapM` gds) <*> (mapType f `mapM` ders)

instance TypeMap Rhs where
     mapType f (UnGuardedRhs l e)     = UnGuardedRhs l <$> mapType f e
     mapType f (GuardedRhss  l grhss) = GuardedRhss  l <$> (mapType f `mapM` grhss)

instance TypeMap GuardedRhs where
     mapType f (GuardedRhs l ss e) = GuardedRhs l <$> (mapType f `mapM` ss) <*> mapType f e

instance TypeMap Type where
    mapType f t1 = f =<< case t1 of
          TyForall l mtvs mcx t         -> TyForall l <$> ((mapType f `mapM`) `mapM` mtvs) <*> (mapType f `mapM` mcx) <*> mapType f t
          TyStar  l                     -> return $ TyStar l
          TyFun   l t1' t2              -> TyFun l <$> mapType f t1' <*> mapType f t2
          TyTuple l b ts                -> TyTuple l b <$> (mapType f `mapM` ts)
          TyUnboxedSum l s              -> TyUnboxedSum l <$> (mapType f `mapM` s)
          TyList  l t                   -> TyList l <$> mapType f t
          TyParArray  l t               -> TyParArray l <$> mapType f t
          TyApp   l t1' t2              -> TyApp l <$> mapType f t1' <*> mapType f t2
          TyVar   l n                   -> return $ TyVar l n
          TyCon   l qn                  -> return $ TyCon l qn
          TyParen l t                   -> TyParen l <$> mapType f t
          TyInfix l ta qn tb            -> TyInfix l <$> mapType f ta <*> return qn <*> mapType f tb
          TyKind  l t k                 -> TyKind l <$> mapType f t <*> mapType f k
          TyPromoted l   p              -> TyPromoted l <$> mapType f p
          TyEquals l a b                -> TyEquals l <$> mapType f a <*> mapType f b
          TySplice l s                  -> TySplice l <$> mapType f s
          TyBang l b u t                  -> TyBang l b u <$> mapType f t
          TyWildCard l n                -> return $ TyWildCard l n
          TyQuasiQuote l n s            -> return $ TyQuasiQuote l n s
          TyComp l c t                  -> TyComp l c <$> mapType f `mapM` t

instance TypeMap Constraint where
    mapType f c = case c of
        FunConstraint l qn ts t       -> FunConstraint l qn <$> mapType f `mapM` ts <*> return t
        PieceConstraint l qn t        -> PieceConstraint l <$> mapType f qn <*> return t
        CategoryConstraint l qn t     -> return $ CategoryConstraint l qn t


instance TypeMap Promoted where
    mapType _ (PromotedInteger l int raw) = return $ PromotedInteger l int raw
    mapType _ (PromotedString l str raw) = return $ PromotedString l str raw
    mapType _ (PromotedCon l b qn)   = return $ PromotedCon l b qn
    mapType f (PromotedList l b ps)  = PromotedList  l b <$> (mapType f `mapM` ps)
    mapType f (PromotedTuple l ps) = PromotedTuple l <$> (mapType f `mapM` ps)
    mapType _ (PromotedUnit l)     = return $ PromotedUnit l

instance TypeMap TyVarBind where
    mapType f (KindedVar   l n k) = KindedVar   l n <$> mapType f k
    mapType _ (UnkindedVar l n)   = return $ UnkindedVar l n

instance TypeMap Context where
    mapType f (CxSingle l asst ) = CxSingle l <$> mapType f asst
    mapType f (CxTuple  l assts) = CxTuple  l <$> (mapType f `mapM` assts)
    mapType _ (CxEmpty l) = return $ CxEmpty l

instance TypeMap Asst where
    mapType f asst = case asst of
        TypeA l t           -> TypeA l <$> mapType f t
        IParam l ipn t      -> IParam l ipn <$> mapType f t
        ParenA l a          -> ParenA l <$> mapType f a
        CompCont l c        -> return $ CompCont l c

instance TypeMap Exp where
    mapType f e1 = case e1 of
        Var l qn        -> return $ Var l qn
        OverloadedLabel l qn -> return $ OverloadedLabel l qn
        IPVar l ipn     -> return $ IPVar l ipn
        Con l qn        -> return $ Con l qn
        Lit l lit       -> return $ Lit l lit
        InfixApp l e1' qop e2    -> InfixApp l <$> mapType f e1' <*> return qop <*> mapType f e2
        App l e1' e2    -> App l <$> mapType f e1' <*> mapType f e2
        NegApp l e      -> NegApp l <$> mapType f e
        Lambda l ps e   -> Lambda l <$> (mapType f `mapM` ps) <*> mapType f e
        Let l bs e      -> Let l <$> mapType f bs <*> mapType f e
        If l ec et ee   -> If l <$> mapType f ec <*> mapType f et <*> mapType f ee
        MultiIf l alts -> MultiIf l <$> (mapType f `mapM` alts)
        Case l e alts   -> Case l <$> mapType f e <*> (mapType f `mapM` alts)
        Do l ss         -> Do l <$> (mapType f `mapM` ss)
        MDo l ss        -> MDo l <$> (mapType f `mapM` ss)
        Tuple l bx es   -> Tuple l bx <$> (mapType f `mapM` es)
        UnboxedSum l b a es -> UnboxedSum l b a <$> mapType f es
        TupleSection l bx mes -> TupleSection l bx <$> ((mapType f `mapM`) `mapM` mes)
        List l es       -> List l <$> (mapType f `mapM` es)
        ParArray l es   -> ParArray l <$> (mapType f `mapM` es)
        Paren l e       -> Paren l <$> mapType f e
        LeftSection l e qop     -> LeftSection l <$> mapType f e <*> return qop
        RightSection l qop e    -> RightSection l qop <$> mapType f e
        RecConstr l qn fups     -> RecConstr l qn <$> (mapType f `mapM` fups)
        RecUpdate l e  fups     -> RecUpdate l <$> mapType f e <*> (mapType f `mapM` fups)
        EnumFrom l e            -> EnumFrom l <$> mapType f e
        EnumFromTo l ef et      -> EnumFromTo l <$> mapType f ef <*> mapType f et
        EnumFromThen l ef et    -> EnumFromThen l <$> mapType f ef <*> mapType f et
        EnumFromThenTo l ef eth eto -> EnumFromThenTo l <$> mapType f ef <*> mapType f eth <*> mapType f eto
        ParArrayFromTo l ef et  -> ParArrayFromTo l <$> mapType f ef <*> mapType f et
        ParArrayFromThenTo l ef eth eto -> ParArrayFromThenTo l <$> mapType f ef <*> mapType f eth <*> mapType f eto
        ListComp l e qss        -> ListComp l <$> mapType f e <*> (mapType f `mapM` qss)
        ParComp  l e qsss       -> ParComp  l <$> mapType f e <*> ((mapType f `mapM`) `mapM` qsss)
        ParArrayComp  l e qsss  -> ParArrayComp  l <$> mapType f e <*> ((mapType f `mapM`) `mapM` qsss)
        ExpTypeSig l e t        -> ExpTypeSig l <$> mapType f e <*> mapType f t
        VarQuote l qn           -> return $ VarQuote l qn
        TypQuote l qn           -> return $ TypQuote l qn
        BracketExp l br         -> BracketExp l <$> mapType f br
        SpliceExp l sp          -> SpliceExp l <$> mapType f sp
        QuasiQuote l sn se      -> return $ QuasiQuote l sn se
        TypeApp l t             -> TypeApp l <$> mapType f t

        XTag  l xn xas me es     -> XTag  l xn <$> (mapType f `mapM` xas) <*> (mapType f `mapM` me) <*> (mapType f `mapM` es)
        XETag l xn xas me        -> XETag l xn <$> (mapType f `mapM` xas) <*> (mapType f `mapM` me)
        XPcdata l s              -> return $ XPcdata l s
        XExpTag l e              -> XExpTag l <$> mapType f e
        XChildTag l es           -> XChildTag l <$> (mapType f `mapM` es)

        CorePragma l s e   -> CorePragma l s <$> mapType f e
        SCCPragma  l s e   -> SCCPragma l s <$> mapType f e
        GenPragma  l s n12 n34 e -> GenPragma l s n12 n34 <$> mapType f e

        Proc            l p e  -> Proc l <$> mapType f p <*> mapType f e
        LeftArrApp      l e1' e2 -> LeftArrApp      l <$> mapType f e1' <*> mapType f e2
        RightArrApp     l e1' e2 -> RightArrApp     l <$> mapType f e1' <*> mapType f e2
        LeftArrHighApp  l e1' e2 -> LeftArrHighApp  l <$> mapType f e1' <*> mapType f e2
        RightArrHighApp l e1' e2 -> RightArrHighApp l <$> mapType f e1' <*> mapType f e2
        ArrOp           l e      -> ArrOp           l <$> mapType f e

        LCase l alts -> LCase l <$> (mapType f `mapM` alts)

instance TypeMap XAttr where
    mapType f (XAttr l xn e) = XAttr l xn <$> mapType f e

instance TypeMap ModulePragma where
    mapType _ (LanguagePragma   l ns) = return $ LanguagePragma l ns
    mapType _ (OptionsPragma    l mt s) = return $ OptionsPragma l mt s
    mapType f (AnnModulePragma  l a) = AnnModulePragma l <$> mapType f a

instance TypeMap Bracket where
    mapType f (ExpBracket l e) = ExpBracket l <$> mapType f e
    mapType f (TExpBracket l e) = TExpBracket l <$> mapType f e
    mapType f (PatBracket l p) = PatBracket l <$> mapType f p
    mapType f (TypeBracket l t) = TypeBracket l <$> mapType f t
    mapType f (DeclBracket l ds) = DeclBracket l <$> (mapType f `mapM` ds)

instance TypeMap Splice where
    mapType _ (IdSplice l s) = return $ IdSplice l s
    mapType _ (TIdSplice l s) = return $ TIdSplice l s
    mapType f (ParenSplice l e) = ParenSplice l <$> mapType f e
    mapType f (TParenSplice l e) = TParenSplice l <$> mapType f e

instance TypeMap Rule where
    mapType f (Rule l s act mrvs e1 e2) = Rule l s act <$> ((mapType f `mapM`) `mapM` mrvs) <*> mapType f e1 <*> mapType f e2

instance TypeMap RuleVar where
    mapType _ (RuleVar l n) = return $ RuleVar l n
    mapType f (TypedRuleVar l n t) = TypedRuleVar l n <$> mapType f t

instance TypeMap Pat where
    mapType f p1 = case p1 of
      PVar l n          -> return $ PVar l n
      PLit l sg lit     -> return $ PLit l sg lit
      PNPlusK l n k     -> return $ PNPlusK l n k
      PInfixApp l pa qn pb  -> PInfixApp l <$> mapType f pa <*> return qn <*> mapType f pb
      PApp l qn ps      -> PApp l qn <$> (mapType f `mapM` ps)
      PTuple l bx ps    -> PTuple l bx <$> (mapType f `mapM` ps)
      PUnboxedSum l b a ps -> PUnboxedSum l b a <$> mapType f ps
      PList l ps        -> PList l <$> (mapType f `mapM` ps)
      PParen l p        -> PParen l <$> mapType f p
      PRec l qn pfs     -> PRec l qn <$> (mapType f `mapM` pfs)
      PAsPat l n p      -> PAsPat l n <$> mapType f p
      PWildCard l       -> return $ PWildCard l
      PIrrPat l p       -> PIrrPat l <$> mapType f p
      PatTypeSig l p t  -> PatTypeSig l <$> mapType f p <*> mapType f t
      PViewPat l e p    -> PViewPat l <$> mapType f e <*> mapType f p
      PRPat l rps       -> PRPat l <$> (mapType f `mapM` rps)
      PXTag l xn pxas mp ps -> PXTag  l xn <$> (mapType f `mapM` pxas) <*> (mapType f `mapM` mp) <*> (mapType f `mapM` ps)
      PXETag l xn pxas mp   -> PXETag l xn <$> (mapType f `mapM` pxas) <*> (mapType f `mapM` mp)
      PXPcdata l s      -> return $ PXPcdata l s
      PXPatTag l p      -> PXPatTag l <$> mapType f p
      PXRPats  l rps    -> PXRPats  l <$> (mapType f `mapM` rps)
      PSplice l sp      -> PSplice l <$> mapType f sp
      PQuasiQuote l sn st   -> return $ PQuasiQuote l sn st
      PBangPat l p          -> PBangPat l <$> mapType f p

instance TypeMap PXAttr where
    mapType f (PXAttr l xn p) = PXAttr l xn <$> mapType f p

instance TypeMap RPat where
    mapType f rp1 = case rp1 of
      RPOp l rp rop         -> RPOp l <$> mapType f rp <*> return rop
      RPEither l rp1' rp2   -> RPEither l <$> mapType f rp1' <*> mapType f rp2
      RPSeq l rps           -> RPSeq l <$> (mapType f `mapM` rps)
      RPGuard l p ss        -> RPGuard l <$> mapType f p <*> (mapType f `mapM` ss)
      RPCAs l n rp          -> RPCAs l n <$> mapType f rp
      RPAs l n rp           -> RPAs l n <$> mapType f rp
      RPParen l rp          -> RPParen l <$> mapType f rp
      RPPat l p             -> RPPat l <$> mapType f p

instance TypeMap PatField where
    mapType f (PFieldPat l qn p) = PFieldPat l qn <$> mapType f p
    mapType _ (PFieldPun l n) = return $ PFieldPun l n
    mapType _ (PFieldWildcard l) = return $ PFieldWildcard l

instance TypeMap Stmt where
    mapType f (Generator l p e) = Generator l <$> mapType f p <*> mapType f e
    mapType f (Qualifier l e)   = Qualifier l <$> mapType f e
    mapType f (LetStmt l bs)    = LetStmt l <$> mapType f bs
    mapType f (RecStmt l ss)    = RecStmt l <$> (mapType f `mapM` ss)

instance TypeMap QualStmt where
    mapType f (QualStmt     l s) = QualStmt l <$> mapType f s
    mapType f (ThenTrans    l e) = ThenTrans l <$> mapType f e
    mapType f (ThenBy       l e1 e2) = ThenBy l <$> mapType f e1 <*> mapType f e2
    mapType f (GroupBy      l e) = GroupBy l <$> mapType f e
    mapType f (GroupUsing   l e) = GroupUsing l <$> mapType f e
    mapType f (GroupByUsing l e1 e2) = GroupByUsing l <$> mapType f e1 <*> mapType f e2

instance TypeMap FieldUpdate where
    mapType f (FieldUpdate l qn e) = FieldUpdate l qn <$> mapType f e
    mapType _ (FieldPun l n)       = return $ FieldPun l n
    mapType _ (FieldWildcard l)    = return $ FieldWildcard l

instance TypeMap Alt where
    mapType f (Alt l p gs bs) = Alt l <$> mapType f p <*> mapType f gs <*> (mapType f `mapM` bs)
