module Desugar
    ( Info
    , Desugar
    , desugar
    ) where

import Control.Applicative
import Data.Traversable
import Language.Haskell.Exts.Annotated.Syntax



type Info = Int

-- data Desugar :: * -> * where
--   D :: (Info, ast) -> Desugar Info ast
data Desugar ast = D (Info, ast)
  deriving (Show)

instance Functor Desugar where
  fmap f (D (info, ast)) = D (info, f ast)

instance Applicative Desugar where
  pure ast = D (0, ast)
  D (info, f) <*> D (info', ast) = D (info + info', f ast)

-- List and Maybe uses do notations in it's mapM implementation
instance Monad Desugar where
  return = pure
  D (info, ast) >>= f =
    let
      D (info', ast') = f ast
    in
      D (info + info', ast')



-- copycat QQ
-- why Monad? why not Applicative Functor?
class (Applicative m) => Desugarable m a where
  desugar :: a -> m a

-- godfat 提示說：
--
-- > 你的 increase 是 Desugar a -> Desugar a
-- > 但是如果用 DesugarState -> (DesugarState, a) 的話，
-- > 那個 increase 就可以是 a -> Desugar a 而不是 Desugar a -> Desugar a,
-- > 像是這樣：
-- > increase a = Desugar $ \info -> (info + 1, a)
-- > 而如果 type 是 a -> Desugar a 的話，就可以串到 monad 上了：
-- > term >>= increase
-- > 我覺得主要是為了後面串其他東西的方便，
-- > 實際上兩者能表達的應該是等價的
--
increase :: Desugar a -> Desugar a
increase (D (info, ast)) = D (info + 1, ast)

-- can I just?
--
-- instance Annotated ast => Desugarable (Desugar Info) ast where
-- ..
--
-- > 如果全部寫在一起，會沒辦法去 match 各個 data constructors
-- > 如果是個範圍很大的 instance ，雖然用起來很方便，但是會製造隱性的成本（？）
-- >
-- > 舉的例子是，數字自己的 monoid ，可以分加法和乘法，正解是靠 newtype ，除非只
-- > 有一種 monoid ，沒有那樣講就是錯的（？）
--
-- 另外我還得搞清楚 sequence 是否能幫我加起 List 中的 ast 們的 info ，如果不行，
-- 是表示我該實作 Monoid 嗎？或是說 Int 是 Monoid ，所以不用煩惱這些？
--
-- 2016.07.07
-- fmap :: (a -> b) -> f a -> f b
-- fmap desugar :: f a -> f (m a)
-- sequence . fmap desugar :: t a -> m (t a)
-- mapM desugar :: t a -> m (t a)
-- mapM :: (a -> m b) -> t a -> m (t b)
--
instance Desugarable Desugar (Alt l) where
  desugar (Alt l pat rhs mBinds) = Alt l <$> desugar pat <*> desugar rhs <*> mapM desugar mBinds

instance Desugarable Desugar (FieldUpdate l) where
  desugar (FieldUpdate l qName exp) = FieldUpdate l <$> desugar qName <*> desugar exp
  desugar (FieldPun l qName) = FieldPun l <$> desugar qName
  desugar (FieldWildcard l) = pure $ FieldWildcard l

instance Desugarable Desugar (QualStmt l) where
  desugar (QualStmt l stmt) = QualStmt l <$> desugar stmt
  desugar (ThenTrans l exp) = ThenTrans l <$> desugar exp
  desugar (ThenBy l exp0 exp1) = ThenBy l <$> desugar exp0 <*> desugar exp1
  desugar (GroupBy l exp) = GroupBy l <$> desugar exp
  desugar (GroupUsing l exp) = GroupUsing l <$> desugar exp
  desugar (GroupByUsing l exp0 exp1) = GroupByUsing l <$> desugar exp0 <*> desugar exp1

instance Desugarable Desugar (Stmt l) where
  desugar (Generator l pat exp) = Generator l <$> desugar pat <*> desugar exp
  desugar (Qualifier l exp) = Qualifier l <$> desugar exp
  desugar (LetStmt l binds) = LetStmt l <$> desugar binds
  desugar (RecStmt l stmtList) = RecStmt l <$> mapM desugar stmtList

instance Desugarable Desugar (PatField l) where
  desugar (PFieldPat l qName pat) = PFieldPat l <$> desugar qName <*> desugar pat
  desugar (PFieldPun l qName) = PFieldPun l <$> desugar qName
  desugar (PFieldWildcard l) = pure $ PFieldWildcard l

instance Desugarable Desugar (RPat l) where
  desugar (RPOp l rPat rPatOp) = RPOp l <$> desugar rPat <*> desugar rPatOp
  desugar (RPEither l rPat0 rPat1) = RPEither l <$> desugar rPat0 <*> desugar rPat1
  desugar (RPSeq l rPatList) = RPSeq l <$> mapM desugar rPatList
  desugar (RPGuard l pat stmtList) = RPGuard l <$> desugar pat <*> mapM desugar stmtList
  desugar (RPCAs l name rPat) = RPCAs l <$> desugar name <*> desugar rPat
  desugar (RPAs l name rPat) = RPAs l <$> desugar name <*> desugar rPat
  desugar (RPParen l rPat) = RPParen l <$> desugar rPat
  desugar (RPPat l pat) = RPPat l <$> desugar pat

instance Desugarable Desugar (RPatOp l) where
  desugar (RPStar l) = pure $ RPStar l
  desugar (RPStarG l) = pure $ RPStarG l
  desugar (RPPlus l) = pure $ RPPlus l
  desugar (RPPlusG l) = pure $ RPPlusG l
  desugar (RPOpt l) = pure $ RPOpt l
  desugar (RPOptG l) = pure $ RPOpt l

instance Desugarable Desugar (PXAttr l) where
  desugar (PXAttr l xName pat) = PXAttr l <$> desugar xName <*> desugar pat

instance Desugarable Desugar (Pat l) where
  desugar (PVar l name) = PVar l <$> desugar name
  desugar (PLit l sign literal) = PLit l <$> desugar sign <*> desugar literal
  desugar (PNPlusK l name integer) = PNPlusK l <$> desugar name <*> pure integer
  desugar (PInfixApp l pat0 qName pat1) = PInfixApp l <$> desugar pat0 <*> desugar qName <*> desugar pat1
  desugar (PApp l qName patList) = PApp l <$> desugar qName <*> mapM desugar patList
  desugar (PTuple l boxed patList) = PTuple l <$> pure boxed <*> mapM desugar patList
  desugar (PList l patList) = PList l <$> mapM desugar patList
  desugar (PParen l pat) = PParen l <$> desugar pat
  desugar (PRec l qName patFieldList) = PRec l <$> desugar qName <*> mapM desugar patFieldList
  desugar (PAsPat l name pat) = PAsPat l <$> desugar name <*> desugar pat
  desugar (PWildCard l) = pure $ PWildCard l
  desugar (PIrrPat l pat) = PIrrPat l <$> desugar pat
  desugar (PatTypeSig l pat ty) = PatTypeSig l <$> desugar pat <*> desugar ty
  desugar (PViewPat l exp pat) = PViewPat l <$> desugar exp <*> desugar pat
  desugar (PRPat l rPatList) = PRPat l <$> mapM desugar rPatList
  desugar (PXTag l xName pxAttrList mPat patList) = PXTag l <$> desugar xName <*> mapM desugar pxAttrList <*> mapM desugar mPat <*> mapM desugar patList
  desugar (PXETag l xName pxAttrList mPat) = PXETag l <$> desugar xName <*> mapM desugar pxAttrList <*> mapM desugar mPat
  desugar (PXPcdata l str) = PXPcdata l <$> pure str
  desugar (PXPatTag l pat) = PXPatTag l <$> desugar pat
  desugar (PXRPats l patList) = PXRPats l <$> mapM desugar patList
  desugar (PQuasiQuote l str0 str1) = PQuasiQuote l <$> pure str0 <*> pure str1
  desugar (PBangPat l pat) = PBangPat l <$> desugar pat

instance Desugarable Desugar (WarningText l) where
  desugar (DeprText l str) = DeprText l <$> pure str
  desugar (WarnText l str) = WarnText l <$> pure str

instance Desugarable Desugar (RuleVar l) where
  desugar (RuleVar l name) = RuleVar l <$> desugar name
  desugar (TypedRuleVar l name ty) = TypedRuleVar l <$> desugar name <*> desugar ty

instance Desugarable Desugar (Rule l) where
  desugar (Rule l str mActivation mRuleValList exp0 exp1) = Rule l <$> pure str <*> mapM desugar mActivation <*> mapM (sequence . fmap desugar) mRuleValList <*> desugar exp0 <*> desugar exp1

instance Desugarable Desugar (Activation l) where
  desugar (ActiveFrom l int) = ActiveFrom l <$> pure int
  desugar (ActiveUntil l int) = ActiveUntil l <$> pure int

instance Desugarable Desugar (Overlap l) where
  desugar (NoOverlap l) = pure $ NoOverlap l
  desugar (Overlap l) = pure $ Overlap l
  desugar (Incoherent l) = pure $ Incoherent l

instance Desugarable Desugar (ModulePragma l) where
  desugar (LanguagePragma l nameList) = LanguagePragma l <$> mapM desugar nameList
  desugar (OptionsPragma l mTool str) = OptionsPragma l <$> mapM pure mTool <*> pure str
  desugar (AnnModulePragma l annotation) = AnnModulePragma l <$> desugar annotation

instance Desugarable Desugar (CallConv l) where
  desugar (StdCall l) = pure $ StdCall l
  desugar (CCall l) = pure $ CCall l
  desugar (CPlusPlus l) = pure $ CPlusPlus l
  desugar (DotNet l) = pure $ DotNet l
  desugar (Jvm l) = pure $ Jvm l
  desugar (Js l) = pure $ Js l
  desugar (JavaScript l) = pure $ JavaScript l
  desugar (CApi l) = pure $ CApi l

instance Desugarable Desugar (Safety l) where
  desugar (PlayRisky l) = pure $ PlayRisky l
  desugar (PlaySafe l bool) = PlaySafe l <$> pure bool
  desugar (PlayInterruptible l) = pure $ PlayInterruptible l

instance Desugarable Desugar (Splice l) where
  desugar (IdSplice l str) = IdSplice l <$> pure str
  desugar (ParenSplice l exp) = ParenSplice l <$> desugar exp

instance Desugarable Desugar (Bracket l) where
  desugar (ExpBracket l exp) = ExpBracket l <$> desugar exp
  desugar (PatBracket l pat) = PatBracket l <$> desugar pat
  desugar (TypeBracket l ty) = TypeBracket l <$> desugar ty
  desugar (DeclBracket l declList) = DeclBracket l <$> mapM desugar declList

instance Desugarable Desugar (XAttr l) where
  desugar (XAttr l xName exp) = XAttr l <$> desugar xName <*> desugar exp

instance Desugarable Desugar (XName l) where
  desugar (XName l str) = XName l <$> pure str
  desugar (XDomName l str0 str1) = XDomName l <$> pure str0 <*> pure str1

instance Desugarable Desugar (Exp l) where
  desugar (Var l qName) = increase (Var l <$> desugar qName)
  desugar (IPVar l ipName) = increase (IPVar l <$> desugar ipName)
  desugar (Con l qName) = increase (Con l <$> desugar qName)
  desugar (Lit l literal) = increase (Lit l <$> desugar literal)
  desugar (InfixApp l exp0 qOp exp1) = increase (InfixApp l <$> desugar exp0 <*> desugar qOp <*> desugar exp1)
  desugar (App l exp0 exp1) = increase (App l <$> desugar exp0 <*> desugar exp1)
  desugar (NegApp l exp) = increase (NegApp l <$> desugar exp)
  desugar (Lambda l patList exp) = increase (Lambda l <$> mapM desugar patList <*> desugar exp)
  desugar (Let l binds exp) = increase (Let l <$> desugar binds <*> desugar exp)
  desugar (If l exp0 exp1 exp2) = increase (If l <$> desugar exp0 <*> desugar exp1 <*> desugar exp2)
  desugar (MultiIf l guardedRhsList) = increase (MultiIf l <$> mapM desugar guardedRhsList)
  desugar (Case l exp altList) = increase (Case l <$> desugar exp <*> mapM desugar altList)
  desugar (Do l stmtList) = increase (Do l <$> mapM desugar stmtList)
  desugar (MDo l stmtList) = increase (MDo l <$> mapM desugar stmtList)
  desugar (Tuple l boxed expList) = increase (Tuple l <$> pure boxed <*> mapM desugar expList)
  desugar (TupleSection l boxed mExpList) = increase (TupleSection l <$> pure boxed <*> (mapM . mapM) desugar mExpList)
  desugar (List l expList) = increase (List l <$> mapM desugar expList)
  desugar (ParArray l expList) = increase (List l <$> mapM desugar expList)
  desugar (Paren l exp) = increase (Paren l <$> desugar exp)
  desugar (LeftSection l exp qOp) = increase (LeftSection l <$> desugar exp <*> desugar qOp)
  desugar (RightSection l qOp exp) = increase (RightSection l <$> desugar qOp <*> desugar exp)
  desugar (RecConstr l qName fieldUpdateList) = increase (RecConstr l <$> desugar qName <*> mapM desugar fieldUpdateList)
  desugar (RecUpdate l exp fieldUpdateList) = increase (RecUpdate l <$> desugar exp <*> mapM desugar fieldUpdateList)
  desugar (EnumFrom l exp) = increase (EnumFrom l <$> desugar exp)
  desugar (EnumFromTo l exp0 exp1) = increase (EnumFromTo l <$> desugar exp0 <*> desugar exp1)
  desugar (EnumFromThen l exp0 exp1) = increase (EnumFromThen l <$> desugar exp0 <*> desugar exp1)
  desugar (EnumFromThenTo l exp0 exp1 exp2) = increase (EnumFromThenTo l <$> desugar exp0 <*> desugar exp1 <*> desugar exp2)
  desugar (ParArrayFromTo l exp0 exp1) = increase (ParArrayFromTo l <$> desugar exp0 <*> desugar exp1)
  desugar (ParArrayFromThenTo l exp0 exp1 exp2) = increase (ParArrayFromThenTo l <$> desugar exp0 <*> desugar exp1 <*> desugar exp2)
  desugar (ListComp l exp qualStmtList) = increase (ListComp l <$> desugar exp <*> mapM desugar qualStmtList)
  desugar (ParComp l exp qualStmtListList) = increase (ParComp l <$> desugar exp <*> mapM (sequence . fmap desugar) qualStmtListList)
  desugar (ExpTypeSig l exp ty) = increase (ExpTypeSig l <$> desugar exp <*> desugar ty)
  desugar (VarQuote l qName) = increase (VarQuote l <$> desugar qName)
  desugar (TypQuote l qName) = increase (TypQuote l <$> desugar qName)
  desugar (BracketExp l bracket) = increase (BracketExp l <$> desugar bracket)
  desugar (SpliceExp l splice) = increase (SpliceExp l <$> desugar splice)
  desugar (QuasiQuote l str0 str1) = increase (QuasiQuote l <$> pure str0 <*> pure str1)
  desugar (XTag l xName xAttrList mExp expList) = increase (XTag l <$> desugar xName <*> mapM desugar xAttrList <*> mapM desugar mExp <*> mapM desugar expList)
  desugar (XETag l xName xAttrList mExp) = increase (XETag l <$> desugar xName <*> mapM desugar xAttrList <*> mapM desugar mExp)
  desugar (XPcdata l str) = increase (XPcdata l <$> pure str)
  desugar (XExpTag l exp) = increase (XExpTag l <$> desugar exp)
  desugar (XChildTag l expList) = increase (XChildTag l <$> mapM desugar expList)
  desugar (CorePragma l str exp) = increase (CorePragma l <$> pure str <*> desugar exp)
  desugar (SCCPragma l str exp) = increase (SCCPragma l <$> pure str <*> desugar exp)
  desugar (GenPragma l str pair0 pair1 exp) = increase (GenPragma l <$> pure str <*> pure pair0 <*> pure pair1 <*> desugar exp)
  desugar (Proc l pat exp) = increase (Proc l <$> desugar pat <*> desugar exp)
  desugar (LeftArrApp l exp0 exp1) = increase (LeftArrApp l <$> desugar exp0 <*> desugar exp1)
  desugar (RightArrApp l exp0 exp1) = increase (RightArrApp l <$> desugar exp0 <*> desugar exp1)
  desugar (LeftArrHighApp l exp0 exp1) = increase (LeftArrHighApp l <$> desugar exp0 <*> desugar exp1)
  desugar (RightArrHighApp l exp0 exp1) = increase (RightArrHighApp l <$> desugar exp0 <*> desugar exp1)
  desugar (LCase l altList) = increase (LCase l <$> mapM desugar altList)
  desugar (ExprHole l) = increase (pure $ ExprHole l)

instance Desugarable Desugar (Sign l) where
  desugar (Signless l) = pure $ Signless l
  desugar (Negative l) = pure $ Negative l

instance Desugarable Desugar (Literal l) where
  desugar (Char l c str) = Char l <$> pure c <*> pure str
  desugar (String l str0 str1) = String l <$> pure str0 <*> pure str1
  desugar (Int l int str) = Int l <$> pure int <*> pure str
  desugar (Frac l rat str) = Frac l <$> pure rat <*> pure str
  desugar (PrimInt l int str) = PrimInt l <$> pure int <*> pure str
  desugar (PrimWord l int str) = PrimWord l <$> pure int <*> pure str
  desugar (PrimFloat l rat str) = PrimFloat l <$> pure rat <*> pure str
  desugar (PrimDouble l rat str) = PrimDouble l <$> pure rat <*> pure str
  desugar (PrimChar l c str) = PrimChar l <$> pure c <*> pure str
  desugar (PrimString l str0 str1) = PrimString l <$> pure str0 <*> pure str1

instance Desugarable Desugar (Asst l) where
  desugar (ClassA l qName tyList) = ClassA l <$> desugar qName <*> mapM desugar tyList
  desugar (AppA l name tyList) = AppA l <$> desugar name <*> mapM desugar tyList
  desugar (InfixA l ty0 qName ty1) =  InfixA l <$> desugar ty0 <*> desugar qName <*> desugar ty1
  desugar (IParam l ipName ty) = IParam l <$> desugar ipName <*> desugar ty
  desugar (EqualP l ty0 ty1) = EqualP l <$> desugar ty0 <*> desugar ty1
  desugar (ParenA l asst) = ParenA l <$> desugar asst
  desugar (WildCardA l mName) = WildCardA l <$> mapM desugar mName

instance Desugarable Desugar (Context l) where
  desugar (CxSingle l asst) = CxSingle l <$> desugar asst
  desugar (CxTuple l asstList) = CxTuple l <$> mapM desugar asstList
  desugar (CxEmpty l) = pure $ CxEmpty l

instance Desugarable Desugar (FunDep l) where
  desugar (FunDep l nameList0 nameList1) = FunDep l <$> mapM desugar nameList0 <*> mapM desugar nameList1

instance Desugarable Desugar (Kind l) where
  desugar (KindStar l) = pure $ KindStar l
  desugar (KindFn l kind0 kind1) = KindFn l <$> desugar kind0 <*> desugar kind1
  desugar (KindParen l kind) = KindParen l <$> desugar kind
  desugar (KindVar l qName) = KindVar l <$> desugar qName
  desugar (KindApp l kind0 kind1) = KindApp l <$> desugar kind0 <*> desugar kind1
  desugar (KindTuple l kindList) = KindTuple l <$> mapM desugar kindList
  desugar (KindList l kind) = KindList l <$> desugar kind

instance Desugarable Desugar (TyVarBind l) where
  desugar (KindedVar l name kind) = KindedVar l <$> desugar name <*> desugar kind
  desugar (UnkindedVar l name) = UnkindedVar l <$> desugar name

instance Desugarable Desugar (Promoted l) where
  desugar (PromotedInteger l int str) = PromotedInteger l <$> pure int <*> pure str
  desugar (PromotedString l str0 str1) = PromotedString l <$> pure str0 <*> pure str1
  desugar (PromotedCon l bool qName) = PromotedCon l <$> pure bool <*> desugar qName
  desugar (PromotedList l bool tyList) = PromotedList l <$> pure bool <*> mapM desugar tyList
  desugar (PromotedTuple l tyList) = PromotedTuple l <$> mapM desugar tyList
  desugar (PromotedUnit l) = pure $ PromotedUnit l

instance Desugarable Desugar (Type l) where
  desugar (TyForall l mTyVarBindList mContext ty) = TyForall l <$> mapM (sequence . fmap desugar) mTyVarBindList <*> mapM desugar mContext <*> desugar ty
  desugar (TyFun l ty0 ty1) = TyFun l <$> desugar ty0 <*> desugar ty1
  desugar (TyTuple l boxed tyList) = TyTuple l <$> pure boxed <*> mapM desugar tyList
  desugar (TyList l ty) = TyList l <$> desugar ty
  desugar (TyParArray l ty) = TyParArray l <$> desugar ty
  desugar (TyApp l ty0 ty1) = TyApp l <$> desugar ty0 <*> desugar ty1
  desugar (TyVar l name) = TyVar l <$> desugar name
  desugar (TyCon l qName) = TyCon l <$> desugar qName
  desugar (TyParen l ty) = TyParen l <$> desugar ty
  desugar (TyInfix l ty0 qName ty1) = TyInfix l <$> desugar ty0 <*> desugar qName <*> desugar ty1
  desugar (TyKind l ty kind) = TyKind l <$> desugar ty <*> desugar kind
  desugar (TyPromoted l ty) = TyPromoted l <$> desugar ty
  desugar (TyEquals l ty0 ty1) = TyEquals l <$> desugar ty0 <*> desugar ty1
  desugar (TySplice l splice) = TySplice l <$> desugar splice
  desugar (TyBang l bangTy ty) = TyBang l <$> desugar bangTy <*> desugar ty
  desugar (TyWildCard l mName) = TyWildCard l <$> mapM desugar mName

instance Desugarable Desugar (GuardedRhs l) where
  desugar (GuardedRhs l stmtList exp) = GuardedRhs l <$> mapM desugar stmtList <*> desugar exp

instance Desugarable Desugar (Rhs l) where
  desugar (UnGuardedRhs l exp) = UnGuardedRhs l <$> desugar exp
  desugar (GuardedRhss l guardedRhsList) = GuardedRhss l <$> mapM desugar guardedRhsList

instance Desugarable Desugar (BangType l) where
  desugar (BangedTy l) = pure $ BangedTy l
  desugar (UnpackedTy l) = pure $ UnpackedTy l

instance Desugarable Desugar (InstDecl l) where
  desugar (InsDecl l decl) = InsDecl l <$> desugar decl
  desugar (InsType l ty0 ty1) = InsType l <$> desugar ty0 <*> desugar ty1
  desugar (InsData l dataOrNew ty qualConDeclList mDeriving) = InsData l <$> desugar dataOrNew <*> desugar ty <*> mapM desugar qualConDeclList <*> mapM desugar mDeriving
  desugar (InsGData l dataOrNew ty mKind gadtDeclList mDeriving) = InsGData l <$> desugar dataOrNew <*> desugar ty <*> mapM desugar mKind <*> mapM desugar gadtDeclList <*> mapM desugar mDeriving

instance Desugarable Desugar (ClassDecl l) where
  desugar (ClsDecl l decl) = ClsDecl l <$> desugar decl
  desugar (ClsDataFam l mContext declHead mKind) = ClsDataFam l <$> mapM desugar mContext <*> desugar declHead <*> mapM desugar mKind
  desugar (ClsTyFam l declHead mKind) = ClsTyFam l <$> desugar declHead <*> mapM desugar mKind
  desugar (ClsTyDef l ty0 ty1) = ClsTyDef l <$> desugar ty0 <*> desugar ty1
  desugar (ClsDefSig l name ty) = ClsDefSig l <$> desugar name <*> desugar ty

instance Desugarable Desugar (GadtDecl l) where
  desugar (GadtDecl l name mFieldDeclList ty) = GadtDecl l <$> desugar name <*> mapM (sequence . fmap desugar) mFieldDeclList <*> desugar ty

instance Desugarable Desugar (FieldDecl l) where
  desugar (FieldDecl l nameList ty) = FieldDecl l <$> mapM desugar nameList <*> desugar ty

instance Desugarable Desugar (ConDecl l) where
  desugar (ConDecl l name tyList) = ConDecl l <$> desugar name <*> mapM desugar tyList
  desugar (InfixConDecl l ty0 name ty1) = InfixConDecl l <$> desugar ty0 <*> desugar name <*> desugar ty1
  desugar (RecDecl l name fieldDeclList) = RecDecl l <$> desugar name <*> mapM desugar fieldDeclList

instance Desugarable Desugar (QualConDecl l) where
  desugar (QualConDecl l mTyVarBindList mContext conDecl) = QualConDecl l <$> mapM (sequence . fmap desugar) mTyVarBindList <*> mapM desugar mContext <*> desugar conDecl

instance Desugarable Desugar (Match l) where
  desugar (Match l name patList rhs mBinds) = Match l <$> desugar name <*> mapM desugar patList <*> desugar rhs <*> mapM desugar mBinds
  desugar (InfixMatch l pat name patList rhs mBinds) = InfixMatch l <$> desugar pat <*> desugar name <*> mapM desugar patList <*> desugar rhs <*> mapM desugar mBinds

instance Desugarable Desugar (IPBind l) where
  desugar (IPBind l ipName exp) = IPBind l <$> desugar ipName <*> desugar exp

instance Desugarable Desugar (Binds l) where
  desugar (BDecls l declList) = BDecls l <$> mapM desugar declList
  desugar (IPBinds l ipBindList) = IPBinds l <$> mapM desugar ipBindList

instance Desugarable Desugar (Deriving l) where
  desugar (Deriving l instRuleList) = Deriving l <$> mapM desugar instRuleList

instance Desugarable Desugar (InstHead l) where
  desugar (IHCon l qName) = IHCon l <$> desugar qName
  desugar (IHInfix l ty qName) = IHInfix l <$> desugar ty <*> desugar qName
  desugar (IHParen l instHead) = IHParen l <$> desugar instHead
  desugar (IHApp l instHead ty) = IHApp l <$> desugar instHead <*> desugar ty

instance Desugarable Desugar (InstRule l) where
  desugar (IRule l mTyVarBindList mContext instHead) = IRule l <$> mapM (sequence . fmap desugar) mTyVarBindList <*> mapM desugar mContext <*> desugar instHead
  desugar (IParen l instRule) = IParen l <$> desugar instRule

instance Desugarable Desugar (DeclHead l) where
  desugar (DHead l name) = DHead l <$> desugar name
  desugar (DHInfix l tyVarBind name) = DHInfix l <$> desugar tyVarBind <*> desugar name
  desugar (DHParen l declHead) = DHParen l <$> desugar declHead
  desugar (DHApp l declHead tyVarBind) = DHApp l <$> desugar declHead <*> desugar tyVarBind

instance Desugarable Desugar (DataOrNew l) where
  desugar (DataType l) = pure $ DataType l
  desugar (NewType l) = pure $ NewType l

instance Desugarable Desugar (Role l) where
  desugar (Nominal l) = pure $ Nominal l
  desugar (Representational l) = pure $ Representational l
  desugar (Phantom l) = pure $ Phantom l
  desugar (RoleWildcard l) = pure $ RoleWildcard l

instance Desugarable Desugar (BooleanFormula l) where
  desugar (VarFormula l name) = VarFormula l <$> desugar name
  desugar (AndFormula l booleanFormulaList) = AndFormula l <$> mapM desugar booleanFormulaList
  desugar (OrFormula l booleanFormulaList) = OrFormula l <$> mapM desugar booleanFormulaList
  desugar (ParenFormula l booleanFormula) = ParenFormula l <$> desugar booleanFormula

instance Desugarable Desugar (Annotation l) where
  desugar (Ann l name exp) = Ann l <$> desugar name <*> desugar exp
  desugar (TypeAnn l name exp) = TypeAnn l <$> desugar name <*> desugar exp
  desugar (ModuleAnn l exp) = ModuleAnn l <$> desugar exp

instance Desugarable Desugar (TypeEqn l) where
  desugar (TypeEqn l ty0 ty1) = TypeEqn l <$> desugar ty0 <*> desugar ty1

-- map-Pair-first
mapP :: (a -> Desugar a) -> (a, b) -> Desugar (a, b)
mapP f (a, b) =
  let
    D (info, a') = f a
  in
    D (info, (a', b))

instance Desugarable Desugar (Decl l) where
  desugar (TypeDecl l declHead ty) = TypeDecl l <$> desugar declHead <*> desugar ty
  desugar (TypeFamDecl l declHead mKind) = TypeFamDecl l <$> desugar declHead <*> mapM desugar mKind
  desugar (ClosedTypeFamDecl l declHead mKind tyEqnList) = ClosedTypeFamDecl l <$> desugar declHead <*> mapM desugar mKind <*> mapM desugar tyEqnList
  desugar (DataDecl l dataOrNew mContext declHead qualConDeclList mDeriving) = DataDecl l <$> desugar dataOrNew <*> mapM desugar mContext <*> desugar declHead <*> mapM desugar qualConDeclList <*> mapM desugar mDeriving
  desugar (GDataDecl l dataOrNew mContext declHead mKind gadtDeclList mDeriving) = GDataDecl l <$> desugar dataOrNew <*> mapM desugar mContext <*> desugar declHead <*> mapM desugar mKind <*> mapM desugar gadtDeclList <*> mapM desugar mDeriving
  desugar (DataFamDecl l mContext declHead mKind) = DataFamDecl l <$> mapM desugar mContext <*> desugar declHead <*> mapM desugar mKind
  desugar (TypeInsDecl l ty0 ty1) = TypeInsDecl l <$> desugar ty0 <*> desugar ty1
  desugar (DataInsDecl l dataOrNew ty qualConDeclList mDeriving) = DataInsDecl l <$> desugar dataOrNew <*> desugar ty <*> mapM desugar qualConDeclList <*> mapM desugar mDeriving
  desugar (GDataInsDecl l dataOrNew ty mKind gadtDeclList mDeriving) = GDataInsDecl l <$> desugar dataOrNew <*> desugar ty <*> mapM desugar mKind <*> mapM desugar gadtDeclList <*> mapM desugar mDeriving
  desugar (ClassDecl l mContext declHead funDepList mClassDeclList) = ClassDecl l <$> mapM desugar mContext <*> desugar declHead <*> mapM desugar funDepList <*> (mapM . mapM) desugar mClassDeclList
  desugar (InstDecl l mOverlap instRule mInstDeclList) = InstDecl l <$> mapM desugar mOverlap <*> desugar instRule <*> (mapM . mapM) desugar mInstDeclList
  desugar (DerivDecl l mOverlap instRule) = DerivDecl l <$> mapM desugar mOverlap <*> desugar instRule
  desugar (InfixDecl l assoc mInt opList) = InfixDecl l <$> desugar assoc <*> pure mInt <*> mapM desugar opList
  desugar (DefaultDecl l tyList) = DefaultDecl l <$> mapM desugar tyList
  desugar (SpliceDecl l exp) = SpliceDecl l <$> desugar exp
  desugar (TypeSig l nameList ty) = TypeSig l <$> mapM desugar nameList <*> desugar ty
  desugar (PatSynSig l name mTyVarBindList mContext0 mContext1 ty) = PatSynSig l <$> desugar name <*> (mapM . mapM) desugar mTyVarBindList <*> mapM desugar mContext0 <*> mapM desugar mContext1 <*> desugar ty
  desugar (FunBind l matchList) = FunBind l <$> mapM desugar matchList
  desugar (PatBind l pat rhs mBinds) = PatBind l <$> desugar pat <*> desugar rhs <*> mapM desugar mBinds
  desugar (PatSyn l pat0 pat1 patternSynDirection) = PatSyn l <$> desugar pat0 <*> desugar pat1 <*> desugar patternSynDirection
  desugar (ForImp l callConv mSafety mString name ty) = ForImp l <$> desugar callConv <*> mapM desugar mSafety <*> pure mString <*> desugar name <*> desugar ty
  desugar (ForExp l callConv mString name ty) = ForExp l <$> desugar callConv <*> pure mString <*> desugar name <*> desugar ty
  desugar (RulePragmaDecl l ruleList) = RulePragmaDecl l <$> mapM desugar ruleList
  desugar (DeprPragmaDecl l nameListStringPairList) = DeprPragmaDecl l <$> (mapM . mapP . mapM) desugar nameListStringPairList
  desugar (WarnPragmaDecl l nameListStringPairList) = WarnPragmaDecl l <$> (mapM . mapP . mapM) desugar nameListStringPairList
  desugar (InlineSig l bool mActivation qName) = InlineSig l <$> pure bool <*> mapM desugar mActivation <*> desugar qName
  desugar (InlineConlikeSig l mActivation qName) = InlineConlikeSig l <$> mapM desugar mActivation <*> desugar qName
  desugar (SpecSig l mActivation qName tyList) = SpecSig l <$> mapM desugar mActivation <*> desugar qName <*> mapM desugar tyList
  desugar (SpecInlineSig l bool mActivation qName tyList) = SpecInlineSig l <$> pure bool <*> mapM desugar mActivation <*> desugar qName <*> mapM desugar tyList
  desugar (InstSig l instRule) = InstSig l <$> desugar instRule
  desugar (AnnPragma l annotation) = AnnPragma l <$> desugar annotation
  desugar (MinimalPragma l mBooleanFormula) = MinimalPragma l <$> mapM desugar mBooleanFormula
  desugar (RoleAnnotDecl l qName roleList) = RoleAnnotDecl l <$> desugar qName <*> mapM desugar roleList

instance Desugarable Desugar (Assoc l) where
  desugar (AssocNone l) = pure $ AssocNone l
  desugar (AssocLeft l) = pure $ AssocLeft l
  desugar (AssocRight l) = pure $ AssocRight l

instance Desugarable Desugar (ImportSpec l) where
  desugar (IVar l name) = IVar l <$> desugar name
  desugar (IAbs l namespace name) = IAbs l <$> desugar namespace <*> desugar name
  desugar (IThingAll l name) = IThingAll l <$> desugar name
  desugar (IThingWith l name cNameList) = IThingWith l <$> desugar name <*> mapM desugar cNameList

instance Desugarable Desugar (ImportSpecList l) where
  desugar (ImportSpecList l bool importSpecList) = ImportSpecList l <$> pure bool <*> mapM desugar importSpecList

instance Desugarable Desugar (Namespace l) where
  desugar (NoNamespace l) = pure $ NoNamespace l
  desugar (TypeNamespace l) = pure $ TypeNamespace l
  desugar (PatternNamespace l) = pure $ PatternNamespace l

instance Desugarable Desugar (ExportSpec l) where
  desugar (EVar l qName) = EVar l <$> desugar qName
  desugar (EAbs l namespace qName) = EAbs l <$> desugar namespace <*> desugar qName
  desugar (EThingAll l qName) = EThingAll l <$> desugar qName
  desugar (EThingWith l qName cNameList) = EThingWith l <$> desugar qName <*> mapM desugar cNameList
  desugar (EModuleContents l moduleName) = EModuleContents l <$> desugar moduleName

instance Desugarable Desugar (ExportSpecList l) where
  desugar (ExportSpecList l exportSpecList) = ExportSpecList l <$> mapM desugar exportSpecList

-- missing from the Annotated list
instance Desugarable Desugar (ImportDecl l) where
  desugar (ImportDecl l moduleName bool0 bool1 bool2 mStr mModuleName mImportSpecList) = ImportDecl l <$> desugar moduleName <*> pure bool0 <*> pure bool1 <*> pure bool2 <*> mapM pure mStr <*> mapM desugar mModuleName <*> mapM desugar mImportSpecList

instance Desugarable Desugar (ModuleHead l) where
  desugar (ModuleHead l moduleName mWarningText mExportSpecList) = ModuleHead l <$> desugar moduleName <*> mapM desugar mWarningText <*> mapM desugar mExportSpecList

instance Desugarable Desugar (Module l) where
  desugar (Module l mModuleHead modulePragmaList importDeclList declList) = Module l <$> mapM desugar mModuleHead <*> mapM desugar modulePragmaList <*> mapM desugar importDeclList <*> mapM desugar declList
  desugar (XmlPage l moduleName modulePragmaList xName xAttrList mExp expList) = XmlPage l <$> desugar moduleName <*> mapM desugar modulePragmaList <*> desugar xName <*> mapM desugar xAttrList <*> mapM desugar mExp <*> mapM desugar expList
  desugar (XmlHybrid l mModuleHead modulePragmaList importDeclList declList xName xAttrList mExp expList) = XmlHybrid l <$> mapM desugar mModuleHead <*> mapM desugar modulePragmaList <*> mapM desugar importDeclList <*> mapM desugar declList <*> desugar xName <*> mapM desugar xAttrList <*> mapM desugar mExp <*> mapM desugar expList

instance Desugarable Desugar (CName l) where
  desugar (VarName l name) = VarName l <$> desugar name
  desugar (ConName l name) = ConName l <$> desugar name

instance Desugarable Desugar (Op l) where
  desugar (VarOp l name) = VarOp l <$> desugar name
  desugar (ConOp l name) = ConOp l <$> desugar name

instance Desugarable Desugar (QOp l) where
  desugar (QVarOp l name) = QVarOp l <$> desugar name
  desugar (QConOp l name) = QConOp l <$> desugar name

instance Desugarable Desugar (IPName l) where
  desugar (IPDup l str) = IPDup l <$> pure str
  desugar (IPLin l str) = IPLin l <$> pure str

instance Desugarable Desugar (Name l) where
  desugar (Ident l str) = Ident l <$> pure str
  desugar (Symbol l str) = Symbol l <$> pure str

instance Desugarable Desugar (QName l) where
  desugar (Qual l moduleName name) = Qual l <$> desugar moduleName <*> desugar name
  desugar (UnQual l name) = UnQual l <$> desugar name
  desugar (Special l specialCon) = Special l <$> desugar specialCon

instance Desugarable Desugar (SpecialCon l) where
  desugar (UnitCon l) = pure $ UnitCon l
  desugar (ListCon l) = pure $ ListCon l
  desugar (FunCon l) = pure $ FunCon l
  desugar (TupleCon l boxed int) = TupleCon l <$> pure boxed <*> pure int
  desugar (Cons l) = pure $ Cons l
  desugar (UnboxedSingleCon l) = pure $ UnboxedSingleCon l

instance Desugarable Desugar (ModuleName l) where
  desugar (ModuleName l str) = ModuleName l <$> pure str

instance Desugarable Desugar (PatternSynDirection l) where
  desugar (Unidirectional) = pure Unidirectional
  desugar (ImplicitBidirectional) = pure ImplicitBidirectional
  desugar (ExplicitBidirectional l declList) = ExplicitBidirectional l <$> mapM desugar declList

-- what is (=~=)?
