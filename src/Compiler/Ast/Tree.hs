{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Ast.Tree
    ( Program(..)
    , OperatorsCategories(..)
    , OperatorsCategory(..)
    , CategoryName(..)
    , Fixity(..)
    , Declaration(..)
    , ADTName(..)
    , ADTConName(..)
    , IntfName(..)
    , SymbolName(..)
    , ParamTypeName(..)
    , ADTNameRep
    , ADTConNameRep
    , PropNameRep
    , SymbolNameRep
    , ParamTypeNameRep
    , GenTypeName
    , ADTDeclare(..)
    , TypeComposite(..)
    , Constraint(..)
    , NonRecType(..)
    , UnConType(..)
    , Type(..)
    , Hint(..)
    , IntfDeclare(..)
    , SymbolDeclare(..)
    , AlgebraicDataType(..)
    , AliasAlgebraicDataType(..)
    , Interface(..)
    , Instance(..)
    , Signature(..)
    , SymbolDeclaration(..)
    , MultiSymbolDeclaration(..)
    , SDUnion(..)
    , ADTConstructor(..)
    , Expression(..)
    , UnAltExpression(..)
    , AppExpression(..)
    , ADTAppMatchExpression(..)
    , BoundExpression(..)
    , MultiBoundExpression(..)
    , LetUnion(..)
    , Literal(..)
    , Lambda(..)
    , MultiLambda(..)
    , PatternMatch(..)
    , MultiPatternMatch(..)
    , MatchCase(..)
    , MultiMatchCase(..)
    , MatchingExpression(..)
    , UnAltMatchingExpression(..)
    , HasHint(..)
    , doOnUnCon
    , doOnUnCon'
    -- AstOp monad
    , AstOpT
    , AstOp
    , AstOpRes
    , astOpErr
    , astOpRes
    , runAstOpT
    , runAstOp
    , runAstOpRes
    , execAstOpT
    , execAstOp
    , execAstOpRes
    , removeDeclsBy
    , getDecls
    , getDeclsBy
    , visitPtsInMany
    , visitPtsInCont
    , visitPtsInAdtCon
    , AstOpSingleFilter(..)
    , AstOpFilters(..)
    , filterDecls
    , splitDecls
    , updateTypes
    , safeUpdateTypes
    , updateUnCons
    , updateAllTypes
    , updateAllUnCons
    , lookupTypes
    , lookupUnCons
    , lookupAllTypes
    , lookupAllUnCons
    , safeLookupTypes
    , safeLookupAllTypes
    , getGenSymDecls
    , lookupAdt
    , safeLookupAdt
    , updateAdt
    , setAdt
    , safeSetAdt
    , lookupAlias
    , safeLookupAlias
    , removeAlias
    , updateSymDecl
    , safeUpdateSymDecl
    , updateMultiSymDecl
    , safeUpdateMultiSymDecl
    , lookupSymDecl
    , safeLookupSymDecl
    , lookupGenSymDecl
    , safeLookupGenSymDecl
    , setSymDecl
    , safeSetSymDecl
    , setMultiSymDecl
    , safeSetMultiSymDecl
    , lookupProp
    , safeLookupProp
    , setProp
    , safeSetProp
    , lookupInst
    , safeLookupInst
    , setInst
    , safeSetInst
    , lookupConts
    , setHintsInHeadSymDecl
    , setHintsInHeadMultiSymDecl
    , updateHintsInHeadSymDecl
    , updateHintsInHeadMultiSymDecl
    , safeUpdateHintsInHeadSymDecl
    , safeUpdateHintsInHeadMultiSymDecl
    , lookupHintsInHeadSymDecl
    , safeLookupHintsInHeadSymDecl
    , VisitCallback
    , visitExprsInExpr
    , visitExprsInSd
    , visitExprsInMsd
    -- Cons
    , buildProgram
    , buildDeclarationAdt
    , buildSymbolDeclaration
    , buildMultiSymbolDeclaration
    , buildRealBaseUnCon
    , buildParamBaseUnCon
    , buildRealCompUnCon
    , buildParamCompUnCon
    , buildType
    , buildPtyName
    , buildSymbolName
    , buildGenTypeName
    , buildGenTypeName'
    , buildGenFromBase
    , buildAdt
    , buildAlias
    , buildSig
    , buildHint
    , buildHint'
    , buildEmptyHint
    , buildCont
    , buildHead
    , buildHead'
    , buildBoundExpression
    , buildNaiveBoundUAExpr
    , buildNaiveMultiBoundUAExpr
    , buildSymbolExpr
    --TODO : useless, buildContMatchExpr
    , buildMultiMatchCase
    , buildMultiPattMatch
    -- Update operations
    , updateSymbolNameInSd
    , updateSymbolNameInMsd
    , updateSymbolName
    -- mapping types
    , mapTypesLam
    , mapTypesInConts
    -- Operations
    , realTypes
    , realTypes'
    , paramTypes
    , paramTypes'
    , declarationsFrom
    , adtDeclFrom
    , aliasDeclFrom
    , intfDeclFrom
    , instDeclFrom
    , sigDeclFrom
    , symDeclFrom
    , multiSymDeclFrom
    , adtNameFrom
    , adtNamesFromUnCon
    , adtNamesFromHint
    , adtNamesFromInst
    , adtNamesFromSig
    , allAdtNamesFromSig
    , adtNamesFromCons
    , adtNamesFromComp
    , adtNamesFromCont
    , adtNameFromAlias
    , adtConNameFrom
    , adtConsNameFrom
    , paramTNamesFromAdt
    , boundParamTNamesFromAdt
    , paramTNamesFromComp
    , paramTNamesFromCon
    , paramTNamesFromCont
    , paramTNamesFromType
    , paramTNamesFromIntf
    , paramTNamesFromInst
    , paramTNameFromAlias
    , baseNameFromAlias
    , paramTNamesFromAlias
    , boundParamTNamesFromAlias
    , paramTNameFromUnCon
    , baseNameFromUnCon
    , aliasNameFrom
    , intfNameFrom
    , intfNameFromInst
    , intfNameFromCont
    , symNameFrom
    , symNameFromMultiSymDecl
    , symNameFromSD
    , symNameFromSig
    , symNamesFromMatchExpr
    , typeFromSig
    , typeFromHint
    , adtConsFrom
    , unConFromAlias
    , unConsFromInst
    , argUnConsFrom
    , unConsFromCon
    , unConsFromCont
    , sigsFromIntf
    , symsFromMatches
    , symsFromLam
    , exprFromPatt
    , multiPattMatchFrom
    , multiPattMatchFromMultiLam
    , multiPattMatchFromMultiBound
    , exprCaseFrom
    , exprCaseFromMulti
    , matchesCaseFrom
    , multiCaseFromSingleCase
    , matchesCaseFromMulti
    , exprCasesFromPatt
    , exprCasesFromMultiPatt
    , matchesCasesFromPatt
    , matchesCasesFromMultiPatt
    , casesFromPatt
    , casesFromMultiPatt
    , casesFromMultiLam
    , exprFromSymDecl
    , exprFromLam
    , symDeclFromBound
    , multiSymDeclFromMultiBound
    , exprFromBound
    , symDeclsFromInst
    , applierExprFromApp
    , appliedExprsFromApp
    , unAltExprFrom
    , unAltMExprFrom
    , hintFromExpr
    , hintFromSymD
    , adtDeclareFromAlias
    , contsFromType
    , contsFromIntf
    , contsFromInst
    , typeFromUnCon
    , unConFromType
    , isRealType
    , isParamType
    , ifHint
    , ifHint'
    , msdArgsNo
    , ptyOccurUnCon
    , ptyOccurCont
    , mergePrograms
    , addContsToType
    , addContsToSig
    , addTypeHintToSd
    , addTypeHintToMsd
    , addTypeHint
    , showNonRec
    , showUnCon
    , showUnCons
    , showCont
    -- Operation of string conversion
    , strOfGenName
    -- Fetching state operations
    , stateOfGenName
    -- Operations from strings
    , strToParamTName
    , strToAdtName
) where

import Utils.Fancy
import Utils.Data.Foldable
import Utils.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.RWS
import Data.Semigroup
import Data.List(foldl', partition)
import Data.List.NonEmpty(NonEmpty(..))
import Compiler.Ast.Common

-- Nodes

{- In general, nodes of the ast have an associated state with parametric type a (it should
occur in almost all types). -}

newtype Program a = Program {- TODO: import/export -} [Declaration a] deriving Show

newtype OperatorsCategories a = OpsCatgs [OperatorsCategory a] deriving Show

newtype OperatorsCategory a =
    Catg
        ( CategoryName a
        , [SymbolName a]
        , [CategoryName a]
        , [CategoryName a]
        , Fixity a
        , a
        ) deriving Show

newtype CategoryName a = CatgName (String, a) deriving Show
newtype Fixity a = Fxty (String, a) deriving Show

data Declaration a =
      ADT (AlgebraicDataType a)
    | AliasADT (AliasAlgebraicDataType a)
    | Intf (Interface a)
    | Ins (Instance a)
    | Sig (Signature a)
    | Let (SymbolDeclaration a)
    | LetMulti (MultiSymbolDeclaration a)
    deriving Show

newtype ADTName a = ADTName (String, a) deriving Show                                       -- type A <-
newtype ADTConName a = ADTConName (String, a) deriving Show                                    -- type A = A <-
newtype IntfName a = IntfName (String, a) deriving Show
newtype SymbolName a = SymName (String, a) deriving Show                                    -- -> f x ..or.. -> (+) x
newtype ParamTypeName a = PtyName (String, a) deriving Show                                     -- val f: a <-

type ADTNameRep = String
type ADTConNameRep = String
type PropNameRep = String
type SymbolNameRep = String
type ParamTypeNameRep = String

{- Just a shorthand to avoid to write the entire type. It is not used in the ast right now. -}
--TODO: this can be an adt (a union)
type GenTypeName a = Either (ADTName a) (ParamTypeName a)

newtype ADTDeclare a = ADTDecl (ADTName a, [ParamTypeName a], a) deriving Show --Type name and eventually parametric types
newtype TypeComposite a = TyComp (NonRecType a, [UnConType a], a) deriving Show

newtype Constraint a = Cont (IntfName a, [UnConType a], a) deriving Show
data NonRecType a = Param (ParamTypeName a) | Real (ADTName a) deriving Show
data UnConType a = Singleton (NonRecType a) | Composite (TypeComposite a) deriving Show
newtype Type a = Type ([Constraint a], UnConType a, a) deriving Show

{-
-- A possible different implementation of Type, this would upset a quite huge part of the code base.
-- This implementantion has the advantage of not to have a tree-like structure which, in some cases,
-- could make explode the time-and-space-complexity of algorithms on Type. Type is nothing more than
-- a non-empty list (a Type cannot have zero types) of TypeItem which wraps the notion of NonRecType,
-- with two more tokens, namely `OpenTy` and `CloseTy`: they works as parens between types, so for
-- each `OpenTy`, a `CloseTy` token must exist and viceversa. What follows an `OpenTy` in a list of
-- TypeItem is enclosed until a `CloseTy` is encountered in the list. This implies automatically that
-- it's up to the programmer to build consistent Type values.

data TypeItem a = OpenTy | CloseTy | Ty NonRecType a
data Type a = NonEmpty (TypeItem a)
-}

newtype Hint a = Hint (Maybe (Type a), a) deriving Show

newtype IntfDeclare a = IntfDecl (IntfName a, [Constraint a], [ParamTypeName a], a) deriving Show
newtype SymbolDeclare a = SymDecl (SymbolName a, Hint a, [SymbolName a], a) deriving Show

newtype AlgebraicDataType a = ADTTok (ADTDeclare a, [ADTConstructor a], a) deriving Show

newtype AliasAlgebraicDataType a = AliasTok (ADTDeclare a, UnConType a, a) deriving Show

newtype Interface a = IntfTok (IntfDeclare a, [Signature a], a) deriving Show

{- TODO: create InstanceDeclare or something like this for the header of Instance. -}
newtype Instance a = InstTok (IntfName a, [Constraint a], [UnConType a], [SDUnion a], a) deriving Show

newtype Signature a = SigTok (SymbolName a, Type a, a) deriving Show --Name of the symbol and type

newtype SymbolDeclaration a = SymTok (SymbolDeclare a, Expression a, a) deriving Show
data MultiSymbolDeclaration a = MultiSymTok (SymbolName a) (Hint a) (MultiPatternMatch a) a deriving Show
{- The union between symbol declaration and multiple symbol declaration, in order to represent both once. -}
data SDUnion a =
      SD (SymbolDeclaration a)
    | MSD (MultiSymbolDeclaration a)
    deriving Show

newtype ADTConstructor a = ADTCon (ADTConName a, [UnConType a], a) deriving Show

newtype Expression a = Expr (UnAltExpression a, Hint a, a) deriving Show

data UnAltExpression a =
      App (AppExpression a)
    | Base (SymbolName a)
    | ADTBase (ADTConName a)
    | Match (PatternMatch a)
    | Lam (Lambda a)
    | MultiLam (MultiLambda a)
    | Bound (BoundExpression a)
    | MultiBound (MultiBoundExpression a)
    | Lit (Literal a)
    deriving Show

newtype AppExpression a = AppExpr (Expression a, [Expression a], a) deriving Show
newtype ADTAppMatchExpression a = ADTAppMExpr (ADTConName a, [MatchingExpression a], a) deriving Show
newtype BoundExpression a = BoundExpr (SymbolDeclaration a, Expression a, a) deriving Show
data MultiBoundExpression a = MultiBoundExpr (MultiSymbolDeclaration a) (Expression a) a deriving Show

data LetUnion a =
      BExpr (BoundExpression a)
    | MBExpr (MultiBoundExpression a)
    deriving Show

data Literal a =
      IntLit (Integer, a)
    | DoubleLit (Double, a)
    | CharLit (Char, a)
    | StringLit (String, a)
    deriving Show

newtype Lambda a = Lambda ([SymbolName a], Expression a, a) deriving Show
data MultiLambda a = MultiLambda (MultiPatternMatch a) a deriving Show

newtype PatternMatch a = PattMatch (Expression a, [MatchCase a], a) deriving Show
data MultiPatternMatch a = MultiPattMatch [MultiMatchCase a] a deriving Show

newtype MatchCase a = Case (MatchingExpression a, Expression a, a) deriving Show
data MultiMatchCase a = MultiCase [MatchingExpression a] (Expression a) a deriving Show

data MatchingExpression a = MatchExpr (UnAltMatchingExpression a) a deriving Show

data UnAltMatchingExpression a =
      MADTApp (ADTAppMatchExpression a)
    | MDefault a
    | MLit (Literal a)
    | MBase (SymbolName a)
    | MADTBase (ADTConName a)
    deriving Show

-- Classes: HasHint

{- For tokens which have type-hinting. -}
class HasHint tok where
    hintOf :: tok a -> Hint a

-- Instances: HasHint

instance HasHint SymbolDeclare where
    hintOf (SymDecl (_, h, _, _)) = h

instance HasHint SymbolDeclaration where
    hintOf (SymTok (sd, _, _)) = hintOf sd

instance HasHint MultiSymbolDeclaration where
    hintOf (MultiSymTok _ h _ _) = h

instance HasHint Expression where
    hintOf (Expr (_, h, _)) = h

instance HasHint Hint where
    hintOf = id

-- Instances: HasArgs

instance HasArgs (AlgebraicDataType a) (ParamTypeName a) where
    argsOf (ADTTok (ADTDecl (_, ps, _), _, _)) = ps

instance HasArgs (AliasAlgebraicDataType a) (ParamTypeName a) where
    argsOf (AliasTok (ADTDecl (_, ps, _), _, _)) = ps

instance HasArgs (Interface a) (ParamTypeName a) where
    argsOf (IntfTok (IntfDecl (_, _, ps, _), _, _)) = ps

instance HasArgs (Instance a) (UnConType a) where
    argsOf (InstTok (_, _, ts, _, _)) = ts

instance HasArgs (SymbolDeclaration a) (SymbolName a) where
    argsOf (SymTok (SymDecl (_, _, syms, _), _, _)) = syms

instance HasArgs (MultiSymbolDeclaration a) (MultiMatchCase a) where
    argsOf (MultiSymTok _ _ mpm _) = casesFromMultiPatt mpm

instance HasArgs (Signature a) (Type a) where
    argsOf (SigTok (_, ty, _)) = [ty]

instance HasArgs (Constraint a) (UnConType a) where
    argsOf (Cont (_, ts, _)) = ts

instance HasArgs (UnConType a) (UnConType a) where
    argsOf = argUnConsFrom

instance HasArgs (Type a) (UnConType a) where
    argsOf = argUnConsFrom . unConFromType

instance HasArgs (MultiMatchCase a) (MatchingExpression a) where
    argsOf (MultiCase ms _ _) = ms

-- Instances: HasHead

instance HasHead (SymbolDeclaration a) (SymbolName a) where
    headOf (SymTok (SymDecl (sn, _, _, _), _, _)) = sn

instance HasHead (MultiSymbolDeclaration a) (SymbolName a) where
    headOf (MultiSymTok sn _ _ _) = sn

instance HasHead (Signature a) (SymbolName a) where
    headOf (SigTok (sn, _, _)) = sn

instance HasHead (Constraint a) (IntfName a) where
    headOf (Cont (pn, _, _)) = pn

-- Instances: Eq and Ord

instance Eq (ADTName a) where
    (==) n n' = repOf n == repOf n'

instance Eq (ADTConName a) where
    (==) n n' = repOf n == repOf n'

instance Eq (IntfName a) where
    (==) n n' = repOf n == repOf n'

instance Eq (SymbolName a) where
    (==) n n' = repOf n == repOf n'

instance Eq (ParamTypeName a) where
    (==) n n' = repOf n == repOf n'

instance Ord (ADTName a) where
    compare n n' = repOf n `compare` repOf n'

instance Ord (ADTConName a) where
    compare n n' = repOf n `compare` repOf n'

instance Ord (IntfName a) where
    compare n n' = repOf n `compare` repOf n'

instance Ord (SymbolName a) where
    compare n n' = repOf n `compare` repOf n'

instance Ord (ParamTypeName a) where
    compare n n' = repOf n `compare` repOf n'

-- Instances: Semigroup and Monoid

{- Semigroup instance for UnConType. This is UNSAFE and should be used with care, because it literally changes
a piece of the abstract syntax tree; the situations where the concatenation of values of type UnConType is needed
should be quite rare. Anyway, this offers a shorthand to concatenate types.
NB: the Monoid instance does not exist (currently) because it is not contemplated for UnConType to have a neutral
element. -}
instance Semigroup (UnConType s) where
    (<>) ty @ (Singleton nrty) ty' = let st = stateOf ty in
        Composite $ TyComp (nrty, [ty'], st)
    (<>) comp @ (Composite (TyComp (nrty, ts, _))) ty' = let st' = stateOf comp in
        Composite $ TyComp (nrty, ts ++ [ty'], st')

    {- Overriding to ensure the correct and wanted implementation. -}
    sconcat (ty :| ts) = chaining ty ts
        where
            chaining ty' [] = ty'
            chaining ty' (ty'' : t) = chaining (ty' <> ty'') t

-- Instances: Functor

pairTrans :: (a -> b) -> (c, a) -> (c, b)
pairTrans f (s, x) = (s, f x)

instance Functor OperatorsCategories where
    fmap f (OpsCatgs catgs) = OpsCatgs $ map (fmap f) catgs

instance Functor OperatorsCategory where
    fmap f (Catg (catgname, symnames, names, names', fxty, st)) =
        Catg (fmap f catgname, map (fmap f) symnames, map (fmap f) names, map (fmap f) names', fmap f fxty, f st)

instance Functor Fixity where
    fmap f (Fxty fxty) = Fxty $ pairTrans f fxty

instance Functor ADTName where
    fmap f (ADTName adtname) = ADTName $ pairTrans f adtname

instance Functor ADTConName where
    fmap f (ADTConName conname) = ADTConName $ pairTrans f conname

instance Functor IntfName where
    fmap f (IntfName intfname) = IntfName $ pairTrans f intfname

instance Functor SymbolName where
    fmap f (SymName symname) = SymName $ pairTrans f symname

instance Functor ParamTypeName where
    fmap f (PtyName ptyname) = PtyName $ pairTrans f ptyname

instance Functor CategoryName where
    fmap f (CatgName catgname) = CatgName $ pairTrans f catgname

instance Functor ADTDeclare where
    fmap f (ADTDecl (rty, ps, st)) = ADTDecl (fmap f rty, map (fmap f) ps, f st)

instance Functor IntfDeclare where
    fmap f (IntfDecl (intfname, cs, ps, st)) = IntfDecl (fmap f intfname, map (fmap f) cs, map (fmap f) ps, f st)

instance Functor SymbolDeclare where
    fmap f (SymDecl (symname, hint, syms, st)) = SymDecl (fmap f symname, fmap f hint, map (fmap f) syms, f st)

instance Functor TypeComposite where
    fmap f (TyComp (nrty, ts, st)) = TyComp (fmap f nrty, map (fmap f) ts, f st)

instance Functor ADTConstructor where
    fmap f (ADTCon (conname, ts, st)) = ADTCon (fmap f conname, map (fmap f) ts, f st)

instance Functor Hint where
    fmap f (Hint (Nothing, st)) = Hint (Nothing, f st)
    fmap f (Hint (Just ty, st)) = Hint (Just $ fmap f ty, f st)

instance Functor Literal where
    fmap f (IntLit (l, st)) = IntLit (l, f st)
    fmap f (DoubleLit (l, st)) = DoubleLit (l, f st)
    fmap f (CharLit (l, st)) = CharLit (l, f st)
    fmap f (StringLit (l, st)) = StringLit (l, f st)

instance Functor Expression where
    fmap f (Expr (uaexpr, hint, st)) = Expr (fmap f uaexpr, fmap f hint, f st)

instance Functor MatchingExpression where
    fmap f (MatchExpr uamexpr st) = MatchExpr (fmap f uamexpr) (f st)

instance Functor UnAltExpression where
    fmap f (App x) = App $ fmap f x
    fmap f (Base x) = Base $ fmap f x
    fmap f (ADTBase x) = ADTBase $ fmap f x
    fmap f (Match x) = Match $ fmap f x
    fmap f (Lam x) = Lam $ fmap f x
    fmap f (MultiLam x) = MultiLam $ fmap f x
    fmap f (Bound x) = Bound $ fmap f x
    fmap f (MultiBound x) = MultiBound $ fmap f x
    fmap f (Lit x) = Lit $ fmap f x

instance Functor UnAltMatchingExpression where
    fmap f (MADTApp x) = MADTApp $ fmap f x
    fmap f (MDefault st) = MDefault $ f st
    fmap f (MLit x) = MLit $ fmap f x
    fmap f (MBase x) = MBase $ fmap f x
    fmap f (MADTBase x) = MADTBase $ fmap f x

instance Functor AppExpression where
    fmap f (AppExpr (e, es, st)) = AppExpr (fmap f e, map (fmap f) es, f st)

instance Functor ADTAppMatchExpression where
    fmap f (ADTAppMExpr (conname, mexprs, st)) = ADTAppMExpr (fmap f conname, map (fmap f) mexprs, f st)

instance Functor BoundExpression where
    fmap f (BoundExpr (sym, expr, st)) = BoundExpr (fmap f sym, fmap f expr, f st)

instance Functor MultiBoundExpression where
    fmap f (MultiBoundExpr msd expr st) = MultiBoundExpr (fmap f msd) (fmap f expr) (f st)

instance Functor Lambda where
    fmap f (Lambda (syms, expr, st)) = Lambda (map (fmap f) syms, fmap f expr, f st)

instance Functor MultiLambda where
    fmap f (MultiLambda mpm st) = MultiLambda (fmap f mpm) (f st)

instance Functor PatternMatch where
    fmap f (PattMatch (expr, mcs, st)) = PattMatch (fmap f expr, map (fmap f) mcs, f st)

instance Functor MultiPatternMatch where
    fmap f (MultiPattMatch multimcs st) = MultiPattMatch (map (fmap f) multimcs) (f st)

instance Functor MatchCase where
    fmap f (Case (mexpr, expr, st)) = Case (fmap f mexpr, fmap f expr, f st)

instance Functor MultiMatchCase where
    fmap f (MultiCase ms expr st) = MultiCase (map (fmap f) ms) (fmap f expr) (f st)

instance Functor Constraint where
    fmap f (Cont (iname, ts, st)) = Cont (fmap f iname, map (fmap f) ts, f st)

instance Functor NonRecType where
    fmap f (Param t) = Param $ fmap f t
    fmap f (Real t) = Real $ fmap f t

instance Functor UnConType where
    fmap f (Singleton t) = Singleton $ fmap f t
    fmap f (Composite t) = Composite $ fmap f t

instance Functor Type where
    fmap f (Type (cs, uty, st)) = Type (map (fmap f) cs, fmap f uty, f st)

instance Functor AlgebraicDataType where
    fmap f (ADTTok (adtdecl, cons, st)) = ADTTok (fmap f adtdecl, map (fmap f) cons, f st)

instance Functor AliasAlgebraicDataType where
    fmap f (AliasTok (adtdecl, ty, st)) = AliasTok (fmap f adtdecl, fmap f ty, f st)

instance Functor Interface where
    fmap f (IntfTok (intfdecl, sigs, st)) = IntfTok (fmap f intfdecl, map (fmap f) sigs, f st)

instance Functor Instance where
    fmap f (InstTok (intfname, cs, ts, syms, st)) =
        InstTok (fmap f intfname, map (fmap f) cs, map (fmap f) ts, map (fmap f) syms, f st)

instance Functor Signature where
    fmap f (SigTok (symname, ty, st)) = SigTok (fmap f symname, fmap f ty, f st)

instance Functor SymbolDeclaration where
    fmap f (SymTok (symdecl, expr, st)) = SymTok (fmap f symdecl, fmap f expr, f st)

instance Functor MultiSymbolDeclaration where
    fmap f (MultiSymTok symname h multipm st) = MultiSymTok (fmap f symname) (fmap f h) (fmap f multipm) (f st)

instance Functor SDUnion where
    fmap f (SD sd) = SD $ fmap f sd
    fmap f (MSD msd) = MSD $ fmap f msd

instance Functor Declaration where
    fmap f (ADT tok) = ADT $ fmap f tok
    fmap f (AliasADT tok) = AliasADT $ fmap f tok
    fmap f (Intf tok) = Intf $ fmap f tok
    fmap f (Ins tok) = Ins $ fmap f tok
    fmap f (Sig tok) = Sig $ fmap f tok
    fmap f (Let tok) = Let $ fmap f tok
    fmap f (LetMulti tok) = LetMulti $ fmap f tok

instance Functor Program where
    fmap f (Program decls) = Program $ map (fmap f) decls

doOnUnCon
    :: UnConType s
    -> (ADTName s -> a)
    -> (ParamTypeName s -> a)
    -> (ADTName s -> [UnConType s] -> a)
    -> (ParamTypeName s -> [UnConType s] -> a)
    -> a
doOnUnCon uty sfr sfp bfr bfp = doOnUnCon' uty sfr sfp (\n ts _ -> bfr n ts) (\n ts _ -> bfp n ts)

{- Same of doOnUnCon, but it handles also the state. -}
doOnUnCon'
    :: UnConType s
    -> (ADTName s -> a)
    -> (ParamTypeName s -> a)
    -> (ADTName s -> [UnConType s] -> s -> a)
    -> (ParamTypeName s -> [UnConType s] -> s -> a)
    -> a
doOnUnCon' (Singleton (Real n)) sfr _ _ _ = sfr n
doOnUnCon' (Singleton (Param n)) _ sfp _ _ = sfp n
doOnUnCon' (Composite (TyComp (Real n, ts, st))) _ _ bfr _ = bfr n ts st
doOnUnCon' (Composite (TyComp (Param n, ts, st))) _ _ _ bfp = bfp n ts st

-- AstOpRes monad (for operations on the tree)

{- A monadic very generic operation on the ast with type `err` for errors and type `a` for return type.
It takes in input a Program and a default value for `err` type. -}
{- TODO: legacy
newtype AstOpRes s err a =
    AstOpRes
        { doOp :: Program s -> err -> Either err (a, Program s)
        }
                -}

type AstOpT s m = StateT (Program s) m
type AstOp s = AstOpT s Identity
type AstOpRes s err = AstOpT s (Either err)

astOpErr :: err -> AstOpRes s err a
astOpErr = lift . Left

{- This is a way to turn a pure ast operation in a "result" ast operation which does exectly the same. -}
astOpRes :: AstOp s a -> AstOpRes s err a
astOpRes pureOp = do
    p <- get
    let (x, p') = runAstOp p pureOp
    put p'
    return x

{- NB: dangerous operation! -}
replaceProg :: Monad m => [Declaration s] -> AstOpT s m ()
replaceProg = put . buildProgram

runAstOpT :: Program s -> AstOpT s m a -> m (a, Program s)
runAstOpT = flip runStateT

runAstOp :: Program s -> AstOp s a -> (a, Program s)
runAstOp p op = runIdentity $ runAstOpT p op

{- It executes a AstOpRes. It needs a Program. -}
runAstOpRes :: Program s -> AstOpRes s err a -> Either err (a, Program s)
runAstOpRes = runAstOpT

execAstOpT :: Monad m => Program s -> AstOpT s m a -> m (Program s)
execAstOpT = flip execStateT

execAstOp :: Program s -> AstOp s a -> Program s
execAstOp p op = runIdentity $ execAstOpT p op

execAstOpRes :: Program s -> AstOpRes s err a -> Either err (Program s)
execAstOpRes = execAstOpT

{- TODO: legacy
astOpMap :: (a -> b) -> AstOpRes s err a -> AstOpRes s err b
astOpMap f (AstOpRes g) = AstOpRes $ \p e -> case g p e of
    Right (x, p) -> Right (f x, p)
    Left err -> Left err

instance Functor (AstOpRes s err) where
    fmap = astOpMap

instance Applicative (AstOpRes s err) where
    pure x = AstOpRes $ \p _ -> Right (x, p)

    (<*>) op op' = AstOpRes $ \p e -> case doOp op p e of
        Left err -> Left err
        {- TODO: not using astOpMap, because the output Program (p') is not discarded, but is it right like this? -}
        Right (f, p') -> (case doOp op' p' e of
            Left err -> Left err
            Right (x', p'') -> Right (f x', p''))

instance Alternative (AstOpRes s err) where
    empty = AstOpRes $ \_ e -> Left e

    {- It offers an alternative: if the first operation (op) fails, it tries the second one (op')
    with a new error value. -}
    (<|>) op op' = AstOpRes $ \p e -> case doOp op p e of
        {- Using the new error value as error (not e). -}
        Left err -> doOp op p err
        right -> right

instance Monad (AstOpRes s err) where
    (>>=) op f = AstOpRes $ \p e -> case doOp op p e of
        Right (x', p') -> doOp (f x') p' e
        Left err -> Left err

instance MonadPlus (AstOpRes s err)

instance MonadFail (AstOpRes s err) where
    fail _ = mzero

changeErrValue :: AstOpRes s err a -> err -> AstOpRes s err a
{- Exploiting <|> which replaces the err value. -}
changeErrValue op err = AstOpRes (\_ _ -> Left err) <|> op

{- <?> is a synonim for changeErrValue. It is inspired by Text.Parsec.Prim.(<?>) which offers a similar
functionality. -}
infix 0 <?>

(<?>) :: AstOpRes s err a -> err -> AstOpRes s err a
(<?>) = changeErrValue

-}

removeDeclsWith :: Monad m => (Declaration s -> Maybe d) -> AstOpT s m [d]
removeDeclsWith select = do
    decls <- getDecls
    let (selected, decls') = splitmap select decls
    replaceProg decls'
    return selected

{- It removes declarations specified by filters and it returns them. -}
removeDeclsBy :: Monad m => AstOpFilters -> AstOpT s m [Declaration s]
removeDeclsBy aof = do
    decls <- getDecls
    let (incl, notIncl) = splitDecls decls aof
    replaceProg notIncl
    return incl

getDecls :: Monad m => AstOpT s m [Declaration s]
getDecls = gets declarationsFrom

getDeclsBy :: Monad m => AstOpFilters -> AstOpT s m [Declaration s]
getDeclsBy aof = do
    decls <- getDecls
    let decls' = filterDecls decls aof
    return decls'

doOnPts :: [ParamTypeName a]
        -> x
        -> (ParamTypeName a -> x -> (ParamTypeName a, x))
        -> ([ParamTypeName a], x)
doOnPts [] x _ = ([], x)
doOnPts (p : t) x f =
    let (p', x') = f p x in
    let (ps, x1) = doOnPts t x' f in
        (p' : ps, x1)

visitPtsInMany :: [tok a]
               -> (tok a -> x -> (ParamTypeName a -> x -> (ParamTypeName a, x)) -> (tok a, x))
               -> x
               -> (ParamTypeName a -> x -> (ParamTypeName a, x))
               -> ([tok a], x)
visitPtsInMany [] _ x _ = ([], x)
visitPtsInMany (tok : t) visit x f =
    let (tok', x') = visit tok x f in
    let (t', x'') = visitPtsInMany t visit x' f in
        (tok' : t', x'')

visitPtsInNRType :: NonRecType a
                 -> x
                 -> (ParamTypeName a -> x -> (ParamTypeName a, x))
                 -> (NonRecType a, x)
visitPtsInNRType (Param pty) x f =
    let (pty', x') = f pty x in
        (Param pty', x')
visitPtsInNRType nrty @ (Real _) x _ = (nrty, x)

visitPtsInUnCon :: UnConType a
                -> x
                -> (ParamTypeName a -> x -> (ParamTypeName a, x))
                -> (UnConType a, x)
visitPtsInUnCon (Singleton nrty) x f =
    let (nrty', x') = visitPtsInNRType nrty x f in
        (Singleton nrty', x')
visitPtsInUnCon (Composite (TyComp (nrty, ts, st))) x f =
    let (nrty', x') = visitPtsInNRType nrty x f in
    let (ts', x'') = visitPtsInMany ts visitPtsInUnCon x' f in
        (Composite $ TyComp (nrty', ts', st), x'')

visitPtsInCont :: Constraint a
               -> x
               -> (ParamTypeName a -> x -> (ParamTypeName a, x))
               -> (Constraint a, x)
visitPtsInCont (Cont (n, ts, st)) x f =
    let (ts', x') = visitPtsInMany ts visitPtsInUnCon x f in
        (Cont (n, ts', st), x')

visitPtsInType :: Type a
               -> x
               -> (ParamTypeName a -> x -> (ParamTypeName a, x))
               -> (Type a, x)
visitPtsInType (Type (conts, uty, st)) x f =
    let (uty', x') = visitPtsInUnCon uty x f in
    let (conts', x'') = visitPtsInMany conts visitPtsInCont x' f in
        (Type (conts', uty', st), x'')

visitPtsInUnConInType
    :: Type a
    -> x
    -> (ParamTypeName a -> x -> (ParamTypeName a, x))
    -> (Type a, x)
visitPtsInUnConInType (Type (conts, uty, st)) x f =
    let (uty', x') = visitPtsInUnCon uty x f in
        (Type (conts, uty', st), x')

visitPtsInContsInType
    :: Type a
    -> x
    -> (ParamTypeName a -> x -> (ParamTypeName a, x))
    -> (Type a, x)
visitPtsInContsInType (Type (conts, uty, st)) x f =
    let (conts', x') = visitPtsInMany conts visitPtsInCont x f in
        (Type (conts', uty, st), x')

visitPtsInSig :: Signature a
              -> x
              -> (ParamTypeName a -> x -> (ParamTypeName a, x))
              -> (Signature a, x)
visitPtsInSig (SigTok (n, ty, st)) x f =
    let (ty', x') = visitPtsInType ty x f in
        (SigTok (n, ty', st), x')

visitPtsInAdtCon :: ADTConstructor a
                 -> x
                 -> (ParamTypeName a -> x -> (ParamTypeName a, x))
                 -> (ADTConstructor a, x)
visitPtsInAdtCon (ADTCon (n, ts, st)) x f =
    let (ts', x') = visitPtsInMany ts visitPtsInUnCon x f in
        (ADTCon (n, ts', st), x')

visitPtsInHint :: Hint a
               -> x
               -> (ParamTypeName a -> x -> (ParamTypeName a, x))
               -> (Hint a, x)
visitPtsInHint (Hint (Nothing, st)) x _ = (Hint (Nothing, st), x)
visitPtsInHint (Hint (Just ty, st)) x f =
    let (ty', x') = visitPtsInType ty x f in
        (Hint (Just ty', st), x')

visitPtsInUAMatchExpr :: UnAltMatchingExpression a
                      -> x
                      -> (ParamTypeName a -> x -> (ParamTypeName a, x))
                      -> (UnAltMatchingExpression a, x)
visitPtsInUAMatchExpr (MADTApp (ADTAppMExpr (con, ms, st))) x f =
    let (ms', x') = visitPtsInMany ms visitPtsInMatchExpr x f in
        (MADTApp (ADTAppMExpr (con, ms', st)), x')
visitPtsInUAMatchExpr ume x _ = (ume, x)

visitPtsInMatchExpr :: MatchingExpression a
                    -> x
                    -> (ParamTypeName a -> x -> (ParamTypeName a, x))
                    -> (MatchingExpression a, x)
visitPtsInMatchExpr (MatchExpr ume st) x f =
    let (ume', x') = visitPtsInUAMatchExpr ume x f in
        (MatchExpr ume' st, x')

visitPtsInMatchCase :: MatchCase a
                    -> x
                    -> (ParamTypeName a -> x -> (ParamTypeName a, x))
                    -> (MatchCase a, x)
visitPtsInMatchCase (Case (m, e, st)) x f =
    let (m', x') = visitPtsInMatchExpr m x f in
    let (e', x'') = visitPtsInExpr e x' f in
        (Case (m', e', st), x'')

visitPtsInPm :: PatternMatch a
             -> x
             -> (ParamTypeName a -> x -> (ParamTypeName a, x))
             -> (PatternMatch a, x)
visitPtsInPm (PattMatch (e, cs, st)) x f =
    let (e', x') = visitPtsInExpr e x f in
    let (cs', x'') = visitPtsInMany cs visitPtsInMatchCase x' f in
        (PattMatch (e', cs', st), x'')

visitPtsInMultiCase
    :: MultiMatchCase a
    -> x
    -> (ParamTypeName a -> x -> (ParamTypeName a, x))
    -> (MultiMatchCase a, x)
visitPtsInMultiCase (MultiCase ms e st) x f =
    let (ms', x') = visitPtsInMany ms visitPtsInMatchExpr x f in
    let (e', x'') = visitPtsInExpr e x' f in
        (MultiCase ms' e' st, x'')

visitPtsInMultiPm
    :: MultiPatternMatch a
    -> x
    -> (ParamTypeName a -> x -> (ParamTypeName a, x))
    -> (MultiPatternMatch a, x)
visitPtsInMultiPm (MultiPattMatch cs st) x f =
    let (cs', x') = visitPtsInMany cs visitPtsInMultiCase x f in
        (MultiPattMatch cs' st, x')

visitPtsInUAExpr
    :: UnAltExpression a
    -> x
    -> (ParamTypeName a -> x -> (ParamTypeName a, x))
    -> (UnAltExpression a, x)
visitPtsInUAExpr (App (AppExpr (e, es, st))) x f =
    let (e', x') = visitPtsInExpr e x f in
    let (es', x'') = visitPtsInMany es visitPtsInExpr x' f in
        (App (AppExpr (e', es', st)), x'')
visitPtsInUAExpr (Match pm) x f =
    let (pm', x') = visitPtsInPm pm x f in
        (Match pm', x')
visitPtsInUAExpr (Lam (Lambda (syms, e, st))) x f =
    let (e', x') = visitPtsInExpr e x f in
        (Lam (Lambda (syms, e', st)), x')
visitPtsInUAExpr (MultiLam (MultiLambda mpm st)) x f =
    let (mpm', x') = visitPtsInMultiPm mpm x f in
        (MultiLam (MultiLambda mpm' st), x')
visitPtsInUAExpr (Bound (BoundExpr (sd, e, st))) x f =
    let (sd', x') = onBindingsOf sd x f in
    let (sd'', x'') = onScopedOf sd' x' f in
    let (e', x''') = visitPtsInExpr e x'' f in
        (Bound (BoundExpr (sd'', e', st)), x''')
visitPtsInUAExpr (MultiBound (MultiBoundExpr msd e st)) x f =
    let (msd', x') = onBindingsOf msd x f in
    let (msd'', x'') = onScopedOf msd' x' f in
    let (e', x''') = visitPtsInExpr e x'' f in
        (MultiBound (MultiBoundExpr msd'' e' st), x''')
visitPtsInUAExpr ue x _ = (ue, x)

visitPtsInExpr :: Expression a
               -> x
               -> (ParamTypeName a -> x -> (ParamTypeName a, x))
               -> (Expression a, x)
visitPtsInExpr (Expr (ue, h, st)) x f =
    let (h', x') = visitPtsInHint h x f in
    let (ue', x'') = visitPtsInUAExpr ue x' f in
        (Expr (ue', h', st), x'')

instance Binder (AlgebraicDataType a) (ParamTypeName a) where
    onBindingsOf (ADTTok (ADTDecl (n, ps, st), cs, tokst)) x f =
        let (ps', x1) = doOnPts ps x f in
            (ADTTok (ADTDecl (n, ps', st), cs, tokst), x1)

    onScopedOf (ADTTok (adtdecl, cs, tokst)) x f =
        let (cs', x') = visitPtsInMany cs visitPtsInAdtCon x f in
            (ADTTok (adtdecl, cs', tokst), x')

instance Binder (Interface a) (ParamTypeName a) where
    onBindingsOf (IntfTok (IntfDecl (n, cs, ps, st), sigs, tokst)) x f =
        let (ps', x') = doOnPts ps x f in
            (IntfTok (IntfDecl (n, cs, ps', st), sigs, tokst), x')

    onScopedOf (IntfTok (IntfDecl (n, cs, ps, st), sigs, tokst)) x f =
        let (cs', x') = visitPtsInMany cs visitPtsInCont x f in
        let (sigs', x'') = visitPtsInMany sigs visitPtsInSig x' f in
            (IntfTok (IntfDecl (n, cs', ps, st), sigs', tokst), x'')

instance Binder (Instance a) (ParamTypeName a) where
    onBindingsOf (InstTok (n, conts, ts, sds, st)) x f =
        let (ts', x') = visitPtsInMany ts visitPtsInUnCon x f in
            (InstTok (n, conts, ts', sds, st), x')

    onScopedOf (InstTok (n, conts, ts, sds, st)) x f =
        let (conts', x') = visitPtsInMany conts visitPtsInCont x f in
        {- Note the use of onBindingOf and onScopedOf (both on symbol declarations), in order to
        treat ParamTypeName values in symbol declarations as scoped. -}
        let (sds', x'') = visitPtsInMany sds onBindingsOf x' f in
        let (sds'', x''') = visitPtsInMany sds' onScopedOf x'' f in
            (InstTok (n, conts', ts, sds'', st), x''')

instance Binder (SymbolDeclaration a) (ParamTypeName a) where
    onBindingsOf (SymTok (SymDecl (n, h, syms, st), e, tokst)) x f =
        {- The hint defines the bindings! -}
        let (h', x') = visitPtsInHint h x f in
            (SymTok (SymDecl (n, h', syms, st), e, tokst), x')

    onScopedOf (SymTok (SymDecl (n, h, syms, st), e, tokst)) x f =
        let (e', x') = visitPtsInExpr e x f in
            (SymTok (SymDecl (n, h, syms, st), e', tokst), x')

instance Binder (MultiSymbolDeclaration a) (ParamTypeName a) where
    onBindingsOf (MultiSymTok sn h mpm st) x f =
        {- The hint defines the bindings! Like for SymbolDeclaration. -}
        let (h', x') = visitPtsInHint h x f in
            (MultiSymTok sn h' mpm st, x')

    onScopedOf (MultiSymTok sn h mpm st) x f =
        let (mpm', x') = visitPtsInMultiPm mpm x f in
            (MultiSymTok sn h mpm' st, x')

instance Binder (SDUnion a) (ParamTypeName a) where
    onBindingsOf (SD sd) x f =
        let (sd', x') = onBindingsOf sd x f in
            (SD sd', x')
    onBindingsOf (MSD msd) x f =
        let (msd', x') = onBindingsOf msd x f in
            (MSD msd', x')

    onScopedOf (SD sd) x f =
        let (sd', x') = onScopedOf sd x f in
            (SD sd', x')
    onScopedOf (MSD msd) x f =
        let (msd', x') = onScopedOf msd x f in
            (MSD msd', x')

instance Binder (Signature a) (ParamTypeName a) where
    onBindingsOf (SigTok (sn, ty, st)) x f =
        let (ty', x') = visitPtsInUnConInType ty x f in
            (SigTok (sn, ty', st), x')

    onScopedOf (SigTok (sn, ty, st)) x f =
        let (ty', x') = visitPtsInContsInType ty x f in
            (SigTok (sn, ty', st), x')

{- Monad to simplify visit functions. The read-only environment is a function for tokens visit. It is read-only because
it would be unpleasant if the visit function changes while evaluating an action. -}
type TokenOpT tok s st m a = RWST (st -> tok s -> m (st, tok s)) () st m a

getVisit :: Monad m => TokenOpT tok s st m (st -> tok s -> m (st, tok s))
getVisit = ask

getState :: Monad m => TokenOpT tok s st m st
getState = get

tokenOpVisit :: Monad m => tok s -> TokenOpT tok s st m (tok s)
tokenOpVisit tok = do
    f <- getVisit
    x <- getState
    (x', tok') <- lift $ f x tok
    put x'
    return tok'

runTokenVisit :: Monad m => st -> (st -> tok s -> m (st, tok s)) -> TokenOpT tok s st m a -> m (st, a)
runTokenVisit st f op = do
    (x, st', _) <- runRWST op f st
    return (st', x)

runTokenVisitWith
    :: Monad m
    => (d s -> TokenOpT tok s a m (d s))
    -> d s
    -> a
    -> (a -> tok s -> m (a, tok s))
    -> m (a, d s)
runTokenVisitWith op decl st f = runTokenVisit st f $ op decl

evalTokenVisitWith
    :: Monad m
    => (d s -> TokenOpT tok s a m (d s))
    -> d s
    -> a
    -> (a -> tok s -> m a)
    -> m a
evalTokenVisitWith op decl st f = do
    (st', _) <- runTokenVisitWith op decl st f'
    return st'
    where
        f' y tok = do
            y' <- f y tok
            return (y', tok)

execTokenVisitWith
    :: Monad m
    => (d s -> TokenOpT tok s () m (d s))
    -> d s
    -> (tok s -> m (tok s))
    -> m (d s)
execTokenVisitWith op decl f = do
    (_, decl') <- runTokenVisitWith op decl () f'
    return decl'
    where
        f' () tok = do
            tok' <- f tok
            return ((), tok')

type TypeOpT s st m a = TokenOpT Type s st m a
--type TypeOp s st a = TypeOpT s st Identity a
--type TypeOpRes s st err a = TypeOpT s st (Either err) a

type HintOpT s st m a = TokenOpT Hint s st m a
--type HintOp s st a = HintOpT s st Identity a
--type HintOpRes s st err a = HintOpT s st (Either err) a

{- Filters to let the client decide which tokens of a program to work with. Perhaps, there can be a
situation where not all tokens have to be "touched". Each constructor has the suffix "AOF" which stands
for "AstOpResFilters", just not to have clashing names problems. -}
data AstOpSingleFilter =
      AOFAdt
    | AOFAlias
    | AOFIntf
    | AOFInst
    | AOFSig
    | AOFSymD
    | AOFMultiSymD
    deriving Eq

data AstOpFilters =
    {- shorthand to have all filters. -}
      AOFAll
    {- shorthand to have no filter. -}
    | AOFNone
    | AOFSome [AstOpSingleFilter]
    | AOFExcept [AstOpSingleFilter]

isIncludedBy :: Declaration a -> AstOpFilters -> Bool
isIncludedBy _ AOFAll = True
isIncludedBy _ AOFNone = False
isIncludedBy (ADT _) (AOFSome fls) = AOFAdt `elem` fls
isIncludedBy (ADT _) (AOFExcept fls) = AOFAdt `notElem` fls
isIncludedBy (AliasADT _) (AOFSome fls) = AOFAlias `elem` fls
isIncludedBy (AliasADT _) (AOFExcept fls) = AOFAlias `notElem` fls
isIncludedBy (Intf _) (AOFSome fls) = AOFIntf `elem` fls
isIncludedBy (Intf _) (AOFExcept fls) = AOFIntf `notElem` fls
isIncludedBy (Ins _) (AOFSome fls) = AOFInst `elem` fls
isIncludedBy (Ins _) (AOFExcept fls) = AOFInst `notElem` fls
isIncludedBy (Sig _) (AOFSome fls) = AOFSig `elem` fls
isIncludedBy (Sig _) (AOFExcept fls) = AOFSig `notElem` fls
isIncludedBy (Let _) (AOFSome fls) = AOFSymD `elem` fls
isIncludedBy (Let _) (AOFExcept fls) = AOFSymD `notElem` fls
isIncludedBy (LetMulti _) (AOFSome fls) = AOFMultiSymD `elem` fls
isIncludedBy (LetMulti _) (AOFExcept fls) = AOFMultiSymD `notElem` fls

isNotIncludedBy :: Declaration a -> AstOpFilters -> Bool
isNotIncludedBy decl aof = not $ isIncludedBy decl aof

filterDecls :: [Declaration a] -> AstOpFilters -> [Declaration a]
filterDecls decls aof = filter (`isIncludedBy` aof) decls

{- The right-hand element of the returned tuple is the one which has declarations that matched the filter,
viceversa for the other one. -}
splitDecls :: [Declaration a] -> AstOpFilters -> ([Declaration a], [Declaration a])
splitDecls decls aof =
    partition (`isIncludedBy` aof) decls

typeOpVisit :: Monad m => Type s -> TypeOpT s a m (Type s)
typeOpVisit = tokenOpVisit

{- Parameterization of visit functions over lists of ast tokens. `m s` is a ast token. -}
visitUntil :: Monad m => [d s] -> (d s -> TypeOpT s a m (d s)) -> TypeOpT s a m [d s]
visitUntil [] _ = return []
visitUntil (m : t) op = do
    m' <- op m
    t' <- visitUntil t op
    return $ m' : t'

withinTypes :: Monad m => [Type s] -> TypeOpT s a m [Type s]
withinTypes [] = return []
{- The Singleton case has only the "base" Type, so no visit has to be performed -}
withinTypes (Type (cs, singleton @ (Singleton _), st) : t) = do
    cs' <- mapM visitTypesInCont cs
    ty' <- typeOpVisit $ Type (cs', singleton, st)
    t' <- withinTypes t
    return $ ty' : t'
{- If the Type is not a Singleton (then it is a composite Type), then it is necessary to visit
firstly the list of Type inside, afterwards the Type itself can be visited. -}
withinTypes (Type (cs, Composite (TyComp (nrty, ts, st)), st') : t) = do
    cs' <- mapM visitTypesInCont cs
    ts' <- withinTypes $ map typeFromUnCon ts
    ty <- typeOpVisit $ Type (cs', Composite $ TyComp (nrty, map unConFromType ts', st), st')
    t' <- withinTypes t
    return $ ty : t'

{- Even a Type could have a Type(s) within, so it is necessary a function to visit a Type. It does not
perform the visit on the Type which represent the "base", namely the first Type which appears when it
is written. -}
visitTypesInType :: Monad m => Type s -> TypeOpT s a m (Type s)
visitTypesInType (Type (cs, uty @ (Singleton _), st)) = do
    cs' <- mapM visitTypesInCont cs
    return $ Type (cs', uty, st)
visitTypesInType (Type (cs, Composite (TyComp (nrty, ts, st)), st')) = do
    cs' <- mapM visitTypesInCont cs
    ts' <- withinTypes $ map typeFromUnCon ts
    return $ Type (cs', Composite $ TyComp (nrty, map unConFromType ts', st), st')

{- The right operation to visit a Type token entirely. -}
visitEntireType :: Monad m => Type s -> TypeOpT s a m (Type s)
visitEntireType ty = do
    ty' <- typeOpVisit ty
    visitTypesInType ty'

visitEntireTypes :: Monad m => [Type s] -> TypeOpT s a m [Type s]
visitEntireTypes = mapM visitEntireType

visitTypesInAdtCon :: Monad m => ADTConstructor s -> TypeOpT s a m (ADTConstructor s)
visitTypesInAdtCon (ADTCon (c, ts, st)) = do
    ts' <- visitEntireTypes $ map typeFromUnCon ts
    return $ ADTCon (c, map unConFromType ts', st)

visitTypesInAdt :: Monad m => AlgebraicDataType s -> TypeOpT s a m (AlgebraicDataType s)
visitTypesInAdt (ADTTok (a, [], st)) = return $ ADTTok (a, [], st)
visitTypesInAdt (ADTTok (a, c : t, st)) = do
    c' <- visitTypesInAdtCon c
    adt <- visitTypesInAdt (ADTTok (a, t, st))
    return $ case adt of
        ADTTok (_, t', _) -> ADTTok (a, c' : t', st)

visitTypesInAlias :: Monad m => AliasAlgebraicDataType s -> TypeOpT s a m (AliasAlgebraicDataType s)
visitTypesInAlias (AliasTok (d, ty, st)) = do
    ty' <- typeOpVisit $ typeFromUnCon ty
    ty'' <- visitTypesInType ty'
    return $ AliasTok (d, unConFromType ty'', st)

visitTypesInHint :: Monad m => Hint s -> TypeOpT s a m (Hint s)
visitTypesInHint (Hint (Nothing, st)) = return $ Hint (Nothing, st)
visitTypesInHint (Hint (Just ty, st)) = do
    ty' <- typeOpVisit ty
    ty'' <- visitTypesInType ty'
    return $ Hint (Just ty'', st)

visitTypesInCont :: Monad m => Constraint s -> TypeOpT s a m (Constraint s)
visitTypesInCont (Cont (pName, ts, st)) = do
    ts' <- visitEntireTypes $ map typeFromUnCon ts
    return $ Cont (pName, map unConFromType ts', st)

visitTypesInMatchExpr :: Monad m => MatchingExpression s -> TypeOpT s a m (MatchingExpression s)
visitTypesInMatchExpr (MatchExpr me st) = do
    case me of
        MADTApp (ADTAppMExpr (cn, ms, mst)) -> do
            { ms' <- visitUntil ms visitTypesInMatchExpr
            ; return $ MatchExpr (MADTApp $ ADTAppMExpr (cn, ms', mst)) st
            }
        MDefault _ -> return $ MatchExpr me st
        MLit _ -> return $ MatchExpr me st
        MBase _ -> return $ MatchExpr me st
        MADTBase _ -> return $ MatchExpr me st

visitTypesInMatchCase :: Monad m => MatchCase s -> TypeOpT s a m (MatchCase s)
visitTypesInMatchCase (Case (m, e, st)) = do
    m' <- visitTypesInMatchExpr m
    e' <- visitTypesInExpr e
    return $ Case (m', e', st)

visitTypesInMultiCase :: Monad m => MultiMatchCase s -> TypeOpT s a m (MultiMatchCase s)
visitTypesInMultiCase (MultiCase ms e st) = do
    ms' <- mapM visitTypesInMatchExpr ms
    e' <- visitTypesInExpr e
    return $ MultiCase ms' e' st

visitTypesInMultiPM :: Monad m => MultiPatternMatch s -> TypeOpT s a m (MultiPatternMatch s)
visitTypesInMultiPM (MultiPattMatch cs st) = do
    cs' <- mapM visitTypesInMultiCase cs
    return $ MultiPattMatch cs' st

visitTypesInExpr :: Monad m => Expression s -> TypeOpT s a m (Expression s)
visitTypesInExpr (Expr (e, h, st)) = do
    h' <- visitTypesInHint h
    (case e of
        App (AppExpr (a, as, est)) ->
            do { a' <- visitTypesInExpr a
               ; as' <- visitUntil as visitTypesInExpr
               ; return $ Expr (App $ AppExpr (a', as', est), h', st)
               }
        Base _ -> return $ Expr (e, h', st)
        ADTBase _ -> return $ Expr (e, h', st)
        Match (PattMatch (etm, cs, est)) ->
            do { etm' <- visitTypesInExpr etm
               ; cs' <- visitUntil cs visitTypesInMatchCase
               ; return $ Expr (Match $ PattMatch (etm', cs', est), h', st)
               }
        Lam (Lambda (syms, ea, est)) ->
            do { ea' <- visitTypesInExpr ea
               ; return $ Expr (Lam $ Lambda (syms, ea', est), h', st)
               }
        MultiLam (MultiLambda mpm est) ->
            do { mpm' <- visitTypesInMultiPM mpm
               ; return $ Expr (MultiLam $ MultiLambda mpm' est, h', st)
               }
        Bound (BoundExpr (symd, ea, est)) ->
            do { ea' <- visitTypesInExpr ea
               ; symd' <- visitTypesInSymDecl symd
               ; return $ Expr (Bound $ BoundExpr (symd', ea', est), h', st)
               }
        MultiBound (MultiBoundExpr msymd ea est) ->
            do { ea' <- visitTypesInExpr ea
               ; msymd' <- visitTypesInMultiSymDecl msymd
               ; return $ Expr (MultiBound $ MultiBoundExpr msymd' ea' est, h', st)
               }
        Lit _ -> return $ Expr (e, h', st))

visitTypesInSymDecl :: Monad m => SymbolDeclaration s -> TypeOpT s a m (SymbolDeclaration s)
visitTypesInSymDecl (SymTok (SymDecl (sn, h, syms, sdst), e, st)) = do
    h' <- visitTypesInHint h
    e' <- visitTypesInExpr e
    return $ SymTok (SymDecl (sn, h', syms, sdst), e', st)

visitTypesInMultiSymDecl :: Monad m => MultiSymbolDeclaration s -> TypeOpT s a m (MultiSymbolDeclaration s)
visitTypesInMultiSymDecl (MultiSymTok sn h mpm st) = do
    h' <- visitTypesInHint h
    mpm' <- visitTypesInMultiPM mpm
    return $ MultiSymTok sn h' mpm' st

visitTypesInSD :: Monad m => SDUnion s -> TypeOpT s a m (SDUnion s)
visitTypesInSD (SD sd) = do
    sd' <- visitTypesInSymDecl sd
    return $ SD sd'
visitTypesInSD (MSD msd) = do
    msd' <- visitTypesInMultiSymDecl msd
    return $ MSD msd'

visitTypesInInst :: Monad m => Instance s -> TypeOpT s a m (Instance s)
visitTypesInInst (InstTok (i, cs, ts, syms, st)) = do
    cs' <- mapM visitTypesInCont cs
    ts' <- visitEntireTypes $ map typeFromUnCon ts
    syms' <- visitUntil syms visitTypesInSD
    return $ InstTok (i, cs', map unConFromType ts', syms', st)

visitTypesInProp :: Monad m => Interface s -> TypeOpT s a m (Interface s)
visitTypesInProp (IntfTok (i, sigs, st)) = do
    i' <- visitTypesInIntfDecl i
    sigs' <- visitUntil sigs visitTypesInSig
    return $ IntfTok (i', sigs', st)
    where
        visitTypesInIntfDecl :: Monad m => IntfDeclare s -> TypeOpT s a m (IntfDeclare s)
        visitTypesInIntfDecl (IntfDecl (pName, cs, pts, dst)) = do
            cs' <- mapM visitTypesInCont cs
            return $ IntfDecl (pName, cs', pts, dst)

visitTypesInSig :: Monad m => Signature s -> TypeOpT s a m (Signature s)
visitTypesInSig (SigTok (s, ty, st)) = do
    ty' <- typeOpVisit ty
    ty'' <- visitTypesInType ty'
    return $ SigTok (s, ty'', st)

visitTypesInDecl :: Monad m => Declaration s -> AstOpFilters -> TypeOpT s a m (Declaration s)
visitTypesInDecl decl aof =
    if decl `isIncludedBy` aof
    then
        case decl of
            ADT adt -> do
                adt' <- visitTypesInAdt adt
                return $ ADT adt'
            AliasADT alias -> do
                alias' <- visitTypesInAlias alias
                return $ AliasADT alias'
            Intf prop -> do
                prop' <- visitTypesInProp prop
                return $ Intf prop'
            Ins inst -> do
                inst' <- visitTypesInInst inst
                return $ Ins inst'
            Sig sig -> do
                sig' <- visitTypesInSig sig
                return $ Sig sig'
            Let symD -> do
                symD' <- visitTypesInSymDecl symD
                return $ Let symD'
            LetMulti symD -> do
                symD' <- visitTypesInMultiSymDecl symD
                return $ LetMulti symD'
    else
        return decl

runTypeVisitWith
    :: Monad m
    => (d s -> TypeOpT s a m (d s))
    -> d s
    -> a
    -> (a -> Type s -> m (a, Type s))
    -> m (a, d s)
runTypeVisitWith = runTokenVisitWith

evalTypeVisitWith
    :: Monad m
    => (d s -> TypeOpT s a m (d s))
    -> d s
    -> a
    -> (a -> Type s -> m a)
    -> m a
evalTypeVisitWith = evalTokenVisitWith

execTypeVisitWith
    :: Monad m
    => (d s -> TypeOpT s () m (d s))
    -> d s
    -> (Type s -> m (Type s))
    -> m (d s)
execTypeVisitWith = execTokenVisitWith

astOpTypeVisit
    :: Monad m
    => (d s -> TypeOpT s a m (d s))
    -> d s
    -> a
    -> (a -> Type s -> m (a, Type s))
    -> AstOpT s m (a, d s)
astOpTypeVisit op decl x tyVisit =
    lift $ runTypeVisitWith op decl x tyVisit

{- Conversion of UnConType updating function. -}
unConTransUpdate :: (UnConType s -> Either err (UnConType s)) -> (Type s -> Either err (Type s))
unConTransUpdate f ty =
    case f $ unConFromType ty of
        Left err -> Left err
        Right uty -> Right $ typeFromUnCon uty

unConTransLookup :: (a -> UnConType s -> b) -> (a -> Type s -> b)
unConTransLookup f x = f x . unConFromType

{- Low-level ast visiting function.
NB: it visits declarations starting from the last one. -}
visitTypes :: Monad m => a -> (a -> Type s -> m (a, Type s)) -> AstOpFilters -> AstOpT s m a
visitTypes x f aof = do
    p <- get
    let decls = declarationsFrom p
    (x', decls') <- fromLastToFstM decls visit `accumulatingIn` (x, [])
    replaceProg decls'
    return x'
    where
        visit decl (y, decls) = do
            (y', decl') <- astOpTypeVisit (`visitTypesInDecl` aof) decl y f
            return (y', decl' : decls)

----------------- Ast operations on types -----------------

{- It offers an operation to change values of type Type. This is very useful to avoid direct pattern
matching on a Program, which would break the abstract data type and would bring boiler-plate code.
The callback provides a way to set the value for the error case. -}
updateTypes :: (Type s -> Either err (Type s)) -> AstOpFilters -> AstOpRes s err ()
updateTypes f =
    visitTypes () builtF
    where
    {- builtF is only an adjustment of `f`, it does the same stuff. -}
    builtF _ ty =
        case f ty of
            Left err -> Left err
            Right ty' -> Right ((), ty')

safeUpdateTypes :: (Type s -> Type s) -> AstOpFilters -> AstOp s ()
safeUpdateTypes f =
    visitTypes () builtF
    where
        builtF _ ty = do
            let ty' = f ty
            return ((), ty')

{- Same of updateTypes, but it acts on UnConType. -}
updateUnCons :: (UnConType s -> Either err (UnConType s)) -> AstOpFilters -> AstOpRes s err ()
updateUnCons f = updateTypes $ unConTransUpdate f

{- `updateAllTypes x f` is a shorthand for `updateTypes x f AOFAll` -}
updateAllTypes :: (Type s -> Either err (Type s)) -> AstOpRes s err ()
updateAllTypes f = updateTypes f AOFAll

updateAllUnCons :: (UnConType s -> Either err (UnConType s)) -> AstOpRes s err ()
updateAllUnCons f = updateUnCons f AOFAll

{- It offers an operation to "read" all values of type Type. Like updateTypes, this is useful to avoid
pattern matching on a Program. -}
lookupTypes :: a -> (a -> Type s -> Either err a) -> AstOpFilters -> AstOpRes s err a
lookupTypes x f =
    visitTypes x builtF
    where
    {- Adjustment of `f` -}
    builtF y ty =
        case f y ty of
            Left err -> Left err
            Right y' -> Right (y', ty)

lookupUnCons :: a -> (a -> UnConType s -> Either err a) -> AstOpFilters -> AstOpRes s err a
lookupUnCons x f filters = lookupTypes x <| unConTransLookup f <| filters

lookupAllTypes :: a -> (a -> Type s -> Either err a) -> AstOpRes s err a
lookupAllTypes x f = lookupTypes x f AOFAll

lookupAllUnCons :: a -> (a -> UnConType s -> Either err a) -> AstOpRes s err a
lookupAllUnCons x f = lookupUnCons x f AOFAll

{- Variant of `lookupTypes` which takes a "safe" callback, namely a callback which does not return an error.
NB: the returned ast operation is granted not to fail. -}
safeLookupTypes :: a -> (a -> Type s -> a) -> AstOpFilters -> AstOp s a
safeLookupTypes x f =
    visitTypes x builtF
    where
        builtF y ty = do
            let y' = f y ty
            return (y', ty)

safeLookupAllTypes :: a -> (a -> Type s -> a) -> AstOp s a
safeLookupAllTypes x f = safeLookupTypes x f AOFAll

-------------------- Ast operations on declarations --------------------

{- Generic ast-operation to read a declaration. -}
lookupDecl :: Monad m => (a -> Declaration s -> m a) -> a -> Declaration s -> AstOpT s m a
lookupDecl f x decl = lift $ f x decl

{- Generic ast-operation to read-and-update a declaration. -}
setDecl
    :: Monad m
    => (a -> Declaration s -> m (a, Declaration s))
    -> a
    -> Declaration s
    -> AstOpT s m (a, Declaration s)
setDecl f x decl = lift $ f x decl

{- Generic ast-operation to update a declaration. -}
updateDecl :: Monad m => (Declaration s -> m (Declaration s)) -> Declaration s -> AstOpT s m (Declaration s)
updateDecl f = lift . f

{- Generic ast-operation to read declarations in a program. -}
lookupDecls :: Monad m => a -> (a -> Declaration s -> m a) -> AstOpT s m a
lookupDecls x f = do
    decls <- getDecls
    forAllM decls doLookup x
    where
        doLookup = lookupDecl f

{- Generic ast-operation to read-and-update declarations in a program. -}
setDecls :: Monad m => a -> (a -> Declaration s -> m (a, Declaration s)) -> AstOpT s m a
setDecls x f = do
    decls <- getDecls
    (x', decls') <- forAllM decls doVisit `accumulatingIn` (x, [])
    replaceProg $ reverse decls'
    return x'
    where
        accumulatingIn = ($)

        doVisit (y, decls) decl = do
            (y', decl') <- lift $ f y decl
            return (y', decl' : decls)

{- Generic ast-operation to update declarations in a program. -}
updateDecls :: Monad m => (Declaration s -> m (Declaration s)) -> AstOpT s m ()
updateDecls f = do
    decls <- getDecls
    decls' <- mapM doUpdate decls
    replaceProg decls'
    where
        doUpdate = updateDecl f

getGenSymDecls :: Monad m => AstOpT s m [SDUnion s]
getGenSymDecls = do
    sds <- lookupDecls [] getGenSD
    return $ reverse sds
    where
        getGenSD accum (Let sd) = return $ SD sd : accum
        getGenSD accum (LetMulti msd) = return $ MSD msd : accum
        getGenSD accum _ = return accum

{- Operation to "read" all occurrences of declaration of adt. -}
lookupAdt :: a -> (a -> AlgebraicDataType s -> Either err a) -> AstOpRes s err a
lookupAdt x f =
    lookupDecls x builtF
    where
        builtF y (ADT adt) = f y adt
        builtF y _ = Right y

{- Safe version of `lookupAdt`, namely it is granted not to fail. -}
safeLookupAdt :: a -> (a -> AlgebraicDataType s -> a) -> AstOp s a
safeLookupAdt x f =
    lookupDecls x builtF
    where
        builtF y (ADT adt) = pure $ f y adt
        builtF y _ = pure y

updateAdt :: (AlgebraicDataType s -> Either err (AlgebraicDataType s)) -> AstOpRes s err ()
updateAdt f =
    updateDecls builtF
    where
        builtF (ADT adt) =
            case f adt of
                Left err -> Left err
                Right adt' -> Right $ ADT adt'
        builtF decl = Right decl

{- Instead of write-only functions (like `update<token>`), it updates a token (in this case an adt)
and returns also a user-defined value. Thus, it can be seen as a read-and-write operation on ast. -}
setAdt :: a -> (a -> AlgebraicDataType s -> Either err (a, AlgebraicDataType s)) -> AstOpRes s err a
setAdt x f =
    setDecls x builtF
    where
        builtF y (ADT adt) =
            case f y adt of
                Left err -> Left err
                Right (y', adt') -> Right (y', ADT adt')
        builtF y decl = Right (y, decl)

safeSetAdt :: a -> (a -> AlgebraicDataType s -> (a, AlgebraicDataType s)) -> AstOp s a
safeSetAdt x f =
    setDecls x builtF
    where
        builtF y (ADT adt) = do
            let (y', adt') = f y adt
            return (y', ADT adt')
        builtF y decl = pure (y, decl)

{- Operation to "read" all occurrences of declaration of alias. -}
lookupAlias :: a -> (a -> AliasAlgebraicDataType s -> Either err a) -> AstOpRes s err a
lookupAlias x f =
    lookupDecls x builtF
    where
        builtF y (AliasADT alias) = f y alias
        builtF y _ = Right y

{- Safe version of `lookupAlias`, namely it is granted not to fail. -}
safeLookupAlias :: a -> (a -> AliasAlgebraicDataType s -> a) -> AstOp s a
safeLookupAlias x f =
    lookupDecls x builtF
    where
        builtF y (AliasADT alias) = pure $ f y alias
        builtF y _ = pure y

removeAlias :: Monad m => AstOpT s m [AliasAlgebraicDataType s]
removeAlias = removeDeclsWith isAlias
    where
        isAlias (AliasADT alias) = Just alias
        isAlias _ = Nothing

lookupSymDecl :: a -> (a -> SymbolDeclaration s -> Either err a) -> AstOpRes s err a
lookupSymDecl x f =
    lookupDecls x builtF
    where
        builtF y (Let symD) = f y symD
        builtF y _ = Right y

safeLookupSymDecl :: a -> (a -> SymbolDeclaration s -> a) -> AstOp s a
safeLookupSymDecl x f =
    lookupDecls x builtF
    where
        builtF y (Let symD) = pure $ f y symD
        builtF y _ = pure y

{- Operation to update all occurrences of declaration of symbols. -}
updateSymDecl :: (SymbolDeclaration s -> Either err (SymbolDeclaration s)) -> AstOpRes s err ()
updateSymDecl f =
    updateDecls builtF
    where
        builtF (Let symD) =
            case f symD of
                Left err -> Left err
                Right symD' -> Right $ Let symD'
        builtF decl = Right decl

safeUpdateSymDecl :: (SymbolDeclaration s -> SymbolDeclaration s) -> AstOp s ()
safeUpdateSymDecl f =
    updateDecls builtF
    where
        builtF (Let symD) = do
            let symD' = f symD
            return $ Let symD'
        builtF decl = pure decl

updateMultiSymDecl :: (MultiSymbolDeclaration s -> Either err (MultiSymbolDeclaration s)) -> AstOpRes s err ()
updateMultiSymDecl f =
    updateDecls builtF
    where
        builtF (LetMulti symD) =
            case f symD of
                Left err -> Left err
                Right symD' -> Right $ LetMulti symD'
        builtF decl = Right decl

safeUpdateMultiSymDecl :: (MultiSymbolDeclaration s -> MultiSymbolDeclaration s) -> AstOp s ()
safeUpdateMultiSymDecl f =
    updateDecls builtF
    where
        builtF (LetMulti symD) = do
            let symD' = f symD
            return $ LetMulti symD'
        builtF decl = pure decl

lookupGenSymDecl
    :: a
    -> (a -> SymbolDeclaration s -> Either err a)
    -> (a -> MultiSymbolDeclaration s -> Either err a)
    -> AstOpRes s err a
lookupGenSymDecl x sdf msdf =
    lookupDecls x builtF
    where
        builtF y (Let symD) = sdf y symD
        builtF y (LetMulti symD) = msdf y symD
        builtF y _ = Right y

safeLookupGenSymDecl
    :: a
    -> (a -> SymbolDeclaration s -> a)
    -> (a -> MultiSymbolDeclaration s -> a)
    -> AstOp s a
safeLookupGenSymDecl x sdf msdf =
    lookupDecls x builtF
    where
        builtF y (Let symD) = pure $ sdf y symD
        builtF y (LetMulti symD) = pure $ msdf y symD
        builtF y _ = pure y

setSymDecl :: a -> (a -> SymbolDeclaration s -> Either err (a, SymbolDeclaration s)) -> AstOpRes s err a
setSymDecl x f =
    setDecls x builtF
    where
        builtF y (Let symD) =
            case f y symD of
                Left err -> Left err
                Right (y', symD') -> Right (y', Let symD')
        builtF y decl = Right (y, decl)

safeSetSymDecl :: a -> (a -> SymbolDeclaration s -> (a, SymbolDeclaration s)) -> AstOp s a
safeSetSymDecl x f =
    setDecls x builtF
    where
        builtF y (Let symD) = do
            let (y', symD') = f y symD
            return (y', Let symD')
        builtF y decl = pure (y, decl)

setMultiSymDecl :: a -> (a -> MultiSymbolDeclaration s -> Either err (a, MultiSymbolDeclaration s)) -> AstOpRes s err a
setMultiSymDecl x f =
    setDecls x builtF
    where
        builtF y (LetMulti symD) =
            case f y symD of
                Left err -> Left err
                Right (y', symD') -> Right (y', LetMulti symD')
        builtF y decl = Right (y, decl)

safeSetMultiSymDecl :: a -> (a -> MultiSymbolDeclaration s -> (a, MultiSymbolDeclaration s)) -> AstOp s a
safeSetMultiSymDecl x f =
    setDecls x builtF
    where
        builtF y (LetMulti symD) = do
            let (y', symD') = f y symD
            return (y', LetMulti symD')
        builtF y decl = pure (y, decl)

lookupProp :: a -> (a -> Interface s -> Either err a) -> AstOpRes s err a
lookupProp x f =
    lookupDecls x builtF
    where
        builtF y (Intf prop) = f y prop
        builtF y _ = Right y

safeLookupProp :: a -> (a -> Interface s -> a) -> AstOp s a
safeLookupProp x f =
    lookupDecls x builtF
    where
        builtF y (Intf prop) = pure $ f y prop
        builtF y _ = pure y

setProp :: a -> (a -> Interface s -> Either err (a, Interface s)) -> AstOpRes s err a
setProp x f =
    setDecls x builtF
    where
        builtF y (Intf prop) =
            case f y prop of
                Left err -> Left err
                Right (y', prop') -> Right (y', Intf prop')
        builtF y decl = Right (y, decl)

safeSetProp :: a -> (a -> Interface s -> (a, Interface s)) -> AstOp s a
safeSetProp x f =
    setDecls x builtF
    where
        builtF y (Intf prop) = do
            let (y', prop') = f y prop
            return (y', Intf prop')
        builtF y decl = pure (y, decl)

lookupInst :: a -> (a -> Instance s -> Either err a) -> AstOpRes s err a
lookupInst x f =
    lookupDecls x builtF
    where
        builtF y (Ins inst) = f y inst
        builtF y _ = Right y

safeLookupInst :: a -> (a -> Instance s -> a) -> AstOp s a
safeLookupInst x f =
    lookupDecls x builtF
    where
        builtF y (Ins inst) = pure $ f y inst
        builtF y _ = pure y

setInst :: a -> (a -> Instance s -> Either err (a, Instance s)) -> AstOpRes s err a
setInst x f =
    setDecls x builtF
    where
        builtF y (Ins inst) =
            case f y inst of
                Left err -> Left err
                Right (y', inst') -> Right (y', Ins inst')
        builtF y decl = Right (y, decl)

safeSetInst :: a -> (a -> Instance s -> (a, Instance s)) -> AstOp s a
safeSetInst x f =
    setDecls x builtF
    where
        builtF y (Ins inst) = do
            let (y', inst') = f y inst
            return (y', Ins inst')
        builtF y decl = pure (y, decl)

lookupConts :: a -> (a -> Constraint s -> Either err a) -> AstOpRes s err a
lookupConts x f = do
    {- There are three different places where to search constraints:
        1) types;
        2) properties;
        3) instances;
    Anyway, they are visited with just a single parse of ast (this is done for a better performance obviously).
    -}
    lookupDecls x builtF
    where
        typeVisit y ty = visitConts y $ contsFromType ty

        builtF y decl =
            {- The first part of the tuple is the evaluation of all the types (the constraints which are searched in
            the types). The second part of the tuple is necessary to select properties and instances. -}
            case (evalTypeVisitWith visitContsInDecl decl y typeVisit, decl) of
                (Right y', Intf prop) -> visitConts y' $ contsFromIntf prop
                (Right y', Ins inst) -> visitConts y' $ contsFromInst inst
                (res, _) -> res

        visitContsInDecl decl = visitTypesInDecl decl AOFAll

        visitConts y = foldl' visitCont $ Right y

        visitCont err @ (Left _) _ = err
        visitCont (Right y) c = f y c

hintOpVisit :: Monad m => Hint s -> HintOpT s a m (Hint s)
hintOpVisit = tokenOpVisit

visitHintsInHeadSymDecl :: Monad m => SymbolDeclaration s -> HintOpT s a m (SymbolDeclaration s)
visitHintsInHeadSymDecl (SymTok (SymDecl (n, h, syms, sst), e, st)) = do
    h' <- hintOpVisit h
    return $ SymTok (SymDecl (n, h', syms, sst), e, st)

visitHintsInHeadMultiSymDecl :: Monad m => MultiSymbolDeclaration s -> HintOpT s a m (MultiSymbolDeclaration s)
visitHintsInHeadMultiSymDecl (MultiSymTok sn h mpm st) = do
    h' <- hintOpVisit h
    return $ MultiSymTok sn h' mpm st

runHintVisitWith
    :: Monad m
    => (d s -> HintOpT s a m (d s))
    -> d s
    -> a
    -> (a -> Hint s -> m (a, Hint s))
    -> m (a, d s)
runHintVisitWith = runTokenVisitWith

evalHintVisitWith
    :: Monad m
    => (d s -> HintOpT s a m (d s))
    -> d s
    -> a
    -> (a -> Hint s -> m a)
    -> m a
evalHintVisitWith = evalTokenVisitWith

execHintVisitWith
    :: Monad m
    => (d s -> HintOpT s () m (d s))
    -> d s
    -> (Hint s -> m (Hint s))
    -> m (d s)
execHintVisitWith = execTokenVisitWith

{- It visits hints but uniquely in symbol declarations, in particular only the hint of the variable
which is being defined. -}
setHintsInHeadSymDecl :: a -> (a -> SymbolName s -> Hint s -> Either err (a, Hint s)) -> AstOpRes s err a
setHintsInHeadSymDecl x f =
    setSymDecl x builtF
    where
        builtF y symD =
            let symName = symNameFrom symD in
                runHintVisitWith visitHintsInHeadSymDecl symD y $ f' symName

        f' sn y = f y sn

setHintsInHeadMultiSymDecl :: a -> (a -> SymbolName s -> Hint s -> Either err (a, Hint s)) -> AstOpRes s err a
setHintsInHeadMultiSymDecl x f =
    setMultiSymDecl x builtF
    where
        builtF y symD =
            let symName = symNameFromMultiSymDecl symD in
                runHintVisitWith visitHintsInHeadMultiSymDecl symD y $ f' symName

        f' sn y = f y sn

{- It visits and updates hints (through the callback) uniquely in symbol declarations, in particular
only the hint of the variable which is being defined. -}
updateHintsInHeadSymDecl :: (SymbolName s -> Hint s -> Either err (Hint s)) -> AstOpRes s err ()
updateHintsInHeadSymDecl f =
    updateSymDecl builtF
    where
        builtF symD =
            let symName = symNameFrom symD in
                execHintVisitWith visitHintsInHeadSymDecl symD $ f symName

updateHintsInHeadMultiSymDecl :: (SymbolName s -> Hint s -> Either err (Hint s)) -> AstOpRes s err ()
updateHintsInHeadMultiSymDecl f =
    updateMultiSymDecl builtF
    where
        builtF symD =
            let symName = symNameFromMultiSymDecl symD in
                execHintVisitWith visitHintsInHeadMultiSymDecl symD $ f symName

{- Safe version of updateHintsInHeadSymDecl. -}
safeUpdateHintsInHeadSymDecl :: (SymbolName s -> Hint s -> Hint s) -> AstOp s ()
safeUpdateHintsInHeadSymDecl f =
    safeUpdateSymDecl builtF
    where
        builtF symD =
            let symName = symNameFrom symD in
                runIdentity . execHintVisitWith visitHintsInHeadSymDecl symD $ f' symName

        f' sn h = pure $ f sn h

safeUpdateHintsInHeadMultiSymDecl :: (SymbolName s -> Hint s -> Hint s) -> AstOp s ()
safeUpdateHintsInHeadMultiSymDecl f =
    safeUpdateMultiSymDecl builtF
    where
        builtF symD =
            let symName = symNameFromMultiSymDecl symD in
                runIdentity . execHintVisitWith visitHintsInHeadMultiSymDecl symD $ f' symName

        f' sn h = pure $ f sn h

{- It visits and "reads" hints (through the callback) uniquely in symbol declarations, in particular
only the hint of the variable which is being defined. -}
lookupHintsInHeadSymDecl :: a -> (a -> SymbolName s -> Hint s -> Either err a) -> AstOpRes s err a
lookupHintsInHeadSymDecl x f =
    lookupSymDecl x builtF
    where
        builtF y symD =
            let symName = symNameFrom symD in
                evalHintVisitWith visitHintsInHeadSymDecl symD y $ f' symName

        f' sn y = f y sn

{- Safe version of lookupHintsInHeadSymDecl. -}
safeLookupHintsInHeadSymDecl :: a -> (a -> SymbolName s -> Hint s -> a) -> AstOp s a
safeLookupHintsInHeadSymDecl x f =
    safeLookupSymDecl x builtF
    where
        builtF y symD =
            let symName = symNameFrom symD in
                runIdentity . evalHintVisitWith visitHintsInHeadSymDecl symD y $ f' symName

        f' sn y h = pure $ f y sn h

type VisitCallback a x = x -> [MatchingExpression a] -> Expression a -> (Expression a, x)
type Visit a x = x -> [MatchingExpression a] -> Expression a -> VisitCallback a x -> (Expression a, x)

visitExprCase :: VisitCallback a x -> Visit a x -> x -> MatchCase a -> MatchCase a
visitExprCase f visit x (Case (me, e, st)) =
    let (e1, _) = visit x [me] e f in
        Case (me, e1, st)

visitPureExpr
    :: VisitCallback a x
    -> Visit a x
    -> x
    -> Expression a
    -> Expression a
visitPureExpr f visit x e =
    let (e1, _) = visit x [] e f in
        e1

visitExprMultiCase
    :: VisitCallback a x
    -> Visit a x
    -> x
    -> MultiMatchCase a
    -> MultiMatchCase a
visitExprMultiCase f visit x (MultiCase ms e st) =
    let (e1, _) = visit x ms e f in
        MultiCase ms e1 st

visitExprsMultiPM
    :: x
    -> MultiPatternMatch a
    -> VisitCallback a x
    -> Visit a x
    -> (MultiPatternMatch a, x)
visitExprsMultiPM x (MultiPattMatch mcs st) f visit =
    let mcs1 = map (visitExprMultiCase f visit x) mcs in
        (MultiPattMatch mcs1 st, x)

visitExpr'
    :: x
    -> Expression a
    -> VisitCallback a x
    -> Visit a x
    -> (Expression a, x)
visitExpr' x e @ (Expr (Base _, _, _)) _ _ = (e, x)
visitExpr' x e @ (Expr (Lit _, _, _)) _ _ = (e, x)
visitExpr' x e @ (Expr (ADTBase _, _, _)) _ _ = (e, x)
visitExpr' x (Expr (App (AppExpr (e, es, ast)), h, st)) f visit =
    let (e1, _) = visit x [] e f in
    let es1 = map (visitPureExpr f visit x) es in
        (Expr (App (AppExpr (e1, es1, ast)), h, st), x)
visitExpr' x (Expr (Match (PattMatch (e, cs, pmst)), h, st)) f visit =
    let (e1, _) = visit x [] e f in
    let cs1 = map (visitExprCase f visit x) cs in
        (Expr (Match (PattMatch (e1, cs1, pmst)), h, st), x)
visitExpr' x (Expr (Lam (Lambda (syms, e, lst)), h, st)) f visit =
    let (e1, _) = visit x [] e f in
        (Expr (Lam (Lambda (syms, e1, lst)), h, st), x)
visitExpr' x (Expr (MultiLam (MultiLambda mpm lst), h, st)) f visit =
    let (mpm1, x1) = visitExprsMultiPM x mpm f visit in
        (Expr (MultiLam (MultiLambda mpm1 lst), h, st), x1)
visitExpr' x (Expr (Bound (BoundExpr (sd, e, bst)), h, st)) f visit =
    let (sd1, x1) = visitExprsInSd x sd f in
    let (e1, x2) = visit x1 [] e f in
        (Expr (Bound (BoundExpr (sd1, e1, bst)), h, st), x2)
visitExpr' x (Expr (MultiBound (MultiBoundExpr msd e bst), h, st)) f visit =
    let (msd1, x1) = visitExprsInMsd x msd f in
    let (e1, x2) = visit x1 [] e f in
        (Expr (MultiBound (MultiBoundExpr msd1 e1 bst), h, st), x2)

visitExprsInExpr' :: Visit a x
visitExprsInExpr' x ms e f =
    let (e1, x1) = f x ms e in
        visitExpr' x1 e1 f visitExprsInExpr'

visitExprsInExpr :: x -> Expression a -> VisitCallback a x -> (Expression a, x)
visitExprsInExpr x = visitExprsInExpr' x []

visitExprsInSd :: x -> SymbolDeclaration a -> VisitCallback a x -> (SymbolDeclaration a, x)
visitExprsInSd x (SymTok (symDecl, e, st)) f =
    let (e1, x1) = visitExprsInExpr x e f in
        (SymTok (symDecl, e1, st), x1)

visitExprsInMsd
    :: x
    -> MultiSymbolDeclaration a
    -> VisitCallback a x
    -> (MultiSymbolDeclaration a, x)
visitExprsInMsd x (MultiSymTok sn h mpm st) f =
    let (mpm1, x1) = visitExprsMultiPM x mpm f visitExprsInExpr' in
        (MultiSymTok sn h mpm1 st, x1)

-- Helpers (not exported)

typeOfDecl
    :: Declaration a
    -> ( Maybe (AlgebraicDataType a)
       , Maybe (AliasAlgebraicDataType a)
       , Maybe (Interface a)
       , Maybe (Instance a)
       , Maybe (Signature a)
       , Maybe (SymbolDeclaration a)
       , Maybe (MultiSymbolDeclaration a)
       )
typeOfDecl (ADT d) = (Just d, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
typeOfDecl (AliasADT d) = (Nothing, Just d, Nothing, Nothing, Nothing, Nothing, Nothing)
typeOfDecl (Intf d) = (Nothing, Nothing, Just d, Nothing, Nothing, Nothing, Nothing)
typeOfDecl (Ins d) = (Nothing, Nothing, Nothing, Just d, Nothing, Nothing, Nothing)
typeOfDecl (Sig d) = (Nothing, Nothing, Nothing, Nothing, Just d, Nothing, Nothing)
typeOfDecl (Let d) = (Nothing, Nothing, Nothing, Nothing, Nothing, Just d, Nothing)
typeOfDecl (LetMulti d) = (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just d)

-- Construction

buildProgram :: [Declaration a] -> Program a
buildProgram = Program

buildDeclarationAdt :: AlgebraicDataType a -> Declaration a
buildDeclarationAdt = ADT

buildSymbolDeclaration
    :: SymbolName a
    -> Hint a
    -> [SymbolName a]
    --Head state
    -> a
    -> Expression a
    --The entire token state
    -> a
    -> SymbolDeclaration a
buildSymbolDeclaration sn h syms hst e st = SymTok (SymDecl (sn, h, syms, hst), e, st)

buildMultiSymbolDeclaration
    :: SymbolName a
    -> Hint a
    -> MultiPatternMatch a
    --The entire token state
    -> a
    -> MultiSymbolDeclaration a
buildMultiSymbolDeclaration = MultiSymTok

buildRealBaseUnCon :: ADTName a -> UnConType a
buildRealBaseUnCon adt = Singleton $ Real adt

buildParamBaseUnCon :: ParamTypeName a -> UnConType a
buildParamBaseUnCon par = Singleton $ Param par

buildRealCompUnCon :: ADTName a -> [UnConType a] -> a -> UnConType a
buildRealCompUnCon adt tys st = Composite $ TyComp (Real adt, tys, st)

buildParamCompUnCon :: ParamTypeName a -> [UnConType a] -> a -> UnConType a
buildParamCompUnCon par tys st = Composite $ TyComp (Param par, tys, st)

buildType :: [Constraint a] -> UnConType a -> a -> Type a
buildType cs ty st = Type (cs, ty, st)

buildPtyName :: TyVarRep -> a -> ParamTypeName a
buildPtyName s st = PtyName (tokenRepToStr s, st)

buildSymbolName :: SymbolRep -> a -> SymbolName a
buildSymbolName s st = SymName (tokenRepToStr s, st)

buildGenTypeName :: ADTName a -> GenTypeName a
buildGenTypeName = Left

buildGenTypeName' :: ParamTypeName a -> GenTypeName a
buildGenTypeName' = Right

buildGenFromBase :: UnConType a -> GenTypeName a
buildGenFromBase (Singleton (Param pty)) = buildGenTypeName' pty
buildGenFromBase (Singleton (Real rty)) = buildGenTypeName rty
buildGenFromBase (Composite (TyComp (Param pty, _, _))) = buildGenTypeName' pty
buildGenFromBase (Composite (TyComp (Real rty, _, _))) = buildGenTypeName rty

buildAdt :: ADTDeclare a -> [ADTConstructor a] -> a -> AlgebraicDataType a
buildAdt d cs st = ADTTok (d, cs, st)

buildAlias :: ADTDeclare a -> UnConType a -> a -> AliasAlgebraicDataType a
buildAlias d t st = AliasTok (d, t, st)

buildSig :: SymbolName a -> Type a -> a -> Signature a
buildSig sName ty st = SigTok (sName, ty, st)

{- NB: The returned hint gets the same state of the type. -}
buildHint :: Type a -> Hint a
buildHint ty = Hint (Just ty, stateOf ty)

buildHint' :: Type a -> a -> Hint a
buildHint' ty st = Hint (Just ty, st)

buildEmptyHint :: a -> Hint a
buildEmptyHint st = Hint (Nothing, st)

buildCont :: IntfName a -> [UnConType a] -> a -> Constraint a
buildCont i ts st = Cont (i, ts, st)

buildHead :: Instance a -> Constraint a
buildHead inst = buildCont (intfNameFromInst inst) (unConsFromInst inst) $ stateOf inst

buildHead' :: Interface a -> Constraint a
buildHead' (IntfTok (IntfDecl (name, _, pts, hst), _, _)) =
    Cont (name, map typeFromPty pts, hst)
    where
        typeFromPty = Singleton . Param

buildBoundExpression :: SDUnion a -> Expression a -> Hint a -> a -> Expression a
buildBoundExpression sdu e h st =
    let uae =
         case sdu of
            MSD msd -> MultiBound (MultiBoundExpr msd e st)
            SD sd -> Bound (BoundExpr (sd, e, st))
        in
        Expr (uae, h, st)

{- It builds a bound expression with the symbol in the symbol declaration as expression, something
like this:

    let f x y z = x + y - z in f

This is just an example in pseudo-code to highlight the fact of `f` occurring as expression.
-}
buildNaiveBoundUAExpr :: SymbolDeclaration a -> Expression a -> a -> UnAltExpression a
buildNaiveBoundUAExpr sd e bst = Bound $ BoundExpr (sd, e, bst)

buildNaiveMultiBoundUAExpr :: MultiSymbolDeclaration a -> Expression a -> a -> UnAltExpression a
buildNaiveMultiBoundUAExpr msd e bst = MultiBound $ MultiBoundExpr msd e bst

buildSymbolExpr :: SymbolName a -> Hint a -> a -> Expression a
buildSymbolExpr sn h st = Expr (Base sn, h, st)

{- TODO: useless
buildContMatchExpr :: Constraint a -> a -> MatchingExpression a
buildContMatchExpr c = MatchExpr (MConstraint c)
-}

buildMultiMatchCase :: [MatchingExpression a] -> Expression a -> a -> MultiMatchCase a
buildMultiMatchCase = MultiCase

buildMultiPattMatch :: [MultiMatchCase a] -> a -> MultiPatternMatch a
buildMultiPattMatch = MultiPattMatch

-- Update operations (usually unsafe)

updateSymbolNameInSd :: SymbolDeclaration a -> SymbolRep -> SymbolDeclaration a
updateSymbolNameInSd (SymTok (SymDecl (SymName (_, snst), h, args, dst), e, st)) symRep =
    SymTok (SymDecl (SymName (tokenRepToStr symRep, snst), h, args, dst), e, st)

updateSymbolNameInMsd :: MultiSymbolDeclaration a -> SymbolRep -> MultiSymbolDeclaration a
updateSymbolNameInMsd (MultiSymTok (SymName (_, snst)) h mpm st) symRep =
    MultiSymTok (SymName (tokenRepToStr symRep, snst)) h mpm st

updateSymbolName :: SDUnion a -> SymbolRep -> SDUnion a
updateSymbolName (SD sd) = SD . updateSymbolNameInSd sd
updateSymbolName (MSD msd) = MSD . updateSymbolNameInMsd msd

-- Mapping types

mapTypesLam
    :: [ParamTypeName a]
    -> [UnConType a]
    -> (UnConType a -> UnConType a)
mapTypesLam pts uts =
    let pairing = zip pts uts in
        transformLam $ lam pairing
    where
        lam pairing pty =
            case firstThat (\(pty', _) -> repOf pty' == repOf pty) pairing of
                Nothing -> Nothing
                Just (_, uty) -> Just uty

        {- Just turning an updating lambda into a more suitable one. -}
        transformLam
            :: (ParamTypeName a -> Maybe (UnConType a))
            -> (UnConType a -> UnConType a)
        transformLam tryChange uty =
            doOnUnCon' uty
                buildRealBaseUnCon
                (\pty -> case tryChange pty of
                    Nothing -> buildParamBaseUnCon pty
                    Just uty' -> uty'
                )
                (\rty ts st ->
                    let ts1 = map (transformLam tryChange) ts in
                        buildRealCompUnCon rty ts1 st
                )
                (\pty ts st -> case tryChange pty of
                    Nothing -> buildParamCompUnCon pty ts st
                    Just uty' ->
                        let ts1 = map (transformLam tryChange) ts in
                            doOnUnCon' uty'
                                (\rty' -> buildRealCompUnCon rty' ts1 st)
                                (\pty' -> buildParamCompUnCon pty' ts1 st)
                                {- It's worth noting that in the last two cases, the state is lost (look at
                                the _ pattern)! doOnUnCon could be used, but it does not just to highlight
                                the state lost. -}
                                (\rty' ts' _ -> buildRealCompUnCon rty' (ts' ++ ts1) st)
                                (\pty' ts' _ -> buildParamCompUnCon pty' (ts' ++ ts1) st)
                )

mapTypesInConts
    :: (UnConType a -> UnConType a)
    -> [Constraint a]
    -> [Constraint a]
mapTypesInConts change = map mapTypesInCont
    where
        mapTypesInCont (Cont (i, uts, st)) =
            let uts' = map change uts in
                Cont (i, uts', st)

-- Operations on the tree

realTypes :: [UnConType a] -> [ADTName a]
realTypes [] = []
realTypes (Singleton (Param _) : t) = realTypes t
realTypes (Singleton (Real r) : t) = r : realTypes t
realTypes (Composite (TyComp (Param _, ts, _)) : t) = realTypes (ts ++ t)
realTypes (Composite (TyComp (Real r, ts, _)) : t) = r : realTypes (ts ++ t)

realTypes' :: UnConType a -> [ADTName a]
realTypes' uty = realTypes [uty]

paramTypes :: [UnConType a] -> [ParamTypeName a]
paramTypes [] = []
paramTypes (Singleton (Param p) : t) = p : paramTypes t
paramTypes (Singleton (Real _) : t) = paramTypes t
paramTypes (Composite (TyComp (Param p, ts, _)) : t) = p : paramTypes (ts ++ t)
paramTypes (Composite (TyComp (Real _, ts, _)) : t) = paramTypes (ts ++ t)

paramTypes' :: UnConType a -> [ParamTypeName a]
paramTypes' uty = paramTypes [uty]

declarationsFrom :: Program a -> [Declaration a]
declarationsFrom (Program l) = l

adtDeclFrom :: Declaration a -> Maybe (AlgebraicDataType a)
adtDeclFrom d = case typeOfDecl d of
    (d', _, _, _, _, _, _) -> d'

aliasDeclFrom :: Declaration a -> Maybe (AliasAlgebraicDataType a)
aliasDeclFrom d = case typeOfDecl d of
    (_, d', _, _, _, _, _) -> d'

intfDeclFrom :: Declaration a -> Maybe (Interface a)
intfDeclFrom d = case typeOfDecl d of
    (_, _, d', _, _, _, _) -> d'

instDeclFrom :: Declaration a -> Maybe (Instance a)
instDeclFrom d = case typeOfDecl d of
    (_, _, _, d', _, _, _) -> d'

sigDeclFrom :: Declaration a -> Maybe (Signature a)
sigDeclFrom d = case typeOfDecl d of
    (_, _, _, _, d', _, _) -> d'

symDeclFrom :: Declaration a -> Maybe (SymbolDeclaration a)
symDeclFrom d = case typeOfDecl d of
    (_, _, _, _, _, d', _) -> d'

multiSymDeclFrom :: Declaration a -> Maybe (MultiSymbolDeclaration a)
multiSymDeclFrom d = case typeOfDecl d of
    (_, _, _, _, _, _, d') -> d'

adtNameFrom :: AlgebraicDataType a -> ADTName a
adtNameFrom (ADTTok (ADTDecl (name, _, _), _, _)) = name

adtNamesFromUnCon :: UnConType a -> [ADTName a]
adtNamesFromUnCon = realTypes'

adtNamesFromHint :: Hint a -> [ADTName a]
adtNamesFromHint (Hint (Nothing, _)) = []
adtNamesFromHint (Hint (Just t, _)) = realTypes' $ unConFromType t

adtNamesFromInst :: Instance a -> [ADTName a]
adtNamesFromInst (InstTok (_, _, ts, _, _)) = realTypes ts

adtNamesFromSig :: Signature a -> [ADTName a]
adtNamesFromSig (SigTok (_, t, _)) = realTypes' $ unConFromType t

allAdtNamesFromSig :: Signature a -> [ADTName a]
allAdtNamesFromSig sig @ (SigTok (_, ty, _)) =
    realTypes (concatMap unConsFromCont (contsFromType ty)) ++ adtNamesFromSig sig

adtNamesFromCons :: [ADTConstructor a] -> [ADTName a]
adtNamesFromCons [] = []
adtNamesFromCons (ADTCon (_, types, _) : t) = realTypes types ++ adtNamesFromCons t

adtNamesFromComp :: TypeComposite a -> [ADTName a]
adtNamesFromComp (TyComp (_, types, _)) = realTypes types

adtNamesFromCont :: Constraint a -> [ADTName a]
adtNamesFromCont (Cont (_, types, _)) = realTypes types

adtNameFromAlias :: AliasAlgebraicDataType a -> Maybe (ADTName a)
adtNameFromAlias (AliasTok (_, Singleton (Real a), _)) = Just a
adtNameFromAlias (AliasTok (_, Composite (TyComp (Real a, _, _)), _)) = Just a
adtNameFromAlias _ = Nothing

adtConNameFrom :: ADTConstructor a -> ADTConName a
adtConNameFrom (ADTCon (name, _, _)) = name

adtConsNameFrom :: AlgebraicDataType a -> [ADTConName a]
adtConsNameFrom (ADTTok (_, cons, _)) = map (\(ADTCon (name, _, _)) -> name) cons

paramTNamesFromAdt :: AlgebraicDataType a -> [ParamTypeName a]
paramTNamesFromAdt a = let cons = adtConsFrom a in
    fetchParam [] cons
        where
            fetchParam accum [] = accum
            fetchParam accum (con : t) = fetchParam (paramTNamesFromCon con ++ accum) t

boundParamTNamesFromAdt :: AlgebraicDataType a -> [ParamTypeName a]
boundParamTNamesFromAdt (ADTTok (ADTDecl (_, types, _), _, _)) = types

paramTNamesFromComp :: TypeComposite a -> [ParamTypeName a]
paramTNamesFromComp (TyComp (_, types, _)) = paramTypes types

paramTNamesFromCon :: ADTConstructor a -> [ParamTypeName a]
paramTNamesFromCon (ADTCon (_, types, _)) = paramTypes types

paramTNamesFromCont :: Constraint a -> [ParamTypeName a]
paramTNamesFromCont (Cont (_, types, _)) = paramTypes types

paramTNamesFromType :: Type a -> [ParamTypeName a]
paramTNamesFromType (Type (_, types, _)) = paramTypes' types

paramTNamesFromIntf :: Interface a -> [ParamTypeName a]
paramTNamesFromIntf (IntfTok (IntfDecl (_, _, ps, _), _, _)) = ps

paramTNamesFromInst :: Instance a -> [ParamTypeName a]
paramTNamesFromInst (InstTok (_, _, ts, _, _)) = paramTypes ts

paramTNameFromAlias :: AliasAlgebraicDataType a -> Maybe (ParamTypeName a)
paramTNameFromAlias (AliasTok (_, Singleton (Param p), _)) = Just p
paramTNameFromAlias (AliasTok (_, Composite (TyComp (Param p, _, _)), _)) = Just p
paramTNameFromAlias _ = Nothing

baseNameFromAlias :: AliasAlgebraicDataType a -> GenTypeName a
baseNameFromAlias (AliasTok (_, ty, _)) = baseNameFromUnCon ty

paramTNamesFromAlias :: AliasAlgebraicDataType a -> [ParamTypeName a]
paramTNamesFromAlias (AliasTok (_, Singleton (Param p), _)) = [p]
paramTNamesFromAlias (AliasTok (_, Composite c, _)) =
    let ps = paramTNamesFromComp c in
        case c of
            (TyComp (Param p, _, _)) -> p : ps
            (TyComp (Real _, _, _)) -> ps
paramTNamesFromAlias _ = []

boundParamTNamesFromAlias :: AliasAlgebraicDataType a -> [ParamTypeName a]
boundParamTNamesFromAlias (AliasTok (ADTDecl (_, types, _), _, _)) = types

paramTNameFromUnCon :: UnConType a -> Maybe (ParamTypeName a)
paramTNameFromUnCon (Singleton (Real _)) = Nothing
paramTNameFromUnCon (Singleton (Param p)) = Just p
paramTNameFromUnCon (Composite (TyComp (Real _, _, _))) = Nothing
paramTNameFromUnCon (Composite (TyComp (Param p, _, _))) = Just p

baseNameFromUnCon :: UnConType a -> GenTypeName a
baseNameFromUnCon (Singleton (Real r)) = Left r
baseNameFromUnCon (Singleton (Param p)) = Right p
baseNameFromUnCon (Composite (TyComp (Real r, _, _))) = Left r
baseNameFromUnCon (Composite (TyComp (Param p, _, _))) = Right p

aliasNameFrom :: AliasAlgebraicDataType a -> ADTName a
aliasNameFrom (AliasTok (ADTDecl (name, _, _), _, _)) = name

intfNameFrom :: Interface a -> IntfName a
intfNameFrom (IntfTok (IntfDecl (name, _, _, _), _, _)) = name

intfNameFromInst :: Instance a -> IntfName a
intfNameFromInst (InstTok (name, _, _, _, _)) = name

intfNameFromCont :: Constraint a -> IntfName a
intfNameFromCont (Cont (name, _, _)) = name

symNameFrom :: SymbolDeclaration a -> SymbolName a
symNameFrom (SymTok (SymDecl (name, _, _, _), _, _)) = name

symNameFromMultiSymDecl :: MultiSymbolDeclaration a -> SymbolName a
symNameFromMultiSymDecl (MultiSymTok name _ _ _) = name

symNameFromSD :: SDUnion a -> SymbolName a
symNameFromSD (SD sd) = symNameFrom sd
symNameFromSD (MSD msd) = symNameFromMultiSymDecl msd

symNameFromSig :: Signature a -> SymbolName a
symNameFromSig (SigTok (name, _, _)) = name

symNamesFromMatchExpr :: MatchingExpression a -> [SymbolName a]
symNamesFromMatchExpr (MatchExpr uame _) = symNamesFrom uame
    where
        symNamesFrom (MBase sn) = [sn]
        symNamesFrom (MDefault _) = []
        symNamesFrom (MADTBase _) = []
        symNamesFrom (MLit _) = []
        symNamesFrom (MADTApp (ADTAppMExpr (_, ms, _))) =
            concatMap symNamesFromMatchExpr ms

typeFromSig :: Signature a -> Type a
typeFromSig (SigTok (_, ty, _)) = ty

typeFromHint :: Hint a -> Maybe (Type a)
typeFromHint (Hint (mayty, _)) = mayty

adtConsFrom :: AlgebraicDataType a -> [ADTConstructor a]
adtConsFrom (ADTTok (_, cons, _)) = cons

unConFromAlias :: AliasAlgebraicDataType a -> UnConType a
unConFromAlias (AliasTok (_, ty, _)) = ty

unConsFromInst :: Instance a -> [UnConType a]
unConsFromInst (InstTok (_, _, ts, _, _)) = ts

argUnConsFrom :: UnConType a -> [UnConType a]
argUnConsFrom (Singleton _) = []
argUnConsFrom (Composite (TyComp (_, ts, _))) = ts

unConsFromCon :: ADTConstructor a -> [UnConType a]
unConsFromCon (ADTCon (_, types, _)) = types

unConsFromCont :: Constraint a -> [UnConType a]
unConsFromCont (Cont (_, ts, _)) = ts

sigsFromIntf :: Interface a -> [Signature a]
sigsFromIntf (IntfTok (_, sigs, _)) = sigs

symsFromMatches :: [MatchingExpression a] -> [SymbolName a]
symsFromMatches [] = []
symsFromMatches (MatchExpr (MADTApp (ADTAppMExpr (_, ms, _))) _ : t) = symsFromMatches (ms ++ t)
symsFromMatches (MatchExpr (MDefault _) _ : t) = symsFromMatches t
symsFromMatches (MatchExpr (MLit _) _ : t) = symsFromMatches t
symsFromMatches (MatchExpr (MBase s) _ : t) = s : symsFromMatches t
symsFromMatches (MatchExpr (MADTBase _) _ : t) = symsFromMatches t

symsFromLam :: Lambda a -> [SymbolName a]
symsFromLam (Lambda (syms, _, _)) = syms

exprFromPatt :: PatternMatch a -> Expression a
exprFromPatt (PattMatch (e, _, _)) = e

exprCaseFrom :: MatchCase a -> Expression a
exprCaseFrom (Case (_, e, _)) = e

multiPattMatchFrom :: MultiSymbolDeclaration a -> MultiPatternMatch a
multiPattMatchFrom (MultiSymTok _ _ mpm _) = mpm

multiPattMatchFromMultiLam :: MultiLambda a -> MultiPatternMatch a
multiPattMatchFromMultiLam (MultiLambda mpm _) = mpm

multiPattMatchFromMultiBound :: MultiBoundExpression a -> MultiPatternMatch a
multiPattMatchFromMultiBound (MultiBoundExpr msd _ _) = multiPattMatchFrom msd

exprCaseFromMulti :: MultiMatchCase a -> Expression a
exprCaseFromMulti (MultiCase _ e _) = e

matchesCaseFrom :: MatchCase a -> MatchingExpression a
matchesCaseFrom (Case (m, _, _)) = m

multiCaseFromSingleCase :: MatchCase a -> MultiMatchCase a
multiCaseFromSingleCase (Case (m, e, st)) = MultiCase [m] e st

matchesCaseFromMulti :: MultiMatchCase a -> [MatchingExpression a]
matchesCaseFromMulti (MultiCase ms _ _) = ms

exprCasesFromPatt :: PatternMatch a -> [Expression a]
exprCasesFromPatt (PattMatch (_, cs, _)) = map exprCaseFrom cs

exprCasesFromMultiPatt :: MultiPatternMatch a -> [Expression a]
exprCasesFromMultiPatt (MultiPattMatch cs _) = map exprCaseFromMulti cs

matchesCasesFromPatt :: PatternMatch a -> [MatchingExpression a]
matchesCasesFromPatt (PattMatch (_, cs, _)) = map matchesCaseFrom cs

matchesCasesFromMultiPatt :: MultiPatternMatch a -> [[MatchingExpression a]]
matchesCasesFromMultiPatt (MultiPattMatch cs _) = map matchesCaseFromMulti cs

casesFromPatt :: PatternMatch a -> [MatchCase a]
casesFromPatt (PattMatch (_, cs, _)) = cs

casesFromMultiPatt :: MultiPatternMatch a -> [MultiMatchCase a]
casesFromMultiPatt (MultiPattMatch cs _) = cs

casesFromMultiLam :: MultiLambda a -> [MultiMatchCase a]
casesFromMultiLam (MultiLambda mpm _) = casesFromMultiPatt mpm

exprFromSymDecl :: SymbolDeclaration a -> Expression a
exprFromSymDecl (SymTok (_, e, _)) = e

exprFromLam :: Lambda a -> Expression a
exprFromLam (Lambda (_, e, _)) = e

symDeclFromBound :: BoundExpression a -> SymbolDeclaration a
symDeclFromBound (BoundExpr (sd, _, _)) = sd

multiSymDeclFromMultiBound :: MultiBoundExpression a -> MultiSymbolDeclaration a
multiSymDeclFromMultiBound (MultiBoundExpr msd _ _) = msd

exprFromBound :: BoundExpression a -> Expression a
exprFromBound (BoundExpr (_, e, _)) = e

symDeclsFromInst :: Instance a -> [SDUnion a]
symDeclsFromInst (InstTok (_, _, _, s, _)) = s

applierExprFromApp :: AppExpression a -> Expression a
applierExprFromApp (AppExpr (e, _, _)) = e

appliedExprsFromApp :: AppExpression a -> [Expression a]
appliedExprsFromApp (AppExpr (_, es, _)) = es

unAltExprFrom :: Expression a -> UnAltExpression a
unAltExprFrom (Expr (e, _, _)) = e

unAltMExprFrom :: MatchingExpression a -> UnAltMatchingExpression a
unAltMExprFrom (MatchExpr m _) = m

hintFromExpr :: Expression a -> Hint a
hintFromExpr (Expr (_, h, _)) = h

hintFromSymD :: SymbolDeclaration a -> Hint a
hintFromSymD (SymTok (SymDecl (_, h, _, _), _, _)) = h

{- The usage of this should be rare. -}
adtDeclareFromAlias :: AliasAlgebraicDataType a -> ADTDeclare a
adtDeclareFromAlias (AliasTok (d, _, _)) = d

contsFromType :: Type a -> [Constraint a]
contsFromType (Type (cs, _, _)) = cs

contsFromIntf :: Interface a -> [Constraint a]
contsFromIntf (IntfTok (IntfDecl (_, cs, _, _), _, _)) = cs

contsFromInst :: Instance a -> [Constraint a]
contsFromInst (InstTok (_, cs, _, _, _)) = cs

typeFromUnCon :: UnConType a -> Type a
typeFromUnCon uty = Type ([], uty, stateOf uty)

unConFromType :: Type a -> UnConType a
unConFromType (Type (_, uty, _)) = uty

isRealType :: NonRecType a -> Bool
isRealType (Real _) = True
isRealType _ = False

isParamType :: NonRecType a -> Bool
isParamType (Param _) = True
isParamType _ = False

{- It executes an action if a hint contains a Type value, it returns a default value if there's no type-hinting. -}
ifHint :: Hint a -> (Type a -> x) -> y -> Either y x
ifHint (Hint (Nothing, _)) _ noTy = Left noTy
ifHint (Hint (Just ty, _)) f _ = Right $ f ty

{- Like `ifHint`, but with just one possible type. -}
ifHint' :: Hint a -> (Type a -> x) -> x -> x
ifHint' (Hint (Nothing, _)) _ noTy = noTy
ifHint' (Hint (Just ty, _)) f _ = f ty

msdArgsNo :: MultiSymbolDeclaration a -> Int
msdArgsNo msd =
    case matchesCasesFromMultiPatt $ multiPattMatchFrom msd of
        [] -> 0
        (ms : _) -> length ms

ptyOccurUnCon :: ParamTypeName a -> UnConType a -> Bool
ptyOccurUnCon pty ty =
    doOnUnCon ty
        (const False)
        (\p -> repOf p == repOf pty)
        (\_ ts -> any (ptyOccurUnCon pty) ts)
        (\p ts -> repOf p == repOf pty ||
                  any (ptyOccurUnCon pty) ts)

ptyOccurCont :: ParamTypeName a -> Constraint a -> Bool
ptyOccurCont pty c = any (ptyOccurUnCon pty) $ argsOf c

mergePrograms :: Program a -> Program a -> Program a
mergePrograms (Program ds) (Program ds') = Program $ ds ++ ds'

{- NB: this should be used with care. -}
addContsToType :: Type a -> [Constraint a] -> Type a
addContsToType (Type (cs, uty, st)) cs' = Type (cs ++ cs', uty, st)

addContsToSig :: Signature a -> [Constraint a] -> Signature a
addContsToSig (SigTok (sn, ty, st)) cs = SigTok (sn, addContsToType ty cs, st)

addTypeHintToSd :: SymbolDeclaration a -> Type a -> SymbolDeclaration a
addTypeHintToSd (SymTok (SymDecl (sn, Hint (_, hst), args, dst), e, st)) ty =
    SymTok (SymDecl (sn, Hint (Just ty, hst), args, dst), e, st)

addTypeHintToMsd :: MultiSymbolDeclaration a -> Type a -> MultiSymbolDeclaration a
addTypeHintToMsd (MultiSymTok sn (Hint (_, hst)) mpm st) ty = MultiSymTok sn (Hint (Just ty, hst)) mpm st

addTypeHint :: SDUnion a -> Type a -> SDUnion a
addTypeHint (SD sd) ty = SD $ addTypeHintToSd sd ty
addTypeHint (MSD msd) ty = MSD $ addTypeHintToMsd msd ty

showNonRec :: NonRecType a -> String
showNonRec (Real adtname) = tokenRepToStr $ repOf adtname
showNonRec (Param ptyname) = tokenRepToStr $ repOf ptyname

parensShow :: UnConType a -> String
parensShow ty = "(" ++ showUnCon ty ++ ")"

showUnCon :: UnConType a -> String
showUnCon (Singleton nrty) = showNonRec nrty
showUnCon (Composite (TyComp (nrty, ts, _))) =
    showNonRec nrty ++ " " ++ concat (lastmap (\ty -> showOr ty ++ " ") showOr ts)
        where
            showOr ty @ (Singleton _) = showUnCon ty
            showOr ty = parensShow ty

showUnCons :: [UnConType a] -> String
showUnCons ts = concat $ lastmap (\ty -> parensShow ty ++ " ") parensShow ts

showCont :: Constraint a -> String
showCont (Cont (pName, ts, _)) = tokenRepToStr (repOf pName) ++ " " ++ showUnCons ts

{- Operations of string converting:
This is semantically a different thing of Show instance. `show` just takes the token and gives
a string representation of that token, while the following functions extract the string part of
the token. -}

instance AtomRep (ADTName a) where
    repOf (ADTName (s, _)) = tokenRepFromStr s

instance AtomRep (ADTConName a) where
    repOf (ADTConName (s, _)) = tokenRepFromStr s

instance AtomRep (IntfName a) where
    repOf (IntfName (s, _)) = tokenRepFromStr s

instance AtomRep (SymbolName a) where
    repOf (SymName (s, _)) = tokenRepFromStr s

instance AtomRep (ParamTypeName a) where
    repOf (PtyName (s, _)) = tokenRepFromStr s

instance AtomRep (CategoryName a) where
    repOf (CatgName (s, _)) = tokenRepFromStr s

strOfGenName :: GenTypeName a -> TypeRep
strOfGenName (Left rty) = repOf rty
strOfGenName (Right pty) = repOf pty

-- state fetching

stateOfGenName :: GenTypeName a -> a
stateOfGenName (Left r) = stateOf r
stateOfGenName (Right p) = stateOf p

instance HasState Declaration where
    stateOf (ADT d) = stateOf d
    stateOf (AliasADT d) = stateOf d
    stateOf (Intf d) = stateOf d
    stateOf (Ins d) = stateOf d
    stateOf (Sig d) = stateOf d
    stateOf (Let d) = stateOf d
    stateOf (LetMulti d) = stateOf d

instance HasState AlgebraicDataType where
    stateOf (ADTTok (_, _, st)) = st

instance HasState AliasAlgebraicDataType where
    stateOf (AliasTok (_, _, st)) = st

instance HasState Interface where
    stateOf (IntfTok (_, _, st)) = st

instance HasState Instance where
    stateOf (InstTok (_, _, _, _, st)) = st

instance HasState Signature where
    stateOf (SigTok (_, _, st)) = st

instance HasState SymbolDeclaration where
    stateOf (SymTok (_, _, st)) = st

instance HasState MultiSymbolDeclaration where
    stateOf (MultiSymTok _ _ _ st) = st

instance HasState SDUnion where
    stateOf (SD sd) = stateOf sd
    stateOf (MSD msd) = stateOf msd

instance HasState ADTName where
    stateOf (ADTName (_, st)) = st

instance HasState ADTConName where
    stateOf (ADTConName (_, st)) = st

instance HasState IntfName where
    stateOf (IntfName (_, st)) = st

instance HasState SymbolName where
    stateOf (SymName (_, st)) = st

instance HasState ParamTypeName where
    stateOf (PtyName (_, st)) = st

instance HasState NonRecType where
    stateOf (Param p) = stateOf p
    stateOf (Real r) = stateOf r

instance HasState TypeComposite where
    stateOf (TyComp (_, _, st)) = st

instance HasState UnConType where
    stateOf (Singleton nrty) = stateOf nrty
    stateOf (Composite cmpty) = stateOf cmpty

instance HasState Type where
    stateOf (Type (_, _, st)) = st

instance HasState Constraint where
    stateOf (Cont (_, _, st)) = st

instance HasState Hint where
    stateOf (Hint (_, st)) = st

instance HasState Literal where
    stateOf (IntLit (_, st)) = st
    stateOf (DoubleLit (_, st)) = st
    stateOf (CharLit (_, st)) = st
    stateOf (StringLit (_, st)) = st

instance HasState ADTConstructor where
    stateOf (ADTCon (_, _, st)) = st

instance HasState ADTDeclare where
    stateOf (ADTDecl (_, _, st)) = st

instance HasState IntfDeclare where
    stateOf (IntfDecl (_, _, _, st)) = st

instance HasState SymbolDeclare where
    stateOf (SymDecl (_, _, _, st)) = st

instance HasState Expression where
    stateOf (Expr (_, _, st)) = st

instance HasState MatchingExpression where
    stateOf (MatchExpr _ st) = st

instance HasState AppExpression where
    stateOf (AppExpr (_, _, st)) = st

instance HasState PatternMatch where
    stateOf (PattMatch (_, _, st)) = st

instance HasState MultiPatternMatch where
    stateOf (MultiPattMatch _ st) = st

instance HasState MatchCase where
    stateOf (Case (_, _, st)) = st

instance HasState MultiMatchCase where
    stateOf (MultiCase _ _ st) = st

instance HasState Lambda where
    stateOf (Lambda (_, _, st)) = st

instance HasState MultiLambda where
    stateOf (MultiLambda _ st) = st

instance HasState BoundExpression where
    stateOf (BoundExpr (_, _, st)) = st

instance HasState MultiBoundExpression where
    stateOf (MultiBoundExpr _ _ st) = st

instance HasState ADTAppMatchExpression where
    stateOf (ADTAppMExpr (_, _, st)) = st

instance HasState UnAltExpression where
    stateOf (App a) = stateOf a
    stateOf (Base n) = stateOf n
    stateOf (ADTBase n) = stateOf n
    stateOf (Match pm) = stateOf pm
    stateOf (Lam l) = stateOf l
    stateOf (MultiLam l) = stateOf l
    stateOf (Bound b) = stateOf b
    stateOf (MultiBound b) = stateOf b
    stateOf (Lit l) = stateOf l

instance HasState UnAltMatchingExpression where
    stateOf (MADTApp a) = stateOf a
    stateOf (MDefault st) = st
    stateOf (MLit l) = stateOf l
    stateOf (MBase n) = stateOf n
    stateOf (MADTBase n) = stateOf n

-- Operations of conversion from string

strToParamTName :: String -> a -> ParamTypeName a
strToParamTName s k = PtyName (s, k)

strToAdtName :: String -> a -> ADTName a
strToAdtName s k = ADTName (s, k)
