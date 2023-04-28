module Compiler.Names.Check.Correct
    ( namesCheck
) where

import Lib.Utils
import Data.Map.Strict hiding (null)
import Control.Monad
import Lib.Monad.Utils
import Control.Monad.State.Lazy
import Compiler.Ast.Common
import Compiler.Ast.Tree
import Compiler.Names.Check.Lib as Names
import Compiler.State as With

typesInConstraint :: Constraint With.ProgState -> Names.Op ()
typesInConstraint c = do
    let names = adtNamesFromCont c
    allAdtExist names

propInConstraint :: Constraint With.ProgState -> Names.Op ()
propInConstraint c = do
    let propName = intfNameFromCont c
    tables <- get
    if repOf propName `Names.isInterfaceMember` tables
    then doNothing'
    else Names.err $ NoProp propName

contCheck :: [ParamTypeName With.ProgState] -> Constraint With.ProgState -> Names.Op ()
contCheck binders c = do
    propInConstraint c
    typesInConstraint c
    let pts = paramTNamesFromCont c
    atLeastOnePty pts
    allParamTypesBound binders pts
    where
        atLeastOnePty = notM . pure . null

typesInSignature :: Signature With.ProgState -> Names.Op ()
typesInSignature sig = do
    let names = adtNamesFromSig sig
    allAdtExist names

symInSignature :: Signature With.ProgState -> Names.Op ()
symInSignature sig = do
    let sym = symNameFromSig sig
    tables <- get
    if repOf sym `Names.isSymbolMember` tables
    then doNothing'
    else Names.err $ NoSymSig sym

contsCheckInType :: Type With.ProgState -> Names.Op ()
contsCheckInType ty = do
    let unCon = unConFromType ty
    let binders = paramTypes' unCon
    let cs = contsFromType ty
    mapM_ (contCheck binders) cs

contsCheckInSignature :: Signature With.ProgState -> Names.Op ()
contsCheckInSignature sig = do
    let ty = typeFromSig sig
    contsCheckInType ty

sigsCheckInProperty :: Interface With.ProgState -> Names.Op ()
sigsCheckInProperty prop = do
    let sigs = sigsFromIntf prop
    mapM_ typesInSignature sigs
    mapM_ contsCheckInSignature sigs

contsCheckInProperty :: Interface With.ProgState -> Names.Op ()
contsCheckInProperty prop = do
    let cs = contsFromIntf prop
    let binders = argsOf prop
    mapM_ (contCheck binders) cs

typesInInstance :: Instance With.ProgState -> Names.Op ()
typesInInstance inst = do
    let names = adtNamesFromInst inst
    allAdtExist names

propInInstance :: Instance With.ProgState -> Names.Op ()
propInInstance inst = do
    let propName = intfNameFromInst inst
    tables <- get
    if repOf propName `Names.isInterfaceMember` tables
    then doNothing'
    else Names.err $ NoProp propName

contsCheckInInstance :: Instance With.ProgState -> Names.Op ()
contsCheckInInstance inst = do
    let cs = contsFromInst inst
    let binders = paramTNamesFromInst inst
    mapM_ (contCheck binders) cs

getMethods :: IntfName With.ProgState -> Names.Op [SymbolNameRep]
getMethods propName = do
    tables <- get
    case Names.getPropMethodsRep (repOf propName) tables of
        Nothing -> Names.err $ NoProp propName
        Just reps -> return reps

allImplsExist :: Instance With.ProgState -> Names.Op ()
allImplsExist inst = do
    let propName = intfNameFromInst inst
    symReps <- getMethods propName
    let sds = symDeclsFromInst inst
    mapM_ (existsImpl symReps) sds
    where
        existsImpl symReps sd = do
            let symRep = repOf $ symNameFromSD sd
            if symRep `elem` symReps
            then doNothing'
            else Names.err $ NoImpl symRep inst

existsAdtName :: ADTName With.ProgState -> Names.Op ()
existsAdtName adtName = do
    tables <- get
    if repOf adtName `Names.isAdtMember` tables
    then doNothing'
    else Names.err $ NoAdt adtName

allAdtExist :: [ADTName With.ProgState] -> Names.Op ()
allAdtExist = mapM_ existsAdtName

allParamTypesBound :: [ParamTypeName With.ProgState] -> [ParamTypeName With.ProgState] -> Names.Op ()
allParamTypesBound binders pts =
    case firstThat isNotBound pts of
        Nothing -> doNothing'
        Just pty -> Names.err $ UnboundParamType pty
    where
        isNotBound pty =
            let ptyRep = repOf pty in
                all (\binder -> repOf binder /= ptyRep) binders

typesInAdt :: AlgebraicDataType With.ProgState -> Names.Op ()
typesInAdt adt = do
    let names = adtNamesFromCons $ adtConsFrom adt
    allAdtExist names

typesInAlias :: AliasAlgebraicDataType With.ProgState -> Names.Op ()
typesInAlias alias = do
    let names = adtNamesFromUnCon $ unConFromAlias alias
    allAdtExist names

allParamTypesBoundInAdt :: AlgebraicDataType With.ProgState -> Names.Op ()
allParamTypesBoundInAdt adt = do
    let binders = argsOf adt
    let pts = paramTNamesFromAdt adt
    allParamTypesBound binders pts

allParamTypesBoundInAlias :: AliasAlgebraicDataType With.ProgState -> Names.Op ()
allParamTypesBoundInAlias alias = do
    let binders = argsOf alias
    let pts = paramTNamesFromAlias alias
    allParamTypesBound binders pts

tryInsert :: Names.Keeper -> SymbolName With.ProgState -> Names.Keeper
tryInsert tables sym =
    case repOf sym `Names.insertSymbol` tables of
        Nothing -> tables
        Just tables' -> tables'

checkSymsInMatchExprs :: [MatchingExpression With.ProgState] -> Names.Op ()
checkSymsInMatchExprs ms =
    foldM_ checkSyms empty ms
    where
        checkSyms syms (MatchExpr (MDefault _) _) =
            return syms
        checkSyms syms (MatchExpr (MLit _) _) =
            return syms
        checkSyms syms (MatchExpr (MBase sym) _) = do
            let symRep = repOf sym
            if repOf sym `member` syms
            then Names.err $ DupSymMatchExpr sym
            else return $ insert symRep () syms
        checkSyms syms (MatchExpr (MADTBase _) _) =
            return syms
        checkSyms syms (MatchExpr (MADTApp (ADTAppMExpr (_, ms', _))) _) =
            foldM checkSyms syms ms'

checkSymsInArgs :: [SymbolName With.ProgState] -> Names.Op ()
checkSymsInArgs =
    foldM_ checkSyms empty
    where
        checkSyms syms sym = do
            let symRep = repOf sym
            if repOf sym `member` syms
            then Names.err $ DupSymArgs sym
            else return $ insert symRep () syms

checkExprGenCase :: [MatchingExpression With.ProgState] -> Expression With.ProgState -> Names.Op ()
checkExprGenCase [] e =
    checkExpr e
checkExprGenCase (MatchExpr (MDefault _) _ : mt) e =
    checkExprGenCase mt e
checkExprGenCase (MatchExpr (MLit _) _ : mt) e =
    checkExprGenCase mt e
checkExprGenCase (MatchExpr (MBase sym) _ : mt) e = do
    local' (`tryInsert` sym) $ checkExprGenCase mt e
checkExprGenCase (MatchExpr (MADTBase con) _ : mt) e = do
    let conRep = repOf con
    tables <- get
    if conRep `Names.isConstructorMember` tables
    then checkExprGenCase mt e
    else Names.err $ NoCon con
checkExprGenCase (MatchExpr (MADTApp (ADTAppMExpr (con, ms, _))) _ : mt) e = do
    let conRep = repOf con
    tables <- get
    if conRep `Names.isConstructorMember` tables
    then checkExprGenCase (ms ++ mt) e
    else Names.err $ NoCon con

checkCase :: MatchCase With.ProgState -> Names.Op ()
checkCase (Case (me, e, _)) = do
    let ms = [me]
    checkSymsInMatchExprs ms
    checkExprGenCase ms e

checkMultiCase :: MultiMatchCase With.ProgState -> Names.Op ()
checkMultiCase (MultiCase ms e _) = do
    checkSymsInMatchExprs ms
    checkExprGenCase ms e

checkPattMatch :: PatternMatch With.ProgState -> Names.Op ()
checkPattMatch (PattMatch (e, cs, _)) = do
    checkExpr e
    mapM_ checkCase cs

checkMultiPattMatch :: MultiPatternMatch With.ProgState -> Names.Op ()
checkMultiPattMatch (MultiPattMatch mcs _) =
    mapM_ checkMultiCase mcs

checkUnAltExpr :: UnAltExpression With.ProgState -> Names.Op ()
checkUnAltExpr (Base sym) = do
    let symRep = repOf sym
    tables <- get
    if symRep `Names.isSymbolMember` tables ||
       symRep `Names.isSignatureMember` tables
    then doNothing'
    else Names.err $ NoSym sym
checkUnAltExpr (ADTBase con) = do
    let conRep = repOf con
    tables <- get
    if conRep `Names.isConstructorMember` tables
    then doNothing'
    else Names.err $ NoCon con
checkUnAltExpr (Lit _) = doNothing'
checkUnAltExpr (App (AppExpr (e, es, _))) = do
    checkExpr e
    mapM_ checkExpr es
checkUnAltExpr (Lam (Lambda (syms, e, _))) = do
    checkSymsInArgs syms
    local' addLamArgs $ checkExpr e
    where
        addLamArgs tables =
            forAll syms tryInsert tables
checkUnAltExpr (Bound (BoundExpr (sd, e, _))) = do
    local' withNested $ exprsInSd sd
    local' withNested $ checkExpr e
    where
        withNested tables =
            tryInsert tables $ symNameFrom sd
checkUnAltExpr (Match pm) =
    checkPattMatch pm
checkUnAltExpr (MultiLam (MultiLambda mpm _)) =
    checkMultiPattMatch mpm
checkUnAltExpr (MultiBound (MultiBoundExpr msd e _)) = do
    local' withNested $ exprsInMultiSd msd
    local' withNested $ checkExpr e
    where
        withNested tables =
            tryInsert tables $ symNameFromMultiSymDecl msd

typesCheckInHint :: Hint With.ProgState -> Names.Op ()
typesCheckInHint (Hint (Nothing, _)) = doNothing'
typesCheckInHint (Hint (Just ty, _)) = do
    let names = adtNamesFromUnCon $ unConFromType ty
    allAdtExist names
    contsCheckInType ty

checkExpr :: Expression With.ProgState -> Names.Op ()
checkExpr (Expr (uae, hint, _)) = do
    typesCheckInHint hint
    checkUnAltExpr uae

symInGenSymDecl :: SDUnion With.ProgState -> Names.Op ()
symInGenSymDecl sd = do
    let sym = symNameFromSD sd
    tables <- get
    if repOf sym `Names.isAnyPropMethod` tables
    then Names.err $ DupSymProp sym
    else doNothing'

exprsInSd :: SymbolDeclaration With.ProgState -> Names.Op ()
exprsInSd (SymTok (SymDecl (_, hint, args, _), e, _)) = do
    typesCheckInHint hint
    checkSymsInArgs args
    local' withArgs $ checkExpr e
    where
        withArgs tables =
            forAll args tryInsert tables

exprsInMultiSd :: MultiSymbolDeclaration With.ProgState -> Names.Op ()
exprsInMultiSd (MultiSymTok _ hint mpm _) = do
    typesCheckInHint hint
    checkMultiPattMatch mpm

exprsInGenSd :: SDUnion With.ProgState -> Names.Op ()
exprsInGenSd (SD sd) = exprsInSd sd
exprsInGenSd (MSD msd) = exprsInMultiSd msd

genSdCheck :: SDUnion With.ProgState -> Names.Op ()
genSdCheck gsd = do
    symInGenSymDecl gsd
    exprsInGenSd gsd

sdCheckInInstance :: Instance With.ProgState -> Names.Op ()
sdCheckInInstance inst = do
    let sds = symDeclsFromInst inst
    mapM_ exprsInGenSd sds

checkDecl :: Declaration With.ProgState -> Names.Op ()
checkDecl (ADT d) = do
    typesInAdt d
    allParamTypesBoundInAdt d
checkDecl (AliasADT d) = do
    typesInAlias d
    allParamTypesBoundInAlias d
checkDecl (Sig d) = do
    typesInSignature d
    symInSignature d
    contsCheckInSignature d
checkDecl (Intf d) = do
    sigsCheckInProperty d
    contsCheckInProperty d
checkDecl (Ins d) = do
    propInInstance d
    contsCheckInInstance d
    typesInInstance d
    allImplsExist d
    sdCheckInInstance d
checkDecl (Let sd) =
    genSdCheck $ SD sd
checkDecl (LetMulti msd) =
    genSdCheck $ MSD msd

namesCheck :: [Declaration With.ProgState] -> Names.Op ()
namesCheck = mapM_ checkDecl
