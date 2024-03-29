module Compiler.Codegen.ToCore
    ( generate
    , MainBndr
) where

import Utils.Fancy
import Utils.Data.Foldable
import Utils.Monad
import qualified Utils.Data.Counter as C
import Control.Monad.State
import Compiler.State as With
import Compiler.Ast.Common
import qualified Compiler.Ast.Typed as Ty
import Compiler.Types.Tables
import Compiler.Config.Lexer
import Compiler.Codegen.Lib
import Compiler.Codegen.Env
import qualified Compiler.Codegen.Type as TyGen
import GHC
import CoreSyn as Core
import Var
import Name
import OccName
import IdInfo

data Visibility =
      Local
    | Global

mainSymbol' :: SymbolRep
mainSymbol' = tokenRepFromStr mainSymbol

isSelfRecursive :: Ty.NotedVar With.ProgState -> Ty.NotedExpr With.ProgState -> Bool
isSelfRecursive nVar ne =
    snd $ Ty.widthVisitExprsInExpr' False ne findSelf
    where
        findSelf True expr = (expr, True)
        findSelf False expr @ (Ty.ExprBound (Ty.NotedBound nVar' _ _ _) _ _) =
            (expr, repOf nVar == repOf nVar')
        findSelf selfRec expr = (expr, selfRec)

isSelfRecursive' :: Ty.NotedBound With.ProgState -> Bool
isSelfRecursive' (Ty.NotedBound nVar _ ne _) = isSelfRecursive nVar ne

rawBindingVariable :: Name -> Type -> Visibility -> CodegenEnv Var
rawBindingVariable name ty vis = do
    case vis of
        Local -> return $ mkLocalVar VanillaId name ty vanillaIdInfo
        Global -> return $ mkGlobalVar VanillaId name ty vanillaIdInfo

bindingVariable :: Ty.NotedVar With.ProgState -> Visibility -> CodegenEnv Var
bindingVariable nVar vis = do
    let nVarRep = repOf nVar
    let nVarSt = stateOf nVar
    name <- mkBindingName nVarRep nVarSt
    let nVarTy = Ty.typeOf nVar
    ty <- TyGen.polyTyGen nVarTy
    rawBindingVariable name ty vis

localVariable :: Ty.NotedVar With.ProgState -> CodegenEnv Var
localVariable nVar = bindingVariable nVar Local

globalVariable :: Ty.NotedVar With.ProgState -> CodegenEnv Var
globalVariable nVar = bindingVariable nVar Global

getScrutinee :: Ty.NotedPM With.ProgState -> CodegenEnv (Var, Type)
getScrutinee npm @ (Ty.NotedPM e _ _ st) =
    case Ty.getScrutinee npm of
        Nothing -> do
            exprTy <- TyGen.getExprCoreType e
            foolName <- mkFoolBindingName st
            var <- rawBindingVariable foolName exprTy Local
            return (var, exprTy)
        Just nVar -> do
            ty <- TyGen.polyTyGen $ Ty.typeOf nVar
            var <- localVariable nVar
            return (var, ty)

genBinderFromMin :: Ty.NotedMinimalExpr With.ProgState -> CodegenEnv CoreBndr
genBinderFromMin (Ty.MatchDefault lpty st) = do
    ty <- TyGen.polyTyGen lpty
    foolName <- mkFoolBindingName st
    {- NB: no variable is injected in the state, because this is a fool scrutinee which will not be used. -}
    rawBindingVariable foolName ty Local
genBinderFromMin (Ty.MatchVar nVar _) =
    localVariable nVar

genAlt :: Ty.NotedCase With.ProgState -> CodegenEnv (Alt CoreBndr)
genAlt (Ty.NotedCase (Ty.MatchMinimal (Ty.MatchDefault _ _)) e _) = do
    coreE <- genExpr e
    return (DEFAULT, [], coreE)
{- The singleton variable mathcing expression is compiled as the default case, because it's expected the variable
is the same as the scrutinee's one. -}
genAlt (Ty.NotedCase (Ty.MatchMinimal (Ty.MatchVar _ _)) e _) = do
    coreE <- genExpr e
    return (DEFAULT, [], coreE)
genAlt (Ty.NotedCase (Ty.MatchValMins nVal mins _ _) e _) = do
    altCon <-
        TyGen.valueGen nVal
            (pure . DataAlt)
            (pure . LitAlt)
    binders <- mapM genBinderFromMin mins
    coreE <- genExpr e
    return (altCon, binders, coreE)
genAlt (Ty.NotedCase Ty.MatchValMs {} _ _) =
    cgErr $ TypePanicErr "Deep pattern matching construct not desugared"

genExpr :: Ty.NotedExpr With.ProgState -> CodegenEnv (Expr CoreBndr)
genExpr (Ty.ExprVar nVar _) = do
    var <- localVariable nVar
    return $ Core.Var var
genExpr Ty.ExprDispatchVar {} =
    cgErr $ PanicErr "Dispatch variable expression not solved"
genExpr (Ty.ExprVal nVal _) = do
    TyGen.valueGen nVal
        (\dataCon -> return $ mkConApp dataCon [])
        (return . Core.Lit)
genExpr (Ty.ExprApp (Ty.NotedApp e1 e2 _ _) _) = do
    coreE1 <- genExpr e1
    coreE2 <- genExpr e2
    return $ Core.App coreE1 coreE2
genExpr (Ty.ExprLam (Ty.NotedLam nVar e _ _) _) = do
    var <- localVariable nVar
    coreE <- genExpr e
    return $ Core.Lam var coreE
genExpr (Ty.ExprBound bound @ (Ty.NotedBound nVar _ be _) e _) = do
    (binder, boundCoreE) <- genBinding nVar be Local
    binding <-
        {- Nested "let..in" constructs can be only self-recursive. -}
        if isSelfRecursive' bound
        then pure $ Rec [(binder, boundCoreE)]
        else pure $ NonRec binder boundCoreE
    coreE <- genExpr e
    return $ Core.Let binding coreE
genExpr (Ty.ExprPM npm @ (Ty.NotedPM e cs _ _) _) = do
    coreE <- genExpr e
    (var, ty) <- getScrutinee npm
    alts <- mapM genAlt cs
    return $ Core.Case coreE var ty alts

genBinding
    :: Ty.NotedVar With.ProgState
    -> Ty.NotedExpr With.ProgState
    -> Visibility
    -> CodegenEnv (CoreBndr, Expr CoreBndr)
genBinding nVar nExpr vis = do
    binding <- bindingVariable nVar vis
    coreExpr <- genExpr nExpr
    return (binding, coreExpr)

anyHasConstraints :: [BindingSingleton With.ProgState] -> Bool
anyHasConstraints = any hasCont
    where
        hasCont (nVar, _, _) =
            let ty = Ty.typeOf nVar in
                not . null $ Ty.contsOf ty

isMain :: [BindingSingleton With.ProgState] -> Bool
isMain = any (\(nVar, _, _) -> repOf nVar == mainSymbol')

ifMainUpdate :: [BindingSingleton With.ProgState] -> CoreBind -> CodegenEnv ()
ifMainUpdate bs binding =
    if isMain bs
    then do
        bndr <- getCoreMain binding
        putMain bndr
    else doNothing

strRepOfCoreVar :: Var -> String
strRepOfCoreVar = occNameString . nameOccName . Var.varName

getCoreMain :: CoreBind -> CodegenEnv MainBndr
{- No check on the Var name, it must be the `main`. -}
getCoreMain (NonRec v _) = return v
getCoreMain (Rec bs) =
    {- The check here is necessary in order to find the `main`. -}
    case firstThat (\(v, _) -> strRepOfCoreVar v == mainSymbol) bs of
        Nothing -> cgErr $ PanicErr "No `main` binding found in recursive bindings cluster which should have it"
        Just (v, _) -> return v

topLevelBinding :: TypedBinding With.ProgState -> CodegenEnv (Maybe CoreBind)
topLevelBinding (TyNonRec b @ (nVar, _, ne)) = do
    let bs = [b]
    if anyHasConstraints bs
    then return Nothing
    else do
        (bndr, coreExpr) <- genBinding nVar ne Global
        let cBndr = NonRec bndr coreExpr
        ifMainUpdate bs cBndr
        return $ Just cBndr
topLevelBinding (TyRec bs) = do
    if anyHasConstraints bs
    then return Nothing
    else do
        coreBs <- mapM (\(nVar, _, ne) -> genBinding nVar ne Global) bs
        let cBndr = Rec coreBs
        ifMainUpdate bs cBndr
        return $ Just cBndr

bindings :: TypedProgram With.ProgState -> CodegenEnv CoreProgram
bindings = createBindings . toL
    where
        toL =
            toList'
                :: TypedProgram With.ProgState
                -> [TypedBinding With.ProgState]

        createBindings [] = return []
        createBindings (tyb : t) = do
            maybinding <- topLevelBinding tyb
            coreBs <- createBindings t
            case maybinding of
                Nothing -> return coreBs
                Just coreB -> return $ coreB : coreBs

coreGenRes :: TypedProgram With.ProgState -> CodegenEnv (CoreProgram, [TyCon], MainBndr)
coreGenRes tp = do
    coreProg <- bindings tp
    {- This is a little optimization: only type constructors which are really used in the program are returned. -}
    tyCons <- getAllTyCons
    mn <- getMain
    return (coreProg, tyCons, mn)

generate
    :: DataConsTable With.ProgState
    -> C.AlphabeticCounterObj
    -> TypedProgram With.ProgState
    -> Either CodegenErr (CoreProgram, [TyCon], MainBndr)
generate dct c tp = evalStateT <| coreGenRes tp <| initState c dct
