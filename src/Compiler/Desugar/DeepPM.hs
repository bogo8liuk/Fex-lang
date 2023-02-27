module Compiler.Desugar.DeepPM
    ( RmDeepPmErr
    , removeDeepPm
) where

import Lib.Utils
import Data.List(foldl')
import Lib.Monad.Utils
import Control.Monad.State
import Lib.Result
import qualified Lib.Counter as C
import Compiler.Desugar.Names
import Compiler.State as With
import Compiler.Ast.Common
import qualified Compiler.Ast.Typed as Ty
import Compiler.Types.Tables
import qualified Compiler.Types.Lib.SpecialExpr as SpExpr
import qualified Compiler.Types.Lib.FreeVars as Fresh

data RmDeepPmErr =
      TypePanicErr String
    | PanicErr String

instance InfoShow RmDeepPmErr where
    infoShow _ = unexpNoInfo

instance DebugShow RmDeepPmErr where
    dbgShow (TypePanicErr reason) = reason
    dbgShow (PanicErr reason) = reason

instance UnreachableState RmDeepPmErr where
    isUnreachable err = Just $ dbgShow err

type RmDeepPmHandle = StateT C.AlphabeticCounterObj (Either RmDeepPmErr)

rmErr :: RmDeepPmErr -> RmDeepPmHandle a
rmErr err = lift $ Left err

getType :: Ty.HasType tok => tok With.ProgState -> RmDeepPmHandle (Ty.LangTypeScheme With.ProgState)
getType = return . Ty.typeOf

getPMType :: [Ty.NotedCase With.ProgState] -> RmDeepPmHandle (Ty.LangTypeScheme With.ProgState)
getPMType [] = rmErr $ PanicErr "No cases in pattern matching"
getPMType ((Ty.NotedCase _ e _) : _) = getType e

matchCases :: Ty.NotedCase With.ProgState -> Ty.NotedCase With.ProgState -> RmDeepPmHandle Bool
matchCases (Ty.NotedCase nme _ _) (Ty.NotedCase nme1 _ _) = return $ matchNME nme nme1
    where
        matchNME (Ty.MatchMinimal _) _ = True
        matchNME _ (Ty.MatchMinimal _) = True
        matchNME (Ty.MatchValMs nVal _ _ _) (Ty.MatchValMs nVal1 _ _ _) = nVal == nVal1
        matchNME (Ty.MatchValMs nVal _ _ _) (Ty.MatchValMins nVal1 _ _ _) = nVal == nVal1
        matchNME (Ty.MatchValMins nVal _ _ _) (Ty.MatchValMs nVal1 _ _ _) = nVal == nVal1
        matchNME (Ty.MatchValMins nVal _ _ _) (Ty.MatchValMins nVal1 _ _ _) = nVal == nVal1

matchAll :: Ty.NotedCase With.ProgState -> Bool
matchAll (Ty.NotedCase (Ty.MatchMinimal _) _ _) = True
matchAll (Ty.NotedCase Ty.MatchValMins {} _ _) = False
matchAll (Ty.NotedCase Ty.MatchValMs {} _ _) = False

deleteUnreachable :: [Ty.NotedCase With.ProgState] -> RmDeepPmHandle [Ty.NotedCase With.ProgState]
deleteUnreachable ncs = return $ takeWhile' (not . matchAll) ncs

rmDeepPmInCases :: [Ty.NotedCase With.ProgState] -> RmDeepPmHandle [Ty.NotedCase With.ProgState]
rmDeepPmInCases ncs = do
    ncs' <- deleteUnreachable ncs
    rmDeepPmInCases' ncs'
    where
        rmDeepPmInCases' [] = return []
        rmDeepPmInCases' (nc : nct) = do
            case nc of
                Ty.NotedCase (Ty.MatchValMs _ [] _ _) _ _ -> do
                    nct' <- rmDeepPmInCases nct
                    return $ nc : nct'
                Ty.NotedCase (Ty.MatchValMs nVal nms ty mst) _ st -> do
                    {- Dividing rest of the cases into ones which would match the current one and ones which do not. -}
                    (matchAlts, notMatchAlts) <- partitionM (matchCases nc) nct
                    notMatchAlts' <- addMatchAll matchAlts notMatchAlts
                    {- Creating variables corresponding to arguments of matching expression. -}
                    nVars <- mapM (\nme' -> newVar nme' $ stateOf nme') nms
                    {- Creating, from variables, the expressions to match in the future. -}
                    let nVarsEs = map (\nVar -> Ty.newVarNotedExpr nVar $ stateOf nVar) nVars

                    let nVarsTs = map Ty.typeOf nVars
                    {- Creating sub-cases, one for each expression, for example, let the matching cases:

                        C m1 ... mn -> e1
                        C w1 ... wn -> e2
                        x           -> e3

                    What happens is that cases are "unrolled":

                        m1 -> e1
                        ...
                        w1 -> e2
                        _ -> let x = C ... in e3
                        ---- first list ----
                        mn -> e1
                        ...
                        wn -> e2
                        _ -> let x = C ... in e3
                        ---- n-th list ----

                    case `x -> e3` is not unrolled, because there's nothing to unroll
                    -}
                    ncss <- mapM (mkManyIndexedCases $ nc : matchAlts) $ indexing nVarsTs
                    {- Associating expressions to match, each with a list of unrolled matching cases. -}
                    let pms = zip nVarsEs ncss
                    {- This is the updated left-hand-side expression of the current case. -}
                    ne <- mkFinalCase pms
                    ne' <- addEventualBound matchAlts nVal nVarsEs ne
                    {- Doing the same procedure with non-matching cases. -}
                    nct' <- rmDeepPmInCases notMatchAlts'
                    let mins = map (\nVar -> Ty.MatchVar nVar $ stateOf nVar) nVars
                    return $ Ty.NotedCase (Ty.MatchValMins nVal mins ty mst) ne' st : nct'
                _ -> do
                    nct' <- rmDeepPmInCases nct
                    return $ nc : nct'

        newVar nme varSt = do
            counter <- get
            let (varRep, counter') = mkProgUniqueName counter
            put counter'
            ty <- getType nme
            return $ Ty.newNotedVar varRep ty varSt

        addEventualBound ncs' nVal nVars ne =
            case last' ncs' of
                Nothing -> return ne
                Just (Ty.NotedCase (Ty.MatchMinimal (Ty.MatchVar nVar _)) _ _) -> do
                    nestedNe <- conApplication nVal nVars
                    let bne = Ty.NotedBound nVar [] nestedNe $ stateOf nVar
                    return . Ty.newBoundNotedExpr bne ne $ stateOf ne
                _ -> return ne

        addMatchAll matchAlts notMatchAlts =
            case last' matchAlts of
                Nothing -> return notMatchAlts
                Just nc ->
                    if matchAll nc
                    then return $ notMatchAlts ++ [nc]
                    else return notMatchAlts

        conApplication nVal args = do
            let st = stateOf nVal
            let headNe = Ty.newValNotedExpr nVal st
            let fv = Fresh.newFreeVarsContainer
            case SpExpr.newApplication headNe args fv of
                Left _ -> rmErr $ PanicErr "Could not create an application expression as case expression"
                Right (ne, _) -> return ne

        {- It takes the n-th sub-matching-expression of a matching expression. If the matching expression is minimal,
        then it builds a new case such that:

            if the original matching expression is of the form:
                _ -> e
            then a new case is built of the form:
                _ -> e
            where the default has not the same type of the original one;

            if the original matching expression iis of the form:
                v -> e
            then a new case is built of the form:
                _ -> let v = C args in e
            where the default has not the same type of the original one and (C args) is the new matching expression.
            Anyway, the "let..in" expression is not built by takeNth.
        -}
        takeNth nme @ (Ty.MatchMinimal _) (_, ty) =
            return . Ty.newDefNotedMExpr ty $ stateOf nme
        takeNth (Ty.MatchValMs _ nms _ _) (n, _) =
            case elemAt n nms of
                Nothing -> rmErr $ PanicErr "Overflow indexing of sub-matching expressions"
                Just nme -> return nme
        takeNth (Ty.MatchValMins _ mins _ _) (n, _) =
            case elemAt n mins of
                Nothing -> rmErr $ PanicErr "Overflow indexing of sub-matching minimal expressions"
                Just minE -> return $ Ty.newMinNotedMExpr minE

        mkManyIndexedCases ncs' ixTy = mapM (mkIndexedCases ixTy) ncs'

        mkIndexedCases ixTy (Ty.NotedCase nme ne matchNcSt) = do
            nthNme <- takeNth nme ixTy
            return $ Ty.newNotedCase nthNme ne matchNcSt

        mkFinalCase [] = rmErr $ PanicErr "No sub-cases to build the final case"
        mkFinalCase [(e, cs)] = do
            cs' <- rmDeepPmInCases cs
            let est = stateOf e
            pmTy <- getPMType cs'
            return $ Ty.newPMNotedExpr (Ty.newNotedPM e cs' pmTy est) est
        mkFinalCase ((e, cs) : t) = do
            cs' <- rmDeepPmInCases cs
            finNe <- mkFinalCase t
            cs'' <- injectExpr finNe cs'
            let est = stateOf e
            pmTy <- getPMType cs''
            return $ Ty.newPMNotedExpr (Ty.newNotedPM e cs'' pmTy est) est

        injectExpr _ [] = rmErr $ PanicErr "No matching cases, at least one should be present"
        {- Just the first case is updated with the injection. The other ones have not to be changed, because
        the injection of the expression which is expected to be a pattern matching expression is the continuation
        of variables pattern matching. Explicitly:

            match e with
                C m1 m2 m3 -> e1    --initial case, where m1, m2, m3 are sub-matching expressions
                  |  |  |
                  V  V  V
                  v1 v2 v3          --variables which replace m1, m2, m3
        The new situation will be:
            match e with
                C v1 v2 v3 ->
                    match v1 with
                        m1 -> ...
                            match v2 with
                                m2 -> ...
                                    match v3 with
                                        m3 -> ...
                                        <other-cases>
                                <other-cases>
                        <other-cases>

        where the <other-cases> are the alternatives which have not to have the continuations `match v2 with m2 -> ...`,
        `match v3 with m3 -> ...`, because `match v2 with m2 -> ...` must be present only under the hypothesis of
        `match v1 with m1 -> ...` and `match v3 with m3 -> ...` must be present only under the hypothesis of
        `match v2 with m2 -> ...` (which are always the first cases). -}
        injectExpr e ((Ty.NotedCase nme _ ncSt) : others) = return $ Ty.NotedCase nme e ncSt : others

rmDeepPmInNPM
    :: C.AlphabeticCounterObj
    -> Ty.NotedPM With.ProgState
    -> Either RmDeepPmErr (C.AlphabeticCounterObj, Ty.NotedPM With.ProgState)
rmDeepPmInNPM c (Ty.NotedPM ne ncs ty st) =
    case runStateT (rmDeepPmInCases ncs) c of
        Left err -> Left err
        Right (ncs', c') -> Right (c', Ty.NotedPM ne ncs' ty st)

rmDeepPmInExpr
    :: Ty.NotedExpr With.ProgState
    -> C.AlphabeticCounterObj
    -> Either RmDeepPmErr (Ty.NotedExpr With.ProgState, C.AlphabeticCounterObj)
rmDeepPmInExpr e c =
    case Ty.lengthVisitExprsInExpr' (Right c) e rm of
        (_, Left err) -> Left err
        (expr, Right c') -> Right (expr, c')
    where
        rm err @ (Left _) expr = (expr, err)
        rm (Right counter) expr @ (Ty.ExprPM npm st) =
            case rmDeepPmInNPM counter npm of
                Left err -> (expr, Left err)
                Right (counter', npm') -> (Ty.ExprPM npm' st, Right counter')
        rm rightCounter expr = (expr, rightCounter)

removeDeepPm
    :: TypedProgram With.ProgState
    -> C.AlphabeticCounterObj
    -> Either RmDeepPmErr (TypedProgram With.ProgState, C.AlphabeticCounterObj)
removeDeepPm tp c =
    let bindings = toL tp in
        case foldl' rmInBinding (Right ([], c)) bindings of
            Left err -> Left err
            Right (bindings', c') -> Right (fromList' bindings', c')
    where
        toL =
            toList'
                :: TypedProgram With.ProgState
                -> [TypedBinding With.ProgState]

        rmInBindingSingleton counter (nVar, nVars, ne) =
            case rmDeepPmInExpr ne counter of
                Left err -> Left err
                Right (ne', counter') -> Right ((nVar, nVars, ne'), counter')

        rmInBinding (Right (accum, counter)) (TyNonRec b) =
            case rmInBindingSingleton counter b of
                Left err -> Left err
                Right (b', counter') -> Right (accum ++ [TyNonRec b'], counter')
        rmInBinding (Right (accum, counter)) (TyRec bs) =
            case forAll bs rm (Right ([], counter)) of
                Left err -> Left err
                Right (bs', counter') -> Right (accum ++ [TyRec bs'], counter')
        rmInBinding err @ (Left _) _ = err

        rm (Right (bs, counter)) b =
            case rmInBindingSingleton counter b of
                Left err -> Left err
                Right (b', counter') -> Right (bs ++ [b'], counter')
        rm err @ (Left _) _ = err
