module Compiler.Desugar.BreadthPM
    ( SinglePMData
    , MultiPMData
    , MultiPMVars
    , RmBreadthPmErr(..)
    , mkPattMatch
) where

import Lib.Utils
import Control.Monad.State.Lazy
--import Control.Monad.Trans.Identity
import Lib.Result
import qualified Compiler.Config.Types as BITy
import qualified Compiler.Types.Lib.State as S
import Compiler.State as With
import Compiler.Ast.Common
import qualified Compiler.Types.Lib.FreeVars as Fresh
import Compiler.Types.Tables
import qualified Compiler.Ast.Typed as Ty
import qualified Compiler.Types.Lib.SpecialExpr as SpExpr

data RmBreadthPmErr =
      NoMatchingCases
    | TooManyMatches Int ProgState
    | GenErr String

{- Shorthands, just to write less and clearer. -}
type SinglePMData = ([Ty.NotedMatchExpr With.ProgState], Ty.NotedExpr With.ProgState)
type MultiPMData = [SinglePMData]
type MultiPMVars = [Ty.NotedVar With.ProgState]

instance InfoShow RmBreadthPmErr where
    infoShow NoMatchingCases = unexpNoInfo
    infoShow (TooManyMatches n st) =
        "At " ++ show st ++ " there are too many case expressions (" ++ show n ++ "), the limit is " ++
        show BITy.maxTupleSize
    infoShow (GenErr _) = unexpNoInfo

instance DebugShow RmBreadthPmErr where
    dbgShow NoMatchingCases = "No matching cases"
    dbgShow err @ (TooManyMatches _ _) = infoShow err
    dbgShow (GenErr reason) = reason

instance UnreachableState RmBreadthPmErr where
    isUnreachable err @ NoMatchingCases = Just $ dbgShow err
    isUnreachable (TooManyMatches _ _) = Nothing
    isUnreachable err @ (GenErr _) = Just $ dbgShow err

type RmBreadthPmHandle = S.EitherHandle () RmBreadthPmErr

rmErr :: RmBreadthPmErr -> RmBreadthPmHandle a
rmErr = lift . Left

noMatchingCases :: RmBreadthPmHandle a
noMatchingCases = rmErr NoMatchingCases

getTypeOf :: Ty.HasType tok => tok With.ProgState -> RmBreadthPmHandle (Ty.LangTypeScheme With.ProgState)
getTypeOf = return . Ty.typeOf

newTyVarFrom :: Ty.LangVarType With.ProgState -> RmBreadthPmHandle (Ty.LangVarType With.ProgState)
newTyVarFrom v = do
    fv <- S.getFV
    let (varStr, fv') = Fresh.allocFreeVar () fv
    let varRep = tokenRepFromStr varStr
    S.putFV fv'
    return $ Ty.newLVTy varRep (Ty.kindOf v) (Ty.roleOf v) (stateOf v)

replaceVar
    :: Ty.LangVarType With.ProgState
    -> RmBreadthPmHandle (Ty.LangVarType With.ProgState, Ty.LangVarType With.ProgState)
replaceVar v = do
    v' <- newTyVarFrom v
    return (v, v')

instantiate :: Ty.LangTypeScheme With.ProgState -> RmBreadthPmHandle (Ty.LangQualType With.ProgState)
instantiate ty = do
    let boundVars = Ty.bVarsOf ty
    replVars <- mapM replaceVar boundVars
    return $ Ty.instantiate ty replVars

instantiateToken :: (Ty.HasType tok, Ty.UpdateType tok) => tok With.ProgState -> RmBreadthPmHandle (tok With.ProgState)
instantiateToken token = do
    let ty = Ty.typeOf token
    qualTy <- instantiate ty
    let ty' = Ty.liftQualType qualTy
    return $ Ty.updateType token ty'

getTupleCon :: Int -> ProgState -> RmBreadthPmHandle (Ty.NotedVal With.ProgState)
getTupleCon n st = do
    dct <- S.getDataCons
    case BITy.getTupleCon n of
        Nothing -> tupleConNotFound
        Just conRep ->
            case kFind conRep dct of
                Nothing -> tupleConNotFound
                {- Before returning the constructor, it's necessary to update it. -}
                Just nVal -> instantiateToken nVal
    where
        tupleConNotFound = rmErr $ TooManyMatches n st

stateOfPMData :: SinglePMData -> RmBreadthPmHandle ProgState
stateOfPMData ([], _) = rmErr $ GenErr "Zero matching expressions"
stateOfPMData (nme : _, _) = return $ stateOf nme

stateOfPMVars :: MultiPMVars -> RmBreadthPmHandle ProgState
stateOfPMVars [] = rmErr $ GenErr "Zero pattern matching variables"
stateOfPMVars (nVar : _) = return $ stateOf nVar

newCase :: Ty.LangTypeScheme With.ProgState -> SinglePMData -> RmBreadthPmHandle (Ty.NotedCase With.ProgState)
newCase ty pmData @ (nms, ne) = do
    st <- stateOfPMData pmData
    nVal <- getTupleCon (length nms) st
    let nme = Ty.newCompNotedMExpr nVal nms ty st
    return $ Ty.newNotedCase nme ne st

newScrutineeExpr :: MultiPMVars -> RmBreadthPmHandle (Ty.NotedExpr With.ProgState)
newScrutineeExpr nVars = do
    st <- stateOfPMVars nVars
    nVal <- getTupleCon (length nVars) st
    let nValExpr = Ty.newValNotedExpr nVal $ stateOf nVal
    let nVarsEs = map (\v -> Ty.newVarNotedExpr v $ stateOf v) nVars
    fv <- S.getFV
    case SpExpr.newApplication nValExpr nVarsEs fv of
        Left _ -> rmErr $ GenErr "Could not make substitutions in top-level expression of multiple-cases pattern matching"
        Right (ne, fv') -> do
            S.putFV fv'
            return ne

newNotedPM :: MultiPMData -> MultiPMVars -> RmBreadthPmHandle (Ty.NotedPM With.ProgState)
newNotedPM nToks nVars = do
    ne <- newScrutineeExpr nVars
    ty <- getTypeOf ne
    ncs <- mapM (newCase ty) nToks
    ncsTy <- typeOfExprInCases ncs
    ncsSt <- stateOfCases ncs
    return $ Ty.newNotedPM ne ncs ncsTy ncsSt
    where
        typeOfExprInCases [] = noMatchingCases
        typeOfExprInCases (Ty.NotedCase _ ne _ : _) = getTypeOf ne

        stateOfCases [] = noMatchingCases
        stateOfCases (nc : _) = return $ stateOf nc

--TODO: legacy code
{-newNotedPM nToks nVars = newNotedPM' nToks nVars M.empty
    where
        newNotedPM' :: MultiPMData -> MultiPMVars -> VarsReplacing -> RmBreadthPmHandle (Ty.NotedPM With.ProgState)
        newNotedPM' mpmData mpmVars replVarsMap = do
            (topExpr, ncs) <- mkCases mpmData mpmVars replVarsMap Nothing
            pmTy <- getPMType mpmData
            return . Ty.newNotedPM topExpr ncs pmTy $ stateOf topExpr

        getPMType [] = noMatchingCases
        getPMType ((_, ne) : _) = getInfrdType ne

        getNME ([], _) = noMatchingCases
        getNME (nme : _, _) = return nme

        mkCases
            :: MultiPMData
            -> MultiPMVars
            -> VarsReplacing
            {- Just for memoization, it would not be necessary. -}
            -> Maybe (Ty.NotedExpr With.ProgState)
            -> RmBreadthPmHandle (Ty.NotedExpr With.ProgState, [Ty.NotedCase With.ProgState])
        mkCases [] _ _ _ = noMatchingCases
        mkCases _ [] _ _ = noMatchingCases
        mkCases mpmData mpmVars @ (nVar : _) replVarsMap mayne = do
            (nc, restMpmData) <- mkCase mpmData mpmVars replVarsMap
            topExpr <-
                case mayne of
                    Nothing -> pure . Ty.newVarNotedExpr nVar $ stateOf nVar
                    Just ne -> pure ne
            (_, ncs) <-
                case restMpmData of
                    [] -> pure (topExpr, [])
                    {- Here the rec call -}
                    otherMpmData -> mkCases otherMpmData mpmVars replVarsMap $ Just topExpr
            return (topExpr, nc : ncs)

        mkLastCase nme ne replVarsMap = do
            ne' <- replaceNVars ne replVarsMap
            return . Ty.newNotedCase nme ne' $ stateOf nme

        mkCase :: MultiPMData -> MultiPMVars -> VarsReplacing -> RmBreadthPmHandle (Ty.NotedCase With.ProgState, MultiPMData)
        mkCase mpmData @ (([nme], ne) : _) mpmVars @ [_] replVarsMap = do
            {- NB: the rest of the pairs (matching exprs, exprs) are ignored, because this is the last case and if
            there are more than one matching expression equal among them, the flow of the program execution will reach
            always the first case. -}
            (_, notEqs, _, _, newMap) <- mkTopExprAndMExpr mpmData mpmVars
            nc <- mkLastCase nme ne $ M.unions [replVarsMap, newMap]
            return (nc, notEqs)
        mkCase mpmData mpmVars replVarsMap = do
            (nme, notEqs, eqs, otherVars, newMap) <- mkTopExprAndMExpr mpmData mpmVars
            npm <- newNotedPM' eqs otherVars $ M.unions [replVarsMap, newMap]
            let expr = Ty.newPMNotedExpr npm $ stateOf npm
            return (Ty.newNotedCase nme expr $ stateOf nme, notEqs)

        mkTopExprAndMExpr
            :: MultiPMData
            -> MultiPMVars
            -> RmBreadthPmHandle
                ( Ty.NotedMatchExpr With.ProgState
                , MultiPMData
                , MultiPMData
                , MultiPMVars
                , VarsReplacing
                )
        mkTopExprAndMExpr [] [] = noMatchingCases
        mkTopExprAndMExpr [] _ = noMatchingCases
        mkTopExprAndMExpr _ [] = noMatchingCases
        mkTopExprAndMExpr mpmData (_ : otherVars) = do
            (nme, eqs, notEqs, replVarsMap) <- pickFirstAndEqs mpmData
            cutEqs <- cutFirst eqs
            return (nme, notEqs, cutEqs, otherVars, replVarsMap)

        cutFirst [] = return []
        cutFirst (([], _) : _) = noMatchingCases
        cutFirst ((_ : t, x) : tt) = do
            tt' <- cutFirst tt
            return $ (t, x) : tt'

        pickFirstAndEqs
            :: MultiPMData
            -> RmBreadthPmHandle (Ty.NotedMatchExpr With.ProgState, MultiPMData, MultiPMData, VarsReplacing)
        pickFirstAndEqs [] = noMatchingCases
        pickFirstAndEqs (toks : t) = do
            nme <- getNME toks
            (replVarsMaps, eqs, notEqs) <- filterEq nme t
            return (nme, toks : eqs, notEqs, M.unions replVarsMaps)

        filterEq elemToEq t = foldM (nmeEqFltr elemToEq) ([], [], []) t

        nmeEqFltr elemToEq (replVarsMaps, eqs, notEqs) toks = do
            firstNME <- getNME toks
            eqRes <- nmeEq elemToEq firstNME M.empty
            case eqRes of
                Nothing -> return (replVarsMaps, eqs, notEqs ++ [toks])
                Just (m, newNme) -> do
                    updToks <- replaceFirstNME toks newNme
                    return (m : replVarsMaps, eqs ++ [updToks], notEqs)

        nmeEq
            :: Ty.NotedMatchExpr With.ProgState
            -> Ty.NotedMatchExpr With.ProgState
            -> VarsReplacing
            -> RmBreadthPmHandle (Maybe (VarsReplacing, Ty.NotedMatchExpr With.ProgState))
        nmeEq nme @ (Ty.MatchMinimal (Ty.MatchDefault _ _)) (Ty.MatchMinimal (Ty.MatchDefault _ _)) m =
            return $ Just (m, nme)
        nmeEq (Ty.MatchMinimal (Ty.MatchDefault _ _)) nme @ (Ty.MatchMinimal (Ty.MatchVar _ _)) m =
            return $ Just (m, nme)
        nmeEq nme @ (Ty.MatchMinimal (Ty.MatchVar _ _)) (Ty.MatchMinimal (Ty.MatchDefault _ _)) m =
            return $ Just (m, nme)
        nmeEq nme @ (Ty.MatchMinimal (Ty.MatchVar nVar _)) (Ty.MatchMinimal (Ty.MatchVar nVar' _)) m =
            return $ Just (M.insert (repOf nVar') (nVar', nVar) m, nme)
        nmeEq (Ty.MatchValMs nVal nms lpty st) (Ty.MatchValMs nVal' nms' _ _) m =
            if repOf nVal == repOf nVal'
            then do
                manyEqRes <- foldM nmeEq' (Just (m, [])) $ zip nms nms'
                case manyEqRes of
                    Nothing -> return Nothing
                    Just (m', newNms) -> return $ Just (m', Ty.MatchValMs nVal newNms lpty st)
            else return Nothing
            where
                nmeEq' Nothing _ = return Nothing
                nmeEq' (Just (m, accumNms)) (nme, nme') = do
                    eqRes <- nmeEq nme nme' m
                    case eqRes of
                        Nothing -> return Nothing
                        Just (m', newNme) -> return $ Just (m', accumNms ++ [newNme])
        {- NB: the rest of the cases contains also the ones with minimal expressions applied to a noted value.
        Logically, it should behave like the case above, but it's granted for this module not to emit noted matching
        expressions with minimal expressions applied to a noted value, so that case can safely ignored. -}
        nmeEq _ _ _ = return Nothing

        replaceFirstNME ([], _) _ = noMatchingCases
        replaceFirstNME (_ : t, ne) nme = return (nme : t, ne)

        replaceNVars ne @ (Ty.ExprVar nVar st) replVarsMap = do
            case M.lookup (repOf nVar) replVarsMap of
                Nothing -> return ne
                Just (_, nVar') -> return $ Ty.ExprVar nVar' st
        replaceNVars ne @ (Ty.ExprVal _ _) _ = return ne
        replaceNVars (Ty.ExprApp (Ty.NotedApp ne ne1 ty appSt) st) replVarsMap = do
            ne' <- replaceNVars ne replVarsMap
            ne1' <- replaceNVars ne1 replVarsMap
            return $ Ty.ExprApp (Ty.NotedApp ne' ne1' ty appSt) st
        replaceNVars (Ty.ExprBound (Ty.NotedBound bNVar bNVars bne bst) ne st) replVarsMap = do
            noTopMap <- deleteShadowing [bNVar] replVarsMap
            updMap <- deleteShadowing bNVars noTopMap
            bne' <- replaceNVars bne updMap
            ne' <- replaceNVars ne noTopMap
            return $ Ty.ExprBound (Ty.NotedBound bNVar bNVars bne' bst) ne' st
            where
                deleteShadowing [] replMap = return replMap
                deleteShadowing (nVar : t) replMap = deleteShadowing t $ M.delete (repOf nVar) replMap
        replaceNVars (Ty.ExprLam (Ty.NotedLam nVar ne ty lamSt) st) replVarsMap = do
            let updMap = M.delete (repOf nVar) replVarsMap
            ne' <- replaceNVars ne updMap
            return $ Ty.ExprLam (Ty.NotedLam nVar ne' ty lamSt) st
        replaceNVars (Ty.ExprPM (Ty.NotedPM topNe ncs ty pmst) st) replVarsMap = do
            topNe' <- replaceNVars topNe replVarsMap
            ncs' <- mapM replaceInNCase ncs
            return $ Ty.ExprPM (Ty.NotedPM topNe' ncs' ty pmst) st
            where
                replaceInNCase (Ty.NotedCase nme ne ncst) = do
                    updMap <- deleteShadowing nme replVarsMap
                    ne' <- replaceNVars ne updMap
                    return $ Ty.NotedCase nme ne' ncst

                deleteShadowing (Ty.MatchMinimal (Ty.MatchDefault _ _)) replMap =
                    return replMap
                deleteShadowing (Ty.MatchMinimal (Ty.MatchVar nVar _)) replMap =
                    return $ M.delete (repOf nVar) replMap
                deleteShadowing (Ty.MatchValMs _ nms _ _) replMap =
                    foldM (flip deleteShadowing) replMap nms
                {- See the note at nmeEq to know this case is ignored. -}
                deleteShadowing Ty.MatchValMins {} replMap =
                    return replMap
                                        -}

mkPattMatch
    :: MultiPMData
    -> MultiPMVars
    -> Fresh.FV ()
    -> DataConsTable With.ProgState
    -> Either RmBreadthPmErr (Ty.NotedPM With.ProgState, Fresh.FV ())
mkPattMatch nToks nVars fv dct =
    case runStateT
        <| newNotedPM nToks nVars
        <| S.initGenState fv noElems dct noElems noElems noElems noElems noElems () of
        Left err -> Left err
        Right (npm, st) -> Right (npm, S.fetchFV st)
