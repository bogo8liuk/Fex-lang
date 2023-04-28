{-# LANGUAGE LambdaCase #-}

module Compiler.Desugar.AdHoc.Create
    ( instantiateBindings
) where

import Lib.Utils
import Data.List(partition)
import Data.Map.Strict as M hiding (map, filter, splitAt, partition)
import Control.Monad
import Lib.Monad.Utils
import Control.Monad.State
import Compiler.State as With
import qualified Compiler.Types.Lib.State as S
import Compiler.Ast.Common
import qualified Compiler.Ast.Typed as Ty
import Compiler.Types.Tables
import Compiler.Desugar.AdHoc.Lib
import Compiler.Desugar.AdHoc.Accumulate

findBindingToUpdate
    :: SymbolToReplace
    -> AdHocHandle
        ( AdHocBinding
        , TopSymRep
        )
findBindingToUpdate symToRepl = do
    (symToFind, oldVarRep, nestedToFind) <-
        case symToRepl of
            TopLevel oldVarRep -> pure (oldVarRep, oldVarRep, False)
            Nested topSymRep oldVarRep -> pure (topSymRep, oldVarRep, True)
    ((nVar, nVars, ne), mayrecReps) <- findSymbolAndRecReps symToFind
    if nestedToFind
    then do
        nestedNe <- findNested symToFind oldVarRep ne
        return (createAdHocBinding (nVar, nVars, nestedNe) mayrecReps, symToFind)
    else return (createAdHocBinding (nVar, nVars, ne) mayrecReps, symToFind)
    where
        findNested topSymRep oldVarRep ne =
            case snd $ Ty.widthVisitExprsInExpr' Nothing ne $ findNested' oldVarRep of
                Nothing -> dispatchErr $ SymNotFound (topSymRep, Just oldVarRep)
                Just expr -> return expr

        findNested' oldVarRep Nothing expr @ (Ty.ExprBound (Ty.NotedBound nVar _ _ _) _ _) =
            if oldVarRep == strOf nVar
            then (expr, Just expr)
            else (expr, Nothing)
        findNested' _ res expr = (expr, res)

        createAdHocBinding bSing Nothing = NonRec bSing
        createAdHocBinding bSing (Just recRepls) = Rec bSing recRepls

{- It can create a new binding singleton from a replacement, but if the eventual binding singleton has a name which
already exists in the typed program, Nothing is returned. It returns even eventual accumulated replacements which
have been collected while visiting the expression of binding to replace. -}
createNewBinding
    :: Replacement
    -> AdHocHandle
        ( Maybe AdHocBinding
        , [Replacement]
        )
createNewBinding (symToRepl, newVarRep, cs) = do
    tp <- S.getProg
    if newVarRep `existIn` tp
    then return (Nothing, [])
    else do
        (adhBinding, topSymRep) <- findBindingToUpdate symToRepl
        mayrecReps <- getRecReps adhBinding
        (nVar, nVars, ne) <- getBindingSingleton adhBinding
        (dispArgs, args) <- splitDispArgs nVars
        argsRepl <- mkArgReplLam $ zip dispArgs cs
        mhts <- S.getMethods
        let newNVar = Ty.newNotedVar newVarRep <| Ty.removeAllConstraints (Ty.typeOf nVar) <| stateOf nVar
        let (ne', repls) = Ty.widthVisitExprsInExpr' [] ne $ updateExprAndAccum topSymRep tp mhts argsRepl
        return (Just $ mkAdHocBinding (newNVar, args, ne') mayrecReps, repls)
    where
        splitDispArgs nVars = do
            let csNo = length cs
            return $ splitAt csNo nVars

        mkArgReplLam pairedArgs =
            return $
                \case
                    val @ (Ty.DispatchVal _ _) -> val
                    var @ (Ty.DispatchVar nVar st) ->
                        case firstThat (\(arg, _) -> strOf nVar == strOf arg) pairedArgs of
                            Nothing -> var
                            Just (_, c) -> Ty.newDispatchVal' c st

        updateExprAndAccum topSym tp mhts replace repls (Ty.ExprDispatchVar nVar dispToks st) =
            let dispToks' = map replace dispToks in
            let ne = Ty.ExprDispatchVar nVar dispToks' st in
                accumReplacements topSym tp mhts repls ne
        updateExprAndAccum _ _ _ _ repls ne = (ne, repls)

findRecReplacements :: [Replacement] -> [SymbolRep] -> AdHocHandle ([RecReplacement], [Replacement])
findRecReplacements repls recSyms = return $ partition onlyRec repls
    where
        onlyRec repl =
            let topSymRep = getTopSym repl in
                topSymRep `elem` recSyms

replaceNested
    :: BindingSingleton With.ProgState
    {- Nested binding. -}
    -> BindingSingleton With.ProgState
    -> OldVarRep
    -> AdHocHandle (BindingSingleton With.ProgState)
replaceNested (nVar, nVars, ne) (nVar', nVars', ne') oldVarRep = do
    let updNe = Ty.widthVisitExprsInExpr ne replace
    return (nVar, nVars, updNe)
    where
        replace expr @ (Ty.ExprBound bound @ (Ty.NotedBound bnVar _ _ bst) ne'' st) =
            let bnVarRep = strOf bnVar in
                if bnVarRep == oldVarRep
                {- Keeping the old let..in expression, since it can happen other replacements are necessary. -}
                then Ty.ExprBound bound (Ty.ExprBound (Ty.NotedBound nVar' nVars' ne' bst) ne'' st) st
                else expr
        replace expr = expr

handleNestedRepls
    :: BindingSingleton With.ProgState
    -> [Replacement]
    -> AdHocHandle (BindingSingleton With.ProgState, [Replacement])
handleNestedRepls bSing = foldM createNested (bSing, [])
    where
        createNested (bSing', repls) repl @ (TopLevel _, _, _) = return (bSing', repl : repls)
        createNested (bSing', repls) repl @ (Nested _ oldVarRep, _, _) = do
            (maybinding, repls') <- createNewBinding repl
            case maybinding of
                Nothing -> dispatchErr $ NestedAsGlobal oldVarRep
                Just nestedAdhBinding -> do
                    nestedBSing <- getBindingSingleton nestedAdhBinding
                    updBSing <- replaceNested bSing' nestedBSing oldVarRep
                    return (updBSing, repls' ++ repls)

createNewBindings :: [Replacement] -> AdHocHandle ()
createNewBindings [] = doNothing'
createNewBindings (repl : t) = do
    (maybinding, repls) <- createNewBinding repl
    case maybinding of
        Nothing -> createNewBindings t
        Just (NonRec bSing) -> do
            {- Resolving replacements for nested symbols eagerly. -}
            (bSing', repls') <- handleNestedRepls bSing repls
            let tyb = TyNonRec bSing'
            tp <- S.getProg
            let updTp = addElem tyb tp
            S.putProg updTp
            createNewBindings $ repls' ++ t
        (Just (Rec bSing @ (nVar, _, _) recs)) -> do
            let recReps = map fst recs
            {- Replacements which come from the same recursive binding are evaluated immediately and not with the
            recursive call, in order to build the new recursive binding. -}
            (recRepls, otherRepls) <- findRecReplacements repls recReps
            let nVarRep = strOf nVar
            (recBs, otherRepls') <- replaceRec (fromList [(nVarRep, ())]) [] [] recRepls
            let recBs' = bSing : recBs
            let tyb = TyRec $ recBs' ++ getRemainingRecs recBs' recs
            tp <- S.getProg
            let updTp = addElem tyb tp
            S.putProg updTp
            createNewBindings $ otherRepls ++ otherRepls' ++ t
    where
        replaceRec occTable repls recBs (recRepl : rrt) = do
            let topSymRep = getTopSym recRepl
            if topSymRep `M.member` occTable
            then replaceRec occTable repls recBs rrt
            else do
                (maybinding, newRepls) <- createNewBinding recRepl
                let updOccTable = M.insert topSymRep () occTable
                case maybinding of
                    Nothing -> replaceRec updOccTable repls recBs rrt
                    Just (NonRec _) -> dispatchErr $ NonRecInRec topSymRep
                    Just (Rec bSing recs) -> do
                        let recReps = map fst recs
                        let recReps' = deleteAlreadyOcc occTable recReps
                        {- Resolving replacements for nested symbols eagerly. -}
                        (bSing', newRepls') <- handleNestedRepls bSing newRepls
                        {- Splitting recursive replacements. -}
                        (recRepls, otherRepls) <- findRecReplacements newRepls' recReps'
                        replaceRec updOccTable <| otherRepls ++ repls <| bSing' : recBs <| rrt ++ recRepls
        replaceRec _ repls recBs [] = return (recBs, repls)

        deleteAlreadyOcc occTable = filter (not . (`M.member` occTable))

        getRemainingRecs bs recs =
            fltmap (isRem bs) recs

        isRem bs (recRep, b) =
            if any (\(nVar, _, _) -> strOf nVar == recRep) bs
            then Nothing
            else Just b

handleNestedEagerly :: [Replacement] -> AdHocHandle [Replacement]
handleNestedEagerly [] = return []
handleNestedEagerly (repl @ (TopLevel _, _, _) : t) = do
    t' <- handleNestedEagerly t
    return $ repl : t'
handleNestedEagerly ((symToRepl @ (Nested topSymRep oldVarRep), _, _) : t) = do
    (adhBinding, _) <- findBindingToUpdate symToRepl
    bSing <- getBindingSingleton adhBinding
    (updBSing, repls) <- handleNestedRepls bSing []
    tp <- S.getProg
    let updTp = updateTyProg topSymRep (updateBinding updBSing) tp
    S.putProg updTp
    handleNestedEagerly $ repls ++ t
    where
        updateTyProg =
            kValUpdate
                :: SymbolRep
                -> (BindingSingleton With.ProgState -> BindingSingleton With.ProgState)
                -> TypedProgram With.ProgState
                -> TypedProgram With.ProgState

        updateBinding updBSing (nVar, nVars, ne) =
            (nVar, nVars, Ty.widthVisitExprsInExpr ne $ updateExpr updBSing)

        updateExpr (nVar, nVars, ne) expr @ (Ty.ExprBound (Ty.NotedBound bNVar _ _ bst) followNe st) =
            if strOf bNVar == oldVarRep
            then Ty.ExprBound (Ty.NotedBound nVar nVars ne bst) followNe st
            else expr
        updateExpr _ expr = expr

instantiate :: [Replacement] -> AdHocHandle ()
instantiate repls = do
    topLvRepls <- handleNestedEagerly repls
    createNewBindings topLvRepls

instantiateBindings
    :: [Replacement]
    -> TypedProgram With.ProgState
    -> PropMethodsTable With.ProgState
    -> Either DispatchErr (TypedProgram With.ProgState)
instantiateBindings repls tp mhts =
    case execStateT (instantiate repls) $ buildEnv tp mhts of
        Left err -> Left err
        Right st -> Right $ S.fetchProg st
