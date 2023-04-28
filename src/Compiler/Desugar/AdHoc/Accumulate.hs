{-# LANGUAGE FlexibleContexts #-}

module Compiler.Desugar.AdHoc.Accumulate
    ( accumReplacements
    , bindingsToDispatch
) where

import Lib.Utils
import Data.List.NonEmpty hiding (takeWhile, (<|))
import Control.Monad.State.Lazy
import Compiler.State as With
import Compiler.Ast.Common
import Compiler.Types.Make(BindingToRefine)
import Compiler.Types.Tables
import qualified Compiler.Ast.Typed as Ty
import qualified Compiler.Types.Lib.State as S
import Compiler.Desugar.Names(mkDispatchSuffix)
import Compiler.Desugar.AdHoc.Lib

accumReplacements
    {- Eventual top-level symbol where to search the binding. -}
    :: TopSymRep
    -> TypedProgram With.ProgState
    -> PropMethodsTable With.ProgState
    -> [Replacement]
    -> Ty.NotedExpr With.ProgState
    -> (Ty.NotedExpr With.ProgState, [Replacement])
accumReplacements topSym tp mhts repls expr @ (Ty.ExprDispatchVar nVar nToks st) =
    case maybemap Ty.constraintFromVal nToks of
        Nothing -> (expr, repls)
        Just cs ->
            let oldVarRep = repOf nVar in
            let newVarRep = mkDispatchSuffix oldVarRep cs in
            let newVar = Ty.newNotedVar newVarRep <| Ty.removeAllConstraints (Ty.typeOf nVar) <| stateOf nVar in
                if nVar `existIn` tp
                then (Ty.ExprVar newVar st, (TopLevel oldVarRep, newVarRep, cs) : repls)
                {- If it is a property method, then no replacement has to be performed, the method should have already
                been created when evaluating instances. -}
                else if nVar `existIn` mhts
                then (Ty.ExprVar newVar st, repls)
                else (Ty.ExprVar newVar st, (Nested topSym oldVarRep, newVarRep, cs) : repls)
accumReplacements _ _ _ repls expr = (expr, repls)

getReplacementsFromExpr
    :: Ty.NotedExpr With.ProgState
    -> Maybe SymbolRep
    -> TopSymRep
    -> AdHocHandle (Ty.NotedExpr With.ProgState, [Replacement])
getReplacementsFromExpr ne (Just symRep) topSym = do
    let (_, bne) = Ty.widthVisitExprsInExpr' ne ne findBound
    (bne', repls) <- getReplacementsFromExpr bne Nothing topSym
    {- Finding and updating nested expression. -}
    let ne' = Ty.widthVisitExprsInExpr ne $ replaceBound bne'
    return (ne', repls)
    where
        {- NB: using the hypothesys that all nested symbols have different symbols. -}
        findBound expr curExpr @ (Ty.ExprBound (Ty.NotedBound bnVar _ bne _) _ _) =
            if repOf bnVar == symRep
            then (curExpr, bne)
            else (curExpr, expr)
        findBound info curExpr = (curExpr, info)

        {- NB: using the hypothesys that all nested symbols have different symbols. -}
        replaceBound bne' expr @ (Ty.ExprBound (Ty.NotedBound bnVar bnVars _ bst) contNe st) =
            if repOf bnVar == symRep
            then Ty.ExprBound (Ty.NotedBound bnVar bnVars bne' bst) contNe st
            else expr
        replaceBound _ expr = expr
getReplacementsFromExpr ne Nothing topSym = do
    tp <- S.getProg
    mhts <- S.getMethods
    return . Ty.widthVisitExprsInExpr' [] ne $ accumReplacements topSym tp mhts

getReplacementsFromBinding
    :: TypedBinding With.ProgState
    -> [TokenRep]
    -> TokenRep
    -> AdHocHandle [Replacement]
getReplacementsFromBinding tyb nestedSyms topSym = do
    (_, _, ne) <- findSymbolInBinding topSym tyb
    (ne', repls) <- getReplacementsFromExpr ne (last' nestedSyms) topSym
    {- The binding's expression has been updated, so also the typed program has to be updated. -}
    tp <- S.getProg
    let tp' = update topSym (\(nVar, nVars, _) -> (nVar, nVars, ne')) tp
    S.putProg tp'
    return repls
    where
        update =
            kValUpdate
                :: TokenRep
                -> (BindingSingleton With.ProgState -> BindingSingleton With.ProgState)
                -> TypedProgram With.ProgState
                -> TypedProgram With.ProgState

getReplacements' :: BindingToRefine -> AdHocHandle [Replacement]
getReplacements' (symRep :| nested) = do
    tyb <- findSymbolRep symRep
    getReplacementsFromBinding tyb nested symRep

getReplacements :: [BindingToRefine] -> AdHocHandle [Replacement]
getReplacements bsToRef = do
    repls <- mapM getReplacements' bsToRef
    return $ concat repls

bindingsToDispatch
    :: TypedProgram With.ProgState
    -> PropMethodsTable With.ProgState
    -> [BindingToRefine]
    -> Either DispatchErr ([Replacement], TypedProgram With.ProgState)
bindingsToDispatch tp mhts bsToRef =
    case runStateT <| getReplacements bsToRef <| buildEnv tp mhts of
        Left err -> Left err
        Right (repls, st) -> Right (repls, S.fetchProg st)
