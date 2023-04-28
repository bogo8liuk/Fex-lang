module Compiler.Desugar.Scrutinee
    ( unify
) where

import Lib.Utils
import Data.List(foldl')
import Control.Monad.State.Lazy
import Lib.Counter as C
import Compiler.Ast.Common
import Compiler.State as With
import qualified Compiler.Desugar.Names as Desugar
import qualified Compiler.Ast.Typed as Ty
import Compiler.Types.Tables

type UnifyScrutineeHandle = State C.AlphabeticCounterObj

ifExistingScrutinee
    :: Ty.NotedPM With.ProgState
    -> (Ty.NotedVar With.ProgState -> UnifyScrutineeHandle a)
    -> UnifyScrutineeHandle a
    -> UnifyScrutineeHandle a
ifExistingScrutinee npm withNVar cont = do
    case Ty.getScrutinee npm of
        Nothing -> cont
        Just nVar -> withNVar nVar

changeNVarsInExpr
    :: Ty.NotedExpr With.ProgState
    -> String
    -> String
    -> UnifyScrutineeHandle (Ty.NotedExpr With.ProgState)
changeNVarsInExpr ne newVarRep oldVarRep =
    return $ changeExpr ne
    where
        changeExpr ne' @ (Ty.ExprVar nVar st) =
            if repOf nVar == oldVarRep
            then
                let varTy = Ty.typeOf nVar in
                let varSt = stateOf nVar in
                let newNVar = Ty.newNotedVar newVarRep varTy varSt in
                    Ty.ExprVar newNVar st
            else ne'
        changeExpr ne' @ (Ty.ExprDispatchVar nVar nToks st) =
            if repOf nVar == oldVarRep
            then
                let varTy = Ty.typeOf nVar in
                let varSt = stateOf nVar in
                let newNVar = Ty.newNotedVar newVarRep varTy varSt in
                    Ty.ExprDispatchVar newNVar nToks st
            else ne'
        changeExpr ne' @ (Ty.ExprVal _ _) = ne'
        changeExpr (Ty.ExprPM (Ty.NotedPM topNe ncs ty pmst) st) =
            let topNe' = changeExpr topNe in
            let ncs' = changeExprInCases ncs in
                Ty.ExprPM (Ty.NotedPM topNe' ncs' ty pmst) st
        changeExpr (Ty.ExprBound (Ty.NotedBound nVar nVars bne bst) extNe st) =
            let bne' = changeExpr bne in
            let extNe' = changeExpr extNe in
            let nVarsMatches = any (\nVar -> repOf nVar == oldVarRep) nVars in
            let nVarMatch = repOf nVar == oldVarRep in
                case (nVarsMatches, nVarMatch) of
                    (False, False) -> Ty.ExprBound (Ty.NotedBound nVar nVars bne' bst) extNe' st
                    (_, True) -> Ty.ExprBound (Ty.NotedBound nVar nVars bne bst) extNe st
                    (True, False) -> Ty.ExprBound (Ty.NotedBound nVar nVars bne bst) extNe' st
        changeExpr (Ty.ExprApp (Ty.NotedApp a1ne a2ne ty appSt) st) =
            let a1ne' = changeExpr a1ne in
            let a2ne' = changeExpr a2ne in
                Ty.ExprApp (Ty.NotedApp a1ne' a2ne' ty appSt) st
        changeExpr ne' @ (Ty.ExprLam (Ty.NotedLam nVar lamNe ty lamSt) st) =
            if repOf nVar == oldVarRep
            then ne'
            else
                let lamNe' = changeExpr lamNe in
                    Ty.ExprLam (Ty.NotedLam nVar lamNe' ty lamSt) st

        changeExprInCases [] = []
        changeExprInCases (nc @ (Ty.NotedCase ncnme ncne ncst) : t) =
            let nVars = Ty.nVarsOf ncnme in
            let t' = changeExprInCases t in
                if any (\nVar -> repOf nVar == oldVarRep) nVars
                then nc : t'
                else
                    let ncne' = changeExpr ncne in
                        Ty.NotedCase ncnme ncne' ncst : t'

unifyScrutineeInNPM' :: Ty.NotedPM With.ProgState -> UnifyScrutineeHandle (Ty.NotedPM With.ProgState)
unifyScrutineeInNPM' npm @ (Ty.NotedPM ne ncs ty st) =
    ifExistingScrutinee npm
        (const $ do
            c <- get
            let (varRep, c') = Desugar.mkProgUniqueName c
            put c'
            ncs' <- changeCases ncs varRep
            return $ Ty.NotedPM ne ncs' ty st
        )
        `else'` do return npm
    where
        changeCases [] _ = return []
        changeCases (Ty.NotedCase (Ty.MatchMinimal (Ty.MatchVar oldNVar nmst)) ncne ncSt : t) varRep = do
            let varSt = stateOf oldNVar
            let varTy = Ty.typeOf oldNVar
            let nme = Ty.MatchMinimal $ Ty.MatchVar (Ty.newNotedVar varRep varTy varSt) nmst
            ncne' <- changeNVarsInExpr ncne varRep (repOf oldNVar)
            t' <- changeCases t varRep
            return $ Ty.NotedCase nme ncne' ncSt : t'
        changeCases (nc @ (Ty.NotedCase (Ty.MatchMinimal (Ty.MatchDefault _ _)) _ _) : t) varRep = do
            t' <- changeCases t varRep
            return $ nc : t'
        changeCases (nc @ (Ty.NotedCase Ty.MatchValMins {} _ _) : t) varRep = do
            t' <- changeCases t varRep
            return $ nc : t'
        changeCases (nc @ (Ty.NotedCase Ty.MatchValMs {} _ _) : t) varRep = do
            t' <- changeCases t varRep
            return $ nc : t'

unifyScrutineeInNPM
    :: Ty.NotedPM With.ProgState
    -> C.AlphabeticCounterObj
    -> (Ty.NotedPM With.ProgState, C.AlphabeticCounterObj)
unifyScrutineeInNPM npm = runState (unifyScrutineeInNPM' npm)

unifyScrutineeInExpr
    :: Ty.NotedExpr With.ProgState
    -> C.AlphabeticCounterObj
    -> (Ty.NotedExpr With.ProgState, C.AlphabeticCounterObj)
unifyScrutineeInExpr ne c = Ty.lengthVisitExprsInExpr' c ne visit
    where
        visit counter (Ty.ExprPM npm st) =
            let (npm', counter') = unifyScrutineeInNPM npm counter in
                (Ty.ExprPM npm' st, counter')
        visit counter ne' = (ne', counter)

unify
    :: TypedProgram With.ProgState
    -> C.AlphabeticCounterObj
    -> (TypedProgram With.ProgState, C.AlphabeticCounterObj)
unify tp c =
    let bindings = toL tp in
    let (bindings', c') = foldl' changeBinding ([], c) bindings in
        (fromList' bindings', c')
    where
        toL =
            toList'
                :: TypedProgram With.ProgState
                -> [TypedBinding With.ProgState]

        changeBindingSingleton counter (nVar, nVars, ne) =
            let (ne', counter') = unifyScrutineeInExpr ne counter in
                ((nVar, nVars, ne'), counter')

        changeBinding (accum, counter) (TyNonRec b) =
            let (b', counter') = changeBindingSingleton counter b in
                (accum ++ [TyNonRec b'], counter')
        changeBinding (accum, counter) (TyRec bs) =
            let (bs', counter') = foldl' change ([], counter) bs in
                (accum ++ [TyRec bs'], counter')

        change (bs, counter) b =
            let (b', counter') = changeBindingSingleton counter b in
                (bs ++ [b'], counter')
