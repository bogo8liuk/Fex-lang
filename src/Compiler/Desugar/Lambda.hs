module Compiler.Desugar.Lambda
    ( make
) where

import Compiler.State as With
import Compiler.Ast.Common
import qualified Compiler.Ast.Typed as Ty
import Compiler.Types.Tables

mkLam :: BindingSingleton With.ProgState -> BindingSingleton With.ProgState
mkLam (bindVar, nVars, ne) =
    {- Why reversing? We have a situation like this:
        nVars := [x, y, z]
        ne := e
    And we want to obtain:
        \x -> \y -> \z -> e
    but if we build starting from `x`, then the other symbols are put ahead of the just created lambda so the result
    would be:
        \z -> \y -> \x -> e -}
    let nVars' = reverse nVars in
        (bindVar, [], mkNestedLams nVars' ne)
    where
        mkNestedLams [] ne' = ne'
        mkNestedLams (nVar : t) ne' =
            let argTy = Ty.typeOf nVar in
            let exprTy = Ty.typeOf ne' in
            let lamTy = argTy <> exprTy in
            let lamSt = stateOf nVar in
            let lamTok = Ty.NotedLam nVar ne' lamTy lamSt in
                mkNestedLams t $ Ty.newNotedLam lamTok lamSt

make :: TypedProgram With.ProgState -> TypedProgram With.ProgState
make = fromL . map mkLamsInTyBinding . toList'
    where
        {- Just a cast to quiet the compiler. -}
        fromL = fromList'
            :: [TypedBinding With.ProgState] -> TypedProgram With.ProgState

        mkLamsInTyBinding (TyNonRec b) =
            TyNonRec $ mkLamsInBinding b
        mkLamsInTyBinding (TyRec bs) =
            TyRec $ map mkLamsInBinding bs

        mkLamsInBinding b =
            let (nVar, nVars, ne) = mkLam b in
            let ne' = Ty.widthVisitExprsInExpr ne mkLamInBound in
                (nVar, nVars, ne')

        mkLamInBound (Ty.ExprBound (Ty.NotedBound nVar nVars bne bst) ne st) =
            let (nVar', nVars', bne') = mkLam (nVar, nVars, bne) in
                Ty.ExprBound (Ty.NotedBound nVar' nVars' bne' bst) ne st
        mkLamInBound ne = ne
