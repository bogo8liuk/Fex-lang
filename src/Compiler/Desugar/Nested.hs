{- The module goal is to change names inside "let..in" expressions, in order to have non-ambigous local names. -}

module Compiler.Desugar.Nested
    ( replaceNested
) where

import Lib.Utils
import qualified Lib.Counter as C
import Data.Map.Strict as M hiding (map)
import Compiler.State as With
import Compiler.Ast.Common
import qualified Compiler.Ast.Tree as Raw
import Compiler.Desugar.Names

type OccurredLocally = Map String String
type NestedEnv = (OccurredLocally, C.CounterObj)

deleteSyms :: OccurredLocally -> [Raw.SymbolName With.ProgState] -> OccurredLocally
deleteSyms m syms = forAll syms deleteSym m
    where
        deleteSym m' sym =
            let symRep = repOf sym in
                M.delete symRep m'

{- The function which visits an expression to make substitutions. -}
replaceNestedInExpr
    :: NestedEnv
    -> [Raw.MatchingExpression With.ProgState]
    -> Raw.Expression With.ProgState
    -> (Raw.Expression With.ProgState, NestedEnv)
replaceNestedInExpr (local, c) ms e @ (Raw.Expr (Raw.Base sn, h, st)) =
    let shadowers = concatMap Raw.symNamesFromMatchExpr ms in
    let shadowLocal = deleteSyms local shadowers in
    let symRep = repOf sn in
    let symSt = stateOf sn in
        case M.lookup symRep shadowLocal of
            Nothing -> (e, (shadowLocal, c))
            Just symRep' ->
                let sn' = Raw.buildSymbolName symRep' symSt in
                    (Raw.Expr (Raw.Base sn', h, st), (shadowLocal, c))
replaceNestedInExpr (local, c) ms (Raw.Expr (Raw.Bound (Raw.BoundExpr (sd, e, bst)), h, st)) =
    let shadowers = concatMap Raw.symNamesFromMatchExpr ms in
    let shadowLocal = deleteSyms local shadowers in
    let sdsn = Raw.symNameFrom sd in
    let symRep = repOf sdsn in
    {- Here the magic moment where name is turned into an non-ambigous one. -}
    let (symRep', c') = mkNestedUniqueName symRep c in
    let sd' = Raw.updateSymbolNameInSd sd symRep' in
    {- Handling shadowing caused by symbol declaration arguments. -}
    let syms = argsOf sd in
    let shadowLocal' = deleteSyms shadowLocal syms in
    {- If there is another symbol with the same representation of `symRep`, it's going to be overwritten and this
    right because that symbol would be shadowed. -}
    let local' = M.insert symRep symRep' shadowLocal' in
        (Raw.Expr (Raw.Bound (Raw.BoundExpr (sd', e, bst)), h, st), (local', c'))
replaceNestedInExpr (local, c) ms (Raw.Expr (Raw.MultiBound (Raw.MultiBoundExpr msd e bst), h, st)) =
    let shadowers = concatMap Raw.symNamesFromMatchExpr ms in
    let shadowLocal = deleteSyms local shadowers in
    let sdsn = Raw.symNameFromMultiSymDecl msd in
    let symRep = repOf sdsn in
    {- Here the magic moment where name is turned into an non-ambigous one. -}
    let (symRep', c') = mkNestedUniqueName symRep c in
    let msd' = Raw.updateSymbolNameInMsd msd symRep' in
    {- If there is another symbol with the same representation of `symRep`, it's going to be overwritten and this
    right because that symbol would be shadowed. -}
    let local' = M.insert symRep symRep' shadowLocal in
        (Raw.Expr (Raw.MultiBound (Raw.MultiBoundExpr msd' e bst), h, st), (local', c'))
replaceNestedInExpr (local, c) ms e @ (Raw.Expr (Raw.Lam (Raw.Lambda (syms, _, _)), _, _)) =
    let shadowers = concatMap Raw.symNamesFromMatchExpr ms ++ syms in
    let shadowLocal = deleteSyms local shadowers in
        (e, (shadowLocal, c))
replaceNestedInExpr (local, c) ms e =
    let shadowers = concatMap Raw.symNamesFromMatchExpr ms in
    let shadowLocal = deleteSyms local shadowers in
        (e, (shadowLocal, c))

replaceNestedInBinding
    :: (C.CounterObj, [Raw.SDUnion With.ProgState])
    -> Raw.SDUnion With.ProgState
    -> (C.CounterObj, [Raw.SDUnion With.ProgState])
replaceNestedInBinding (c, sds) (Raw.SD sd) =
    let (sd', (_, c')) = Raw.visitExprsInSd (empty, c) sd replaceNestedInExpr in
        (c', sds ++ [Raw.SD sd'])
replaceNestedInBinding (c, sds) (Raw.MSD msd) =
    let (msd', (_, c')) = Raw.visitExprsInMsd (empty, c) msd replaceNestedInExpr in
        (c', sds ++ [Raw.MSD msd'])

replaceNested :: [Raw.SDUnion With.ProgState] -> [Raw.SDUnion With.ProgState]
replaceNested sds =
    snd $ forAll sds replaceNestedInBinding (C.new :: C.CounterObj, [])