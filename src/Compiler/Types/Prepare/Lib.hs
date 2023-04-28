module Compiler.Types.Prepare.Lib
    ( RawBinding(..)
    , SortedBindings
    , strRep
    , symRepsOf
    , depsOf
    , depsOfBinding
) where

import Data.List(nub)
import Data.Map.Strict as M hiding (map, filter)
import Compiler.Ast.Common
import Compiler.State as With
import qualified Compiler.Ast.Tree as Raw
import Compiler.Types.Tables(PropMethodsTable, existIn)

data RawBinding =
      RawNonRec (Raw.SDUnion With.ProgState)
    | RawRec [Raw.SDUnion With.ProgState]    --Cluster of recursive definitions
    deriving Show

{- Just a list of RawBinding values, but granting they are sorted and ready for type inference. -}
type SortedBindings = [RawBinding]

strRep :: Raw.SDUnion With.ProgState -> SymbolRep
strRep = strOf . Raw.symNameFromSD

symRepsOf :: RawBinding -> [SymbolRep]
symRepsOf (RawNonRec sd) = [strRep sd]
symRepsOf (RawRec sds) = map strRep sds

--TODO: use visitExprsInExpr
depsOf :: Raw.SDUnion With.ProgState -> PropMethodsTable With.ProgState -> [SymbolRep]
depsOf symDecl mhts =
    filter (\dep -> not (dep `existIn` mhts)) $ depsOf' symDecl empty
    where
        depsOf' symDecl' bounds =
            case symDecl' of
                Raw.SD sd ->
                    let syms = map mkKey $ argsOf sd in
                    let updBounds = bounds `union` fromList syms in
                        visitExpr' (Raw.exprFromSymDecl sd) updBounds
                Raw.MSD msd ->
                    let mpm = Raw.multiPattMatchFrom msd in
                        visitMpm mpm bounds

        visitExpr' :: Raw.Expression With.ProgState -> Map SymbolRep () -> [SymbolRep]
        visitExpr' (Raw.Expr (uae, _, _)) = visitExpr uae

        visitExpr :: Raw.UnAltExpression With.ProgState -> Map SymbolRep () -> [SymbolRep]
        visitExpr (Raw.Base sn) bounds =
            let symRep = strOf sn in
                [ symRep | not (symRep `member` bounds) ] --List comprehension, but it's at most a singleton list
        visitExpr (Raw.ADTBase _) _ = []
        visitExpr (Raw.Lit _) _ = []
        visitExpr (Raw.App (Raw.AppExpr (e, es, _))) bounds =
            visitExpr' e bounds ++ concatMap (`visitExpr'` bounds) es
        visitExpr (Raw.Match (Raw.PattMatch (e, cs, _))) bounds =
            visitExpr' e bounds ++ concatMap (`visitCase` bounds) cs
        visitExpr (Raw.Lam (Raw.Lambda (sns, e, _))) bounds =
            let syms = map mkKey sns in
                visitExpr' e $ bounds `union` fromList syms
        visitExpr (Raw.Bound (Raw.BoundExpr (sd, e, _))) bounds =
            let sym = Raw.symNameFrom sd in
            let updBounds = M.insert (strOf sym) () bounds in
                {- Using the updated map for the expression, but for the symbol definition using the old map because
                the nested definition does not depend from itself (the rec call will discover if it is recursive). -}
                visitExpr' e updBounds ++ depsOf' (Raw.SD sd) bounds
        visitExpr (Raw.MultiLam (Raw.MultiLambda mpm _)) bounds =
            visitMpm mpm bounds
        visitExpr (Raw.MultiBound (Raw.MultiBoundExpr msd e _)) bounds =
            let sym = Raw.symNameFromMultiSymDecl msd in
            let updBounds = M.insert (strOf sym) () bounds in
                {- Same as for single binder. -}
                visitExpr' e updBounds ++ depsOf' (Raw.MSD msd) bounds

        visitMpm :: Raw.MultiPatternMatch With.ProgState -> Map SymbolRep () -> [SymbolRep]
        visitMpm (Raw.MultiPattMatch mcs _) bounds =
            concatMap (`visitMultiCase` bounds) mcs

        visitCase :: Raw.MatchCase With.ProgState -> Map SymbolRep () -> [SymbolRep]
        visitCase (Raw.Case (me, e, _)) bounds =
            let syms = map mkKey $ Raw.symNamesFromMatchExpr me in
                visitExpr' e $ bounds `union` fromList syms

        visitMultiCase :: Raw.MultiMatchCase With.ProgState -> Map SymbolRep () -> [SymbolRep]
        visitMultiCase (Raw.MultiCase ms e _) bounds =
            let syms = concatMap (map mkKey . Raw.symNamesFromMatchExpr) ms in
                visitExpr' e $ bounds `union` fromList syms

        mkKey sn = (strOf sn, ())

depsOfBinding :: RawBinding -> PropMethodsTable With.ProgState -> [SymbolRep]
depsOfBinding (RawNonRec sd) mhts = depsOf sd mhts
depsOfBinding (RawRec c) mhts =
    let sdNames = map strRep c in
    let deps = concatMap (`depsOf` mhts) c in
    {- Removing from dependencies the ones which appears in the cluster. -}
    let deps' = filter (`notElem` sdNames) deps in
    {- Removing duplicates, because two or more elements of a cluster may have the same dependecies. -}
        nub deps'
