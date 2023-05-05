module Compiler.Desugar.Alias.Lib.Alias
    ( CycleAliasesErrorInfo
    , AliasSubstRes
    , aliasSubstitution
) where

import Lib.Utils
import Data.List as List
import Data.List.NonEmpty as NEList hiding (map, (<|))
import Data.Map.Strict as Map hiding (map)
import Data.Semigroup
import Compiler.Ast.Common
import Compiler.Ast.Tree as Raw
import qualified Compiler.State as With

type AliasMap = Map String ([Raw.ParamTypeName With.ProgState], Raw.UnConType With.ProgState)

type CycleAliasesErrorInfo = [Raw.AliasAlgebraicDataType With.ProgState]

type AliasSubstRes a = KnowledgeOneOf a CycleAliasesErrorInfo

{- `expandTypes ps tys ty` replaces the i-th parametric type of `ps` with the i-th type in `tys`
in the Type `ty` (so the elements of `ps` are searched for in `ty`).
NB: `ty` is treated as an accumulator, namely it is changed in-place and then it is returned. -}
expandTypes :: [Raw.ParamTypeName With.ProgState]
            -> [Raw.UnConType With.ProgState]
            -> Raw.UnConType With.ProgState
            -> Raw.UnConType With.ProgState
expandTypes [] _ ty = ty
expandTypes _ [] ty = ty --Unreachable
expandTypes (par : t) (ty' : t') ty =
    expandTypes t t' $ Raw.doOnUnCon ty
        (\_ -> ty)  --nothing to replace
        (\p ->
            if repOf p == repOf par
            then ty'
            else ty)
        {- TODO: This is a very inefficient hack to change a Type in-place (due to the rec call in a map function).
        NB: in the rec call, the lists are passed and not reduced, but this won't cause an infinite loop because
        sooner or later, it will arrive to the non-rec case (the first two callbacks passed to `doOnUnCon`). -}
        (\a ts -> let ts' = map (expandTypes (par : t) (ty' : t')) ts in
            Raw.buildRealCompUnCon a ts' $ stateOf ty)   --setting the state of the old type
        (\p ts -> let ts' = map (expandTypes (par : t) (ty' : t')) ts in
            if repOf p == repOf par
            {- The parametric type `p` has also params (`ts`), thus, besides the substitution of `p` with type `ty`,
            it is also necessary that the new type `ty` gets the old params of `p` (but changed by the rec call). -}
            then sconcat (ty' :| ts')
            else Raw.buildParamCompUnCon p ts' $ stateOf ty)

{- Given a list of paramtetric types `params`, a string `name` representing an adt name, a type `bty` and another
type `ty`, it substitutes each occurrence in `ty` of `name` with `bty` according to elements of `params` which stand
inside `ty`. Here a schematic example to make the situation clearer:

types starting with uppercase letter are considered to be concrete types, while ones starting with lowercase
letter are supposed to be paramtetric types:

A a1 a2 a3 ...          --A is `name`, a1, a2, a3, ... are `params`
A T1 T2 T3 ...          --this is `ty`
B a1 K1 a2 K2 a3 K3 ... --this is `bty`

the output must be:
B T1 K1 T2 K2 T3 K3 ...

-}
replace :: [Raw.ParamTypeName With.ProgState]
        -> String
        -> Raw.UnConType With.ProgState
        -> Raw.UnConType With.ProgState
        -> Raw.UnConType With.ProgState
replace params name bty ty =
    Raw.doOnUnCon ty
        (\adt -> let name' = repOf adt in
            if name == name'
            then bty    --it's only the base type and nothing more because this is the case where only a real type appears.
            else ty)
        (\_ -> ty)
        {- This works under the hypothesis that the args number check has already been performed. -}
        (\adt ts ->
            let name' = repOf adt in
            let ts' = map (replace params name bty) ts in
            let st = stateOf ty in
                if name == name'
                then expandTypes params ts' bty
                else Raw.buildRealCompUnCon adt ts' st)
        (\par ts -> Raw.buildParamCompUnCon par (map (replace params name bty) ts) $ stateOf ty)

{- Given an alias `a` and an alias `a'`, it substitutes each occurrence in implementation of `a'` of the alias' name
of `a` with the type which implements `a`. -}
replaceAlias :: Raw.AliasAlgebraicDataType With.ProgState
             -> Raw.AliasAlgebraicDataType With.ProgState
             -> Raw.AliasAlgebraicDataType With.ProgState
replaceAlias a a' =
    let params = Raw.boundParamTNamesFromAlias a in
    let name = Raw.aliasNameFrom a in
    let strName = repOf name in
    let aliasSt = stateOf a' in
    let adtDecl = Raw.adtDeclareFromAlias a' in
    let bty = Raw.unConFromAlias a in
    let ty = Raw.unConFromAlias a' in
        Raw.buildAlias adtDecl (replace params strName bty ty) aliasSt

splitAliases :: [Raw.Declaration With.ProgState]
             -> ([Raw.AliasAlgebraicDataType With.ProgState], [Raw.Declaration With.ProgState])
splitAliases l = onFst (map unWrap) . Raw.splitDecls l $ Raw.AOFSome [AOFAlias]
    where
        unWrap (AliasADT a) = a

{- `subst a l1 l2 l1' l2'` replaces every occurrence of the name of alias `a` with the name of the type
which implements `a` in all aliases of `l1` and `l2`. `l1'` and `l2'` are just accumulator lists. -}
subst :: Raw.AliasAlgebraicDataType With.ProgState
      -> [Raw.AliasAlgebraicDataType With.ProgState]
      -> [Raw.AliasAlgebraicDataType With.ProgState]
      -> [Raw.AliasAlgebraicDataType With.ProgState]  --accum list of the first list
      -> [Raw.AliasAlgebraicDataType With.ProgState]  --accum list of the second list
      -> ([Raw.AliasAlgebraicDataType With.ProgState], [Raw.AliasAlgebraicDataType With.ProgState])
{- This is the base case (when the algorithm ends), but elements has been inserted to the head, so
the order they were changed (they are reversed), thus they get reversed one more time to keep the
original order. Maybe this a useless operation and can be removed. -}
subst a [] [] l1 l2 = (List.reverse l1, List.reverse l2)
subst a [] (a'' : t'') l1 l2 = subst a [] t'' l1 $ replaceAlias a a'' : l2
subst a (a' : t') l l1 l2 = subst a t' l (replaceAlias a a' : l1) l2

{- `findCycleOfAlias adt ty l res` looks for `adt` in `ty`, if found, then the aliases in `res`
constitutes a cycle and `res` is returned, else `adt` is searched in implementations of aliases
in `l`, if found, then a rec call is performed with the alias name whose implementation was
containing `adt` as first argument. -}
findCycleOfAlias :: Raw.ADTName With.ProgState                          --current adt name to find
                 -> Raw.UnConType With.ProgState                             --original type
                 -> [Raw.AliasAlgebraicDataType With.ProgState]         --read-only list to find the cycle
                 -> [Raw.AliasAlgebraicDataType With.ProgState]         --accum list to eventually return
                 -> Maybe [Raw.AliasAlgebraicDataType With.ProgState]
findCycleOfAlias adt ty l res =
    if existAdt adt ty
    then Just res
    else case find (existAdt adt . Raw.unConFromAlias) l of
        {- Searching for the new adt name (the alias name). -}
        Just alias -> findCycleOfAlias (Raw.aliasNameFrom alias) ty l (alias : res)
        Nothing -> Nothing
    where
        existAdt name ty = any (\name' -> repOf name' == repOf name) $ Raw.realTypes' ty

{- Given two lists of aliases, it iterates all the elements of the first list and for each one
looks for a cycling dependency of aliases in the second list. It returns the list of aliases which
makes the cycle. -}
findCycle :: [Raw.AliasAlgebraicDataType With.ProgState]    --list to analyze
          -> [Raw.AliasAlgebraicDataType With.ProgState]    --read-only list to find the cycle
          -> [Raw.AliasAlgebraicDataType With.ProgState]
findCycle [] _ = [] --unreachable
findCycle (a : t) l = let aliasName = Raw.aliasNameFrom a in
    case findCycleOfAlias aliasName (Raw.unConFromAlias a) l [a] of
        Nothing -> findCycle t l
        Just res -> res

buildMap :: [Raw.AliasAlgebraicDataType With.ProgState] -> AliasMap -> AliasMap
buildMap [] m = m
buildMap (a : t) m =
    buildMap t $ Map.insert (repOf $ Raw.aliasNameFrom a) (argsOf a, Raw.unConFromAlias a) m

__aliasSubst :: [Raw.AliasAlgebraicDataType With.ProgState]
             -> [Raw.AliasAlgebraicDataType With.ProgState]     --accumulator list
             -> [Raw.AliasAlgebraicDataType With.ProgState]     --original read-only list to eventually find cycles
             -> Either [Raw.AliasAlgebraicDataType With.ProgState] AliasMap
__aliasSubst [] al _ = Right $ buildMap al empty
{- How can it say there is a cycle just from one alias token? The alias replacing takes place also on
the sublist `t`, so when a new alias token is analyzed, it is exptected that previous aliases have been
already replaced in it. -}
__aliasSubst (a : t) al rol = if isCycle a
                              then Left $ findCycle rol rol
                              {- As said before, the replacing takes place also on `t`. -}
                              else case subst a t al [] [] of
                                  (l1, l2) -> __aliasSubst l1 (l2 ++ [a]) rol
    where
        {- Affirming there's a cycle is quite easy: the name of an alias occurs in the type of its definition.
        NB: this function just says there's a cycle, it does not find it. -}
        isCycle a = cycleOnType (Raw.aliasNameFrom a) $ Raw.unConFromAlias a

        cycleOnType r ty = doOnUnCon ty
            (\rty -> repOf r == repOf rty)
            (\_ -> False)
            (\rty ts -> repOf r == repOf rty || any (cycleOnType r) ts)
            (\_ ts -> any (cycleOnType r) ts)

{- Given a list of aliases, it builds up a map representing the same aliases, but already expanded, namely
if there's some alias which refers to another alias, the first alias is expanded into what implement the
second. It does it by following implementations, let's make it clearer with an example:

type T = ...something...
alias A = T
alias B = A
alias C = B
alias D = C

Then, when declaration of A is encountered, all occurrences of A are searched for and replaced with
implementation of A, so the situation will be like this:

type T = ...something...
alias A = T
alias B = T     <- replaced
alias C = B
alias D = C

Same as B, each occurrence of it is replaced by its implementation, so:

type T = ...something...
alias A = T
alias B = T
alias C = T     <- replaced
alias D = C

and so on...

aliasSubst is also capable of discovering cycles among aliases and returning an error with aliases which
make up the cycle. -}
aliasSubst :: [Raw.AliasAlgebraicDataType With.ProgState] -> Either [Raw.AliasAlgebraicDataType With.ProgState] AliasMap
aliasSubst l = __aliasSubst l [] l

aliasSubstOp :: AliasMap -> Raw.AstOp With.ProgState ()
aliasSubstOp m = Raw.safeUpdateTypes substCallback $ AOFExcept [AOFAlias]
    where
        substCallback ty =
            let conts = Raw.contsFromType ty in
            let unCon = Raw.unConFromType ty in
            let newConts = map mkNewCont conts in
            let newUnCon = substCallback' unCon in
                Raw.buildType newConts newUnCon $ stateOf ty

        mkNewCont c =
            let ts = map substCallback' $ Raw.unConsFromCont c in
                Raw.buildCont (Raw.intfNameFromCont c) ts $ stateOf c

        {- This cannot return a Nothing. -}
        substCallback' ty = Raw.doOnUnCon ty
            (lookupAndReplace ty)
            (\_ -> ty)
            (\adtn _ -> lookupAndReplace ty adtn)
            (\_ _ -> ty)

        lookupAndReplace ty adtn =
            let name = repOf adtn in
            case m !? name of
                Nothing -> ty
                Just (params, bty) -> replace params name bty ty

aliasSubstitutionForDecls :: AliasMap -> Raw.Program With.ProgState -> Raw.Program With.ProgState
aliasSubstitutionForDecls m p = snd . runAstOp p $ aliasSubstOp m

{- Given a Program, it returns a new Program, namely the changed version of the old one, where all occurrences
of aliases are replaced with the implementations of those aliases (until no aliases remain). It handles also
the cases of errors which are:
    a cycle among aliases => it returns `That aliases`, where `aliases` are the aliases which build up the cycle.
    unreachable state => this can be due only to a bad implementation of Ast.Tree visit functions, it returns None. -}
aliasSubstitution :: Raw.Program With.ProgState -> AliasSubstRes (Raw.Program With.ProgState)
aliasSubstitution p = case splitAliases $ Raw.declarationsFrom p of
    (aliases, nonAliases) -> (case aliasSubst aliases of
        Left [] -> None     --This is unreachable
        Left err -> That err
        Right m -> This . aliasSubstitutionForDecls m $ Raw.buildProgram nonAliases)
