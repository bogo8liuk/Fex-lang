module Compiler.Desugar.Alias
    ( AliasErr
    , substitution
) where

import Utils.Fancy
import Utils.Data.Foldable
import Lib.Result
import Data.List.NonEmpty as NEList hiding (map, (<|))
import Data.Map.Strict as Map hiding (map)
import Data.Semigroup
import Control.Monad.Trans.Class(lift)
import Compiler.Ast.Common
import Compiler.Ast.Tree as Raw
import qualified Compiler.State as With

type AliasMap = Map TyConRep ([Raw.ParamTypeName With.ProgState], Raw.UnConType With.ProgState)

newtype AliasErr =
      Cycle [Raw.AliasAlgebraicDataType With.ProgState]

instance InfoShow AliasErr where
    infoShow (Cycle aliases) =
        "A cycle among the following aliases has been found:" ++ concatMap showAlias aliases
        where
            showAlias alias =
                "\n" ++ tokenRepToStr (repOf $ Raw.aliasNameFrom alias) ++ " defined at " ++ show (stateOf alias)

instance DebugShow AliasErr where
    dbgShow = infoShow

instance UnreachableState AliasErr where
    isUnreachable = Just . dbgShow

{- `expandTypes ps tys ty` replaces the i-th parametric type of `ps` with the i-th type in `tys`
in the Type `ty` (so the elements of `ps` are searched for in `ty`).
NB: `ty` is treated as an accumulator, namely it is changed in-place and then it is returned. -}
expandTypes
    :: [Raw.ParamTypeName With.ProgState]
    -> [Raw.UnConType With.ProgState]
    -> Raw.UnConType With.ProgState
    -> Raw.UnConType With.ProgState
expandTypes [] _ ty = ty
expandTypes _ [] ty = ty --Unreachable
expandTypes (par : t) (ty' : t') ty =
    expandTypes t t' $ Raw.doOnUnCon ty
        {- With a singleton concrete type, there's nothing to expand. -}
        (const ty)
        (\p ->
            if repOf p == repOf par
            then ty'
            else ty
        )
        {- NB: in the rec call, the lists are passed and not reduced, but this won't cause an infinite loop because
        sooner or later, it will arrive to the non-rec case (the first two callbacks passed to `doOnUnCon`). -}
        (\a ts ->
            let ts' = map (expandTypes (par : t) (ty' : t')) ts in
                Raw.buildRealCompUnCon a ts' $ stateOf ty
        )
        (\p ts ->
            let ts' = map (expandTypes (par : t) (ty' : t')) ts in
                if repOf p == repOf par
                {- The parametric type `p` has also params (`ts`), thus, besides the substitution of `p` with type `ty`,
                it is also necessary that the new type `ty` gets the old params of `p` (but changed by the rec call). -}
                then sconcat (ty' :| ts')
                else Raw.buildParamCompUnCon p ts' $ stateOf ty
        )

{- Given a list of paramtetric types `params`, a `name` representing an adt name, a type `bty` and another
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
replace
    :: [Raw.ParamTypeName With.ProgState]
    -> TyConRep
    -> Raw.UnConType With.ProgState
    -> Raw.UnConType With.ProgState
    -> Raw.UnConType With.ProgState
replace params name bty ty =
    Raw.doOnUnCon ty
        (\adt ->
            if name == repOf adt
            then bty
            else ty
        )
        {- With a singleton parametric type, there's nothing to replace since a parametric type name cannot be equal
        to an alias name. -}
        (const ty)
        {- This works under the hypothesis that the args number check has already been performed. -}
        (\adt ts ->
            {- Recursive calls: performing replacing on arguments. -}
            let ts' = map (replace params name bty) ts in
            let st = stateOf ty in
                {- If the name of the type (`adt`) is the same of the one we are looking for (`name`), then into the
                arguments (`ts'`) - which are supposed to contain the parameters `params` - `params` have to be
                substituted with the arguments of `ty` (expanding the type). -}
                if name == repOf adt
                then expandTypes params ts' bty
                else Raw.buildRealCompUnCon adt ts' st
        )
        (\par ts ->
            {- Recursive calls: performing replacing on arguments. -}
            let ts' = map (replace params name bty) ts in
                Raw.buildParamCompUnCon par ts' $ stateOf ty
        )

{- Given an `alias` and an `alias'`, it substitutes each occurrence of `alias` name in implementation of `alias'` with
the type which implements `alias`. -}
replaceAlias
    :: Raw.AliasAlgebraicDataType With.ProgState
    -> Raw.AliasAlgebraicDataType With.ProgState
    -> Raw.AliasAlgebraicDataType With.ProgState
replaceAlias alias alias' =
    {- Here, the scheme of what is used

       type A  a1 a2 a3 =  T ...
            |  |  |  |    |     |
         +--+  +--+--+    +--+--+
         |        |          |
         V        V          V
        name   params       bty

       type B  b1 b2 b3 = P ...
                         |     |
                         +--+--+
                            |
                            V
                            ty
    -}
    let params = argsOf alias in
    let name = repOf $ Raw.aliasNameFrom alias in
    let aliasSt = stateOf alias' in
    let adtDecl = Raw.adtDeclareFromAlias alias' in
    let bty = Raw.unConFromAlias alias in
    let ty = Raw.unConFromAlias alias' in
        Raw.buildAlias adtDecl (replace params name bty ty) aliasSt

{- `subst a as` replaces every occurrence of the name of alias `a` with the name of the type which implements `a` in
all aliases `as`. -}
subst
    :: Raw.AliasAlgebraicDataType With.ProgState
    -> [Raw.AliasAlgebraicDataType With.ProgState]
    -> [Raw.AliasAlgebraicDataType With.ProgState]
subst alias aliases =
    fromLastToFst aliases replaceAlias' `accumulatingIn` []
    where
        replaceAlias' oldAlias as =
            let newAlias = replaceAlias alias oldAlias in
                newAlias : as

{- `findCycleOfAlias alias as` looks for an aliases cycle involving `alias` in the list `as`. If it can find the cycle,
it returns the list which builds up the cycle. The algorithm works like this:
    1) accumulate a potential cycle;
    2) if there is a cycle in `alias`, it is returned with the accumulated cycle;
    3) else, if an alias (we call it `newAlias`) which has an implementation type where the `alias`'s name occurs,
       go to 2) with `newAlias` as `alias` parameter, accumulating the old `alias` in the cycle;
    4) otherwise, there is no cycle.
-}
findCycleOfAlias
    :: Raw.AliasAlgebraicDataType With.ProgState
    -> [Raw.AliasAlgebraicDataType With.ProgState]
    -> Maybe [Raw.AliasAlgebraicDataType With.ProgState]
findCycleOfAlias alias aliases =
    findCycleOf (Raw.aliasNameFrom alias) `accumulatingIn` []
    where
        aliasUty = Raw.unConFromAlias alias

        findCycleOf alName curCycle =
            if alName `hasTypeName` aliasUty
            then Just curCycle
            else
                case firstThat (hasTypeName alName . Raw.unConFromAlias) aliases of
                    Nothing -> Nothing
                    {- Searching for the new alias name. -}
                    Just al -> findCycleOf <| Raw.aliasNameFrom al <| al : curCycle

        hasTypeName alName uty =
            let types = Raw.realTypes' uty in
                any (\alName' -> repOf alName' == repOf alName) types

{- Given a list of aliases which contains a cycle, it returns the list of only aliases which makes the cycle.
NB: it's supposed that a cycle really exists. It returns an empty list if it cannot find the cycle. -}
getCycle
    :: [Raw.AliasAlgebraicDataType With.ProgState]
    -> [Raw.AliasAlgebraicDataType With.ProgState]
getCycle aliases =
    try'
        (fromFstToLast aliases lookForCycle `startingFrom` Nothing)
    `else'`
        []    --Unreachable, if there is really a cycle
    where
        lookForCycle res @ (Just _) _ = res
        lookForCycle Nothing alias =
            findCycleOfAlias alias aliases

buildMap :: [Raw.AliasAlgebraicDataType With.ProgState] -> AliasMap
buildMap aliases =
    fromFstToLast aliases buildMap' `startingFrom` empty
    where
        buildMap' table alias =
            Map.insert (repOf $ Raw.aliasNameFrom alias) (argsOf alias, Raw.unConFromAlias alias) table

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
mkAliasMap :: [Raw.AliasAlgebraicDataType With.ProgState] -> Either AliasErr AliasMap
mkAliasMap aliases =
    aliasSubst' aliases []
    where
        aliasSubst'
            {- Aliases to visit list -}
            :: [Raw.AliasAlgebraicDataType With.ProgState]
            {- Visited aliases list: this is kept in order to update all aliases while they are visited. -}
            -> [Raw.AliasAlgebraicDataType With.ProgState]
            -> Either AliasErr AliasMap
        aliasSubst' [] visited = Right $ buildMap visited
        {- How can it say there is a cycle just from one alias token? The alias replacing takes place also on
        the sublist `t`, so when a new alias token is analyzed, it is exptected that previous aliases have been
        already replaced in it. -}
        aliasSubst' (alias : toVisit) visited =
            let toVisit' = subst alias toVisit in
            let visited' = subst alias visited in
                if isCycle alias
                then Left . Cycle $ getCycle aliases
                else aliasSubst' toVisit' (alias : visited')

        {- Affirming there's a cycle is quite easy: the name of an alias occurs in the type of its definition. This
        is true since updates are perfomed on visited aliases and aliases to visit as well. The updates make substitutions
        on aliases.
        NB: this function just says there's a cycle, it does not find it. -}
        isCycle a = cycleOnType (Raw.aliasNameFrom a) $ Raw.unConFromAlias a

        cycleOnType r ty =
            doOnUnCon ty
                (\rty -> repOf r == repOf rty)
                (const False)
                (\rty ts -> repOf r == repOf rty || any (cycleOnType r) ts)
                (\_ ts -> any (cycleOnType r) ts)

mkAliasMapOp :: [Raw.AliasAlgebraicDataType With.ProgState] -> Raw.AstOpRes With.ProgState AliasErr AliasMap
mkAliasMapOp = lift . mkAliasMap

aliasSubstOp :: AliasMap -> Raw.AstOpRes With.ProgState err ()
aliasSubstOp m = astOpRes . Raw.safeUpdateTypes substCallback $ AOFExcept [AOFAlias]
    where
        substCallback ty =
            let conts = Raw.contsFromType ty in
            let unCon = Raw.unConFromType ty in
            {- Applying the substitution to the constraints as well. -}
            let newConts = map mkNewCont conts in
            let newUnCon = substCallback' unCon in
                Raw.buildType newConts newUnCon $ stateOf ty

        mkNewCont c =
            let ts = map substCallback' $ Raw.unConsFromCont c in
                Raw.buildCont (Raw.intfNameFromCont c) ts $ stateOf c

        {- This cannot return a Nothing. -}
        substCallback' ty =
            Raw.doOnUnCon ty
                (lookupAndReplace ty)
                (const ty)
                (\adtn _ -> lookupAndReplace ty adtn)
                (\_ _ -> ty)

        lookupAndReplace ty adtn =
            let name = repOf adtn in
                case Map.lookup name m of
                    Nothing -> ty
                    Just (params, bty) -> replace params name bty ty

aliasSubstitutionOp :: Raw.AstOpRes With.ProgState AliasErr ()
aliasSubstitutionOp = do
    aliases <- removeAlias
    table <- mkAliasMapOp aliases
    aliasSubstOp table

{- Given a Program, it returns a new Program, namely the changed version of the old one, where all occurrences
of aliases are replaced with the implementations of those aliases (until no aliases remain). It handles also
the cases of errors which are:
    - a cycle among aliases
-}
substitution :: Raw.Program With.ProgState -> Either AliasErr (Raw.Program With.ProgState)
substitution p = Raw.execAstOpRes p aliasSubstitutionOp
