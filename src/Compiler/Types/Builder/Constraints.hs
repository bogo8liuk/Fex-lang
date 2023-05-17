module Compiler.Types.Builder.Constraints
    ( ContGenErr(..)
    , build
) where

import Utils.Fancy
import Utils.Data.Foldable
import Lib.Result
import Data.Map.Strict as M hiding (map, foldl', filter)
import Data.List(foldl', nubBy)
import Compiler.Ast.Common
import qualified Compiler.Ast.Tree as Raw
import qualified Compiler.Ast.Typed as Ty
import Compiler.State as With
import Compiler.Types.Tables
import qualified Compiler.Types.Lib.Create as Create
import qualified Compiler.Types.Lib.InferKind as KInfr

data ContGenErr =
      KindErr KInfr.TypeGenErr
    | InfCont [Raw.IntfName With.ProgState]
    | AmbigVar (Raw.IntfName With.ProgState, Raw.ParamTypeName With.ProgState, Raw.Signature With.ProgState)
    | NoName (Raw.IntfName With.ProgState)
    | ContCreationErr (Create.Err String)
    | UnavailableKindVar
    | UnreachableState String

infiniteContErr :: [Raw.IntfName With.ProgState]
                -> String
infiniteContErr names = "Trying to build an infinite constraint among properties " ++ showNames names
    where
        showNames = concat . lastmap (\n -> showN n ++ ", ") showN

        showN name = tokenRepToStr (repOf name) ++ " at " ++ show (stateOf name)

ambigousVarErr :: Raw.IntfName With.ProgState
               -> Raw.ParamTypeName With.ProgState
               -> Raw.Signature With.ProgState
               -> String
ambigousVarErr name pty sig = "Bound type variable ambiguity in property " ++ tokenRepToStr (repOf name) ++ ": "
    ++ tokenRepToStr (repOf pty) ++ " type variable " ++ "does not appear in signature at " ++ show (stateOf sig)

noName :: Raw.IntfName With.ProgState -> String
noName iName = "No constraint with name " ++ tokenRepToStr (repOf iName) ++ " found"

unavailKindVar :: String
unavailKindVar = "Unavailable kind variable during population of constraints table"

instance DebugShow ContGenErr where
    dbgShow (KindErr tgerr) = dbgShow tgerr
    dbgShow (InfCont names) = infiniteContErr names
    dbgShow (AmbigVar (iName, pty, sig)) = ambigousVarErr iName pty sig
    dbgShow (NoName iName) = noName iName
    dbgShow UnavailableKindVar = unavailKindVar
    dbgShow (ContCreationErr (Create.CustomErr reason)) = reason
    dbgShow (ContCreationErr err) = descOf err
    dbgShow (UnreachableState reason) = reason

instance InfoShow ContGenErr where
    infoShow (KindErr tgerr) = infoShow tgerr
    infoShow (InfCont names) = infiniteContErr names
    infoShow (AmbigVar (iName, pty, sig)) = ambigousVarErr iName pty sig
    infoShow (NoName _) = unexpNoInfo
    infoShow UnavailableKindVar = unexpNoInfo
    infoShow (ContCreationErr (Create.ContErr err @ (Ty.KindErr _ _))) = descOf err
    infoShow (ContCreationErr _) = unexpNoInfo
    infoShow (UnreachableState _) = unexpNoInfo

instance UnreachableState ContGenErr where
    isUnreachable (KindErr tgerr) = isUnreachable tgerr
    isUnreachable (InfCont _) = Nothing
    isUnreachable (AmbigVar _) = Nothing
    isUnreachable err @ (NoName _) = Just $ dbgShow err
    isUnreachable err @ UnavailableKindVar = Just $ dbgShow err
    isUnreachable (ContCreationErr (Create.ContErr (Ty.KindErr _ _))) = Nothing
    isUnreachable err @ (ContCreationErr _) = Just $ dbgShow err
    isUnreachable err @ (UnreachableState _) = Just $ dbgShow err

{- See `buildCont` for the reason why this function exists. -}
populateInitTable :: [Raw.ParamTypeName With.ProgState] -> Maybe (KInfr.KindsTable, KInfr.FV ())
populateInitTable ps =
    push ps empty KInfr.newFreeVarsContainer
    where
        push [] kt fv = Just (kt, fv)
        push (pty : t) kt fv =
            let (var, fv') = KInfr.allocFreeVar () fv in
                push t (M.insert <| repOf pty <| (Ty.newVarLKTy var, stateOf pty) <| kt) fv'

mkConstraint
    :: [(Raw.ParamTypeName With.ProgState, Ty.LangKind)]
    -> PropConRep
    -> With.ProgState
    -> Ty.LangNewConstraint With.ProgState
mkConstraint ps name =
    {- Note the use of `Ty.newLVTy` which does not includes specialized constraints in the
    building of type variables. This fact is not handled here. -}
    Ty.newLNCont name (map (\(pn, lk) -> Ty.newLVTy <| repOf pn <| lk <| Ty.Representational <| stateOf pn) ps)

checkAmbigVars
    :: [Raw.ParamTypeName With.ProgState]
    -> [Raw.Signature With.ProgState]
    -> Maybe (Raw.ParamTypeName With.ProgState, Raw.Signature With.ProgState)
checkAmbigVars ps sigs = checkVars ps sigs
    where
        checkVars _ [] = Nothing
        checkVars [] (_ : t') = checkVars ps t'
        checkVars (pty : t) sigs' =
            case foldl' (checkIfOk pty) Nothing sigs' of
                Nothing -> checkVars t sigs'
                err -> err

        checkIfOk pty Nothing sig =
            if repOf pty `elem` (map repOf . Raw.paramTNamesFromType $ Raw.typeFromSig sig)
            then Nothing
            else Just (pty, sig)
        checkIfOk _ err _ = err

{- TODO: instead of a chain of pattern matching constructs, this code can be written in a monadic way. -}
{- It builds and, if successful, adds a new LangNewConstraint value in a constraints table. -}
buildCont
    :: TypesTable With.ProgState
    -> ConstraintsTable With.ProgState
    -> Raw.Interface With.ProgState
    -> Either ContGenErr (ConstraintsTable With.ProgState)
buildCont tt ct intf =
    let pName = Raw.intfNameFrom intf in
    let sigs = Raw.sigsFromIntf intf in
    let ps = Raw.paramTNamesFromIntf intf in
        {- Checking the presence of ambiguity of property bound variables. -}
        case checkAmbigVars ps sigs of
            Just (pty, sig) -> Left $ AmbigVar (pName, pty, sig)
            Nothing ->
                {- It is necessary to populate the table with property bound type variables,
                because if there are no signatures along with the property, the kind table
                remains empty and in the constraint building phase fails because it can't
                find information about bound type variables. -}
                case populateInitTable ps of
                    Nothing -> Left UnavailableKindVar
                    Just (kt, fv) ->
                        {- First, the kinds of property bound variables have to be inferred. -}
                        case foldl' <| inferFrom (map (Raw.unConFromType . Raw.typeFromSig) sigs)
                                    <| Right ([], kt, fv)
                                    <| ps of
                            Left err -> Left $ KindErr err
                            Right (kps, _, _) ->
                                {- With foldl', ParamTypeName values `ps` have been reversed, so using
                                reverse function to restore the previous order. -}
                                let lncont = mkConstraint <| reverse kps <| repOf pName <| stateOf intf in
                                    Right $ addElem lncont ct
        where
            inferFrom _ err @ (Left _) _ = err
            inferFrom ts (Right (kps, kt, fv)) pty =
                case KInfr.getKinds pty ts tt kt fv of
                    Left err -> Left err
                    {- Returning new kinds table and fvars container, in order to use them later. -}
                    Right (lk, _, kt', fv') -> Right ((pty, lk) : kps, kt', fv')

updateTyVars
    :: TypesTable With.ProgState
    -> ConstraintsTable With.ProgState
    -> Raw.Interface With.ProgState
    -> Either ContGenErr (ConstraintsTable With.ProgState)
updateTyVars tt ct prop =
    let pName = Raw.intfNameFrom prop in
    let pNameRep = repOf pName in
        case findCont pNameRep ct of
            Nothing -> Left $ NoName pName
            Just (lnc, _) ->
                case mkConts lnc $ Raw.contsFromIntf prop of
                    Left err -> Left err
                    Right cs -> Right $ kValUpdate' pNameRep cs ct
        where
            findCont
                :: PropConRep
                -> ConstraintsTable With.ProgState
                -> Maybe (Ty.LangNewConstraint With.ProgState, [Ty.LangSpecConstraint With.ProgState])
            findCont = kFind

            mkConts
                :: Ty.LangNewConstraint With.ProgState
                -> [Raw.Constraint With.ProgState]
                -> Either ContGenErr [Ty.LangSpecConstraint With.ProgState]
            mkConts lnc cs = foldl' <| mkC lnc <| Right [] <| cs

            mkC
                :: Ty.LangNewConstraint With.ProgState
                -> Either ContGenErr [Ty.LangSpecConstraint With.ProgState]
                -> Raw.Constraint With.ProgState
                -> Either ContGenErr [Ty.LangSpecConstraint With.ProgState]
            mkC lnc (Right lscs) c =
                case Create.aConstraint tt ct (fetchKind lnc) c of
                    Left err -> Left $ ContCreationErr err
                    Right (fstLsc, lscs') -> Right (fstLsc : lscs' ++ lscs)
            mkC _ err _ = err

            fetchKind
                :: Ty.LangNewConstraint With.ProgState
                -> Raw.ParamTypeName With.ProgState
                -> Raw.UnConType With.ProgState
                -> Either String Ty.LangKind
            fetchKind lnc pty _ =
                KInfr.getKindsFromCont pty lnc "Cannot infer kinds during type variables update in constraints building"

{- This is a hack: the algorithm of constraints building does not actually care of constraints on
bound type variables of a property, because to build a specialized constraint it's necessary to have
a constraints table which cannot be available during the execution of the algorithm. So, the
specialized constraints get added afterwards, by updating bound type variables (in typed token form)
in the constraints table. -}
addSpecConts
    :: TypesTable With.ProgState
    -> ConstraintsTable With.ProgState
    -> Raw.AstOpRes With.ProgState ContGenErr (ConstraintsTable With.ProgState)
addSpecConts tt ct = Raw.lookupProp ct $ updateTyVars tt

{- The property name as first component in the tuple is just the name which the key is the string
representation of. This is just useful for backtracking. TODO: implement Ord for names tokens and
change the map. -}
type CyclesMap = Map PropConRep (Raw.IntfName With.ProgState, [Raw.IntfName With.ProgState])

{- It checks for the presence of cycles among constraints of properties. -}
checkCycles :: Raw.AstOpRes With.ProgState ContGenErr CyclesMap
checkCycles = Raw.lookupProp empty propCycle
    where
        propCycle
            :: CyclesMap
            -> Raw.Interface With.ProgState
            -> Either ContGenErr CyclesMap
        propCycle m p =
            let pName = Raw.intfNameFrom p in
            let names = map headOf $ Raw.contsFromIntf p in
            let m' = replace pName (nubBy (\pn pn' -> repOf pn == repOf pn') $ getReplace names m) m in
                if pName `isCycle` m'
                then Left . InfCont $ backTrack pName m'
                else Right m'

        getReplace [] _ = []
        getReplace (n : t) m =
            case M.lookup (repOf n) m of
                Nothing -> n : getReplace t m
                Just (_, names) -> names ++ getReplace t m

        replace pName names m =
            insert (repOf pName) (pName, names) m

        isCycle pName m =
            let name = repOf pName in
                case M.lookup name m of
                    Nothing -> False
                    Just (_, names) -> name `elem` map repOf names

        {- It backtracks the map to find properties which forms the cycle. -}
        backTrack pName m =
            fltmap (\(_, (n, names)) ->
                if repOf pName `elem` map repOf names
                then Just n
                else Nothing) $ toList m

build :: TypesTable With.ProgState -> Raw.AstOpRes With.ProgState ContGenErr (ConstraintsTable With.ProgState)
build tt = do
    checkCycles
    ct <- Raw.lookupProp noElems $ buildCont tt
    addSpecConts tt ct
