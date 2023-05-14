{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Types.Builder.Instances
    ( build
    , InstanceErr(..)
) where

import Lib.Utils
import Lib.Result
import Control.Monad.State
import Compiler.Ast.Common
import Data.Map.Strict as Map hiding (map, foldl')
import Compiler.State as With
import Compiler.Desugar.Names as Desugar
import qualified Compiler.Ast.Tree as Raw
import qualified Compiler.Ast.Typed as Ty
import qualified Compiler.Types.Lib.State as S
import Compiler.Types.Tables
import Compiler.Types.Lib.Create as Create
import qualified Compiler.Types.Lib.FreeVars as Fresh
import qualified Compiler.Types.Lib.InferKind as KInfr

data InstanceErr =
      NoProp PropConRep
    | NoProp' (Raw.IntfName With.ProgState)
    | NoInst [Raw.Constraint With.ProgState]
    | NoMethod SymbolRep (Raw.IntfName With.ProgState)
    | CreationErr (Create.Err KInfr.TypeGenErr)
    | UnreachableState String

noProp :: PropConRep -> String
noProp name = "Property name " ++ tokenRepToStr name ++ " not found in the constraints table"

noProp' :: Raw.IntfName With.ProgState -> String
noProp' name = "Property name " ++ strOf name ++ " not found in the constraints data"

noInstFor :: [Raw.Constraint With.ProgState] -> String
noInstFor cs = "No instance for:\n" ++ concat (lastmap (\c -> showContInfo c ++ ", \n") showContInfo cs)
    where
        showContInfo :: Raw.Constraint With.ProgState -> String
        showContInfo c = Raw.showCont c ++ " at " ++ show (stateOf c)

noMethod :: SymbolRep -> Raw.IntfName With.ProgState -> String
noMethod name propName =
    "Method " ++ tokenRepToStr name ++ " not implemented for property " ++ strOf propName ++ " at " ++
    show (stateOf propName)

instance InfoShow InstanceErr where
    infoShow (NoProp _) = unexpNoInfo
    infoShow (NoProp' _) = unexpNoInfo
    infoShow (NoInst cs) = noInstFor cs
    infoShow (NoMethod _ _) = unexpNoInfo
    infoShow (CreationErr (Create.ContErr err @ (Ty.KindErr _ _))) = descOf err
    infoShow (CreationErr (Create.CustomErr err)) = infoShow err
    infoShow (CreationErr _) = unexpNoInfo
    infoShow (UnreachableState _) = unexpNoInfo

instance DebugShow InstanceErr where
    dbgShow (NoProp pName) = noProp pName
    dbgShow (NoProp' pName) = noProp' pName
    dbgShow (NoInst cs) = noInstFor cs
    dbgShow (NoMethod name pName) = noMethod name pName
    dbgShow (CreationErr (Create.CustomErr err)) = dbgShow err
    dbgShow (CreationErr err) = descOf err
    dbgShow (UnreachableState reason) = reason

instance UnreachableState InstanceErr where
    isUnreachable err @ (NoProp _) = Just $ dbgShow err
    isUnreachable err @ (NoProp' _) = Just $ dbgShow err
    isUnreachable (NoInst _) = Nothing
    isUnreachable err @ (NoMethod _ _) = Just $ dbgShow err
    isUnreachable (CreationErr (Create.ContErr (Ty.KindErr _ _))) = Nothing
    isUnreachable err @ (CreationErr _) = Just $ dbgShow err
    isUnreachable err @ (UnreachableState _) = Just $ dbgShow err

{- A type to keep track of instances which have to exist, for example:

    property Bar a => Foo a b
and
    instance Foo Int y

implies the existence of:

    instance Bar Int
-}
type ContPropSignature = Raw.Signature With.ProgState
type PurePropSignature = Raw.Signature With.ProgState
type NecessaryInsts = [Raw.Constraint With.ProgState]
type PropConstraint = Raw.Constraint With.ProgState
type PropArgs = [Raw.ParamTypeName With.ProgState]
type PropsData = Map PropConRep (NecessaryInsts, PropConstraint, PropArgs, [ContPropSignature], [PurePropSignature])

type InstHandle res = S.EitherHandle PropsData InstanceErr res

type InstState = S.GenericState PropsData

putBinding :: Raw.Type With.ProgState -> Raw.SDUnion With.ProgState -> InstHandle ()
putBinding ty sd = do
    insts <- S.getInsts
    let sd' = Raw.addTypeHint sd ty
    let updInsts = addElem sd' insts
    S.putInsts updInsts

putPropVar :: Ty.NotedVar With.ProgState -> InstHandle ()
putPropVar nVar = do
    methods <- S.getMethods
    let updMethods = addElem nVar methods
    S.putMethods updMethods

putImpl :: [Ty.LangSpecConstraint With.ProgState] -> InstHandle ()
putImpl lspcs = do
    it <- S.getImpls
    let updIt = addElem lspcs it
    S.putImpls updIt

getPropsData :: InstHandle PropsData
getPropsData = S.getData

{- NB: it returns an error if it cannot find the property. -}
getPropsDataOf
    :: Raw.IntfName With.ProgState
    -> InstHandle (NecessaryInsts, PropConstraint, PropArgs, [ContPropSignature], [PurePropSignature])
getPropsDataOf pName = do
    pd <- getPropsData
    case Map.lookup (repOf pName) pd of
        Nothing -> instanceErr $ NoProp' pName
        Just res -> return res

putPropsData :: PropsData -> InstHandle ()
putPropsData = S.putData

putSigs
    :: Raw.Interface With.ProgState
    -> NecessaryInsts
    -> PropConstraint
    -> PropArgs
    -> [ContPropSignature]
    -> [PurePropSignature]
    -> InstHandle ()
putSigs p nInsts pCont pArgs sigs pureSigs = do
    pd <- getPropsData
    putPropsData $ Map.insert (repOf $ Raw.intfNameFrom p) (nInsts, pCont, pArgs, sigs, pureSigs) pd

instanceErr :: InstanceErr -> InstHandle a
instanceErr err = lift $ Left err

fetchPropConstraint :: Raw.Interface With.ProgState -> InstHandle (Raw.Constraint With.ProgState)
fetchPropConstraint p = do
    let args = argsOf p
    let unCons = map Raw.buildParamBaseUnCon args
    let pName = Raw.intfNameFrom p
    return . Raw.buildCont pName unCons $ stateOf p

fetchPropsDataFrom :: Raw.Interface With.ProgState -> InstHandle ()
fetchPropsDataFrom p = do
    let cs = Raw.contsFromIntf p
    propCont <- fetchPropConstraint p
    let pArgs = argsOf p
    let sigs = Raw.sigsFromIntf p
    sigs' <- mapM (injectConts [propCont]) sigs
    putSigs p cs propCont pArgs sigs' sigs

mkPropSym :: Raw.Signature With.ProgState -> InstHandle ()
mkPropSym sig = do
    let sn = Raw.symNameFromSig sig
    lqty <- mkQualType $ Raw.typeFromSig sig
    let nVar = Ty.newNotedVar <| repOf sn <| Ty.generalize' lqty <| stateOf sig
    putPropVar nVar

mkPropSyms :: [Raw.Signature With.ProgState] -> InstHandle ()
mkPropSyms = mapM_ mkPropSym

{- It extracts the information about properties and it builds the property noted variables from the signatures of
each property. -}
mkAllPropSyms :: InstHandle ()
mkAllPropSyms = do
    pd <- getPropsData
    let propsSigs = map (\(_, _, _, sigs, _) -> sigs) $ elems pd
    foldM_ (\_ sigs -> mkPropSyms sigs) () propsSigs

{- It changes the unconstrained type tokens in the type of a signature token according to a callback. -}
mapSig
    :: (Raw.UnConType With.ProgState -> Raw.UnConType With.ProgState)
    -> Raw.Signature With.ProgState
    -> InstHandle (Raw.Signature With.ProgState)
mapSig change sig = do
    let sn = Raw.symNameFromSig sig
    let ty = Raw.typeFromSig sig
    let uty = change $ Raw.unConFromType ty
    let conts = Raw.mapTypesInConts change $ Raw.contsFromType ty
    let ty' = Raw.buildType conts uty $ stateOf ty
    return . Raw.buildSig sn ty' $ stateOf sig

injectConts
    :: [Raw.Constraint With.ProgState]
    -> Raw.Signature With.ProgState
    -> InstHandle (Raw.Signature With.ProgState)
injectConts cs sig = return $ Raw.addContsToSig sig cs

{- It extracts information from an instance. -}
instanceInfo
    :: Raw.Instance With.ProgState
    -> InstHandle
        ( Raw.IntfName With.ProgState
        , NecessaryInsts
        , PropConstraint
        , PropArgs
        , [ContPropSignature]
        , [PurePropSignature]
        {- Instance constraints -}
        , [Raw.Constraint With.ProgState]
        {- Instance arguments -}
        , [Raw.UnConType With.ProgState]
        )
instanceInfo inst = do
    let pName = Raw.intfNameFromInst inst
    (nInsts, pCont, pArgs, sigs, pureSigs) <- getPropsDataOf pName
    let instConts = Raw.contsFromInst inst
    let instArgs = argsOf inst
    return (pName, nInsts, pCont, pArgs, sigs, pureSigs, instConts, instArgs)

kindInf
    :: TypesTable With.ProgState
    -> [Raw.UnConType With.ProgState]
    -> Raw.ParamTypeName With.ProgState
    -> Raw.UnConType With.ProgState        --Useless, just for the callback signature
    -> Either KInfr.TypeGenErr Ty.LangKind
kindInf tt ts pty _ =
    case KInfr.getKinds''' pty ts tt of
        Left err -> Left err
        Right (lk, _) -> Right lk

buildImpl
    :: Raw.Constraint With.ProgState
    {- These types are useful to do kind inference. -}
    -> [Raw.UnConType With.ProgState]
    -> InstHandle ()
buildImpl c ts = do
    {- Here there is the magic moment: the instance is turned into a typed specialized constraint token. -}
    (lspc, lspcs) <- mkTypedConstraint c ts
    putImpl $ lspc : lspcs

mkQualType :: Raw.Type With.ProgState -> InstHandle (Ty.LangQualType With.ProgState)
mkQualType rawTy = do
    tt <- S.getTypes
    ct <- S.getConts
    let uty = Raw.unConFromType rawTy
    {- For kind-inference. -}
    let ts = uty : concatMap Raw.unConsFromCont (Raw.contsFromType rawTy)
    case Create.aConstrainedType tt ct (kindInf tt ts) rawTy of
        Left err -> instanceErr $ CreationErr err
        Right lqty -> return lqty

mkTypedConstraint
    :: Raw.Constraint With.ProgState
    -> [Raw.UnConType With.ProgState]
    -> InstHandle (Ty.LangSpecConstraint With.ProgState, [Ty.LangSpecConstraint With.ProgState])
mkTypedConstraint c ts = do
    let cts = argsOf c
    let kInfTs = cts ++ ts
    tt <- S.getTypes
    ct <- S.getConts
    case Create.aConstraint tt ct (kindInf tt kInfTs) c of
        Left err -> instanceErr $ CreationErr err
        Right lspcs -> return lspcs

{- Given a signature, it builds a new noted variable and it delegates an action with that noted variable. -}
onInstBinding
    :: Raw.Signature With.ProgState
    -> Raw.Instance With.ProgState
    -> Ty.LangSpecConstraint With.ProgState
    -> (Raw.Type With.ProgState -> Raw.SDUnion With.ProgState -> InstHandle ())
    -> InstHandle ()
onInstBinding sig inst lspc insertBinding = do
    let sn = Raw.symNameFromSig sig
    let rawTy = Raw.typeFromSig sig
    let symRep = repOf sn
    sd <- getAssociatedSymDecl symRep $ Raw.symDeclsFromInst inst
    {- Making dispatch name -}
    let newSymRep = mkDispatchSuffix symRep [lspc]
    let updSd = Raw.updateSymbolName sd newSymRep
    insertBinding rawTy updSd
    where
        getAssociatedSymDecl symRep sds = do
            case firstThat (\sd -> repOf (Raw.symNameFromSD sd) == symRep) sds of
                Nothing -> instanceErr . NoMethod symRep $ Raw.intfNameFromInst inst
                Just sd -> return sd

addInstBindings
    :: Raw.Instance With.ProgState
    {- Head of property already mapped with arguments of instance. -}
    -> Raw.Constraint With.ProgState
    {- Signatures of property already mapped with arguments of instance. -}
    -> [Raw.Signature With.ProgState]
    -> InstHandle ()
addInstBindings inst pCont sigs = do
    (lspc, _) <- mkTypedConstraint pCont $ argsOf inst
    mapM_ (addBinding lspc) sigs
    where
        addBinding lspc sig = onInstBinding sig inst lspc putBinding

handleInst :: Raw.Instance With.ProgState -> InstHandle NecessaryInsts
handleInst inst = do
    (_, nInsts, propCont, propArgs, _, pureSigs, instConts, instArgs) <- instanceInfo inst
    {- List of unconstrained types, just used for kind inference. -}
    let ts = instArgs ++ concatMap argsOf instConts
    buildImpl (Raw.buildHead inst) ts
    let f = Raw.mapTypesLam propArgs instArgs
    {- Taking the type of each signatures and replacing each bound "type variable" with the associated type
    taken from the instance (the types which implement the property). -}
    instSigs <- mapM (mapSig f) pureSigs
    {- Doing the same thing with THE property constraint, in order to make dispatch names. -}
    instPropCont <- mapTypesInCont f propCont
    {- Doing the same thing with constraints of the property, in order to discover which instances have to exist. -}
    let necInsts = Raw.mapTypesInConts f nInsts
    {- Adding the constraints of the instance to the type of the signatures. -}
    contsSigs <- mapM (injectConts instConts) instSigs
    addInstBindings inst instPropCont contsSigs
    return necInsts
    where
        mapTypesInCont f c =
            case Raw.mapTypesInConts f [c] of
                [c'] -> return c'
                _ -> instanceErr $ UnreachableState "Inconsistent number of list elements"

{- The notion of match between the constraint of an instance and another constraint. It is worth noting that
the comparison is done only at a syntactic level, no typed token figures. -}
matchInst :: Raw.Instance With.ProgState -> Raw.Constraint With.ProgState -> Bool
matchInst inst cont =
    let propName = Raw.intfNameFromInst inst in
    let instArgs = argsOf inst in
    let contName = Raw.intfNameFromCont cont in
    let contArgs = argsOf cont in
        repOf propName == repOf contName &&
        matchManyTypes instArgs contArgs
    where
        matchManyTypes :: [Raw.UnConType With.ProgState] -> [Raw.UnConType With.ProgState] -> Bool
        matchManyTypes [] [] = True
        matchManyTypes [] _ = False
        matchManyTypes _ [] = False
        matchManyTypes (uty : t) (uty' : t') =
            matchTypes uty uty' &&
            matchManyTypes t t'

        matchTypes :: Raw.UnConType With.ProgState -> Raw.UnConType With.ProgState -> Bool
        matchTypes uty uty' =
            Raw.doOnUnCon uty
                (\rty -> Raw.doOnUnCon uty'
                    (\rty' -> repOf rty == repOf rty')
                    (const False)
                    (\_ _ -> False)
                    (\_ _ -> False)
                )
                (\_ -> Raw.doOnUnCon uty'
                    (const False)
                    {- How the type variable is made does not matter! The kind inference and other stuff with
                    typed tokens are not handled here. -}
                    (const True)
                    (\_ _ -> False)
                    (\_ _ -> False)
                )
                (\rty ts -> Raw.doOnUnCon uty'
                    (const False)
                    (const False)
                    (\rty' ts' ->
                        repOf rty == repOf rty' &&
                        matchManyTypes ts ts'
                    )
                    (\_ _ -> False)
                )
                (\_ ts -> Raw.doOnUnCon uty'
                    (const False)
                    (const False)
                    (\_ _ -> False)
                    {- Same as above: how the type variable is made does not matter. -}
                    (\_ ts' -> matchManyTypes ts ts')
                )

removeNecessaryInsts :: NecessaryInsts -> Raw.Instance With.ProgState -> NecessaryInsts
removeNecessaryInsts [] _ = []
removeNecessaryInsts (c : t) inst =
    if matchInst inst c
    then removeNecessaryInsts t inst
    else c : removeNecessaryInsts t inst

visitProp :: InstState -> Raw.AstOpRes With.ProgState InstanceErr InstState
visitProp st = Raw.lookupProp st fetch
    where
        fetch :: InstState -> Raw.Interface With.ProgState -> Either InstanceErr InstState
        fetch st' prop = execStateT (fetchPropsDataFrom prop) st'

symsAndImpls
    :: InstState
    -> Raw.AstOpRes With.ProgState InstanceErr
        ( InstState
        , NecessaryInsts
        )
symsAndImpls st =
    Raw.lookupInst (st, []) mkTables
    where
        mkTables
            :: (InstState, NecessaryInsts)
            -> Raw.Instance With.ProgState
            -> Either InstanceErr (InstState, NecessaryInsts)
        mkTables (st', nInsts) inst =
            case runStateT (handleInst inst) st' of
                Left err -> Left err
                {- Attaching the new necessary instances to the old ones. -}
                Right (nInsts', st'') -> Right (st'', nInsts' ++ nInsts)

propSyms
    :: InstState
    -> Raw.AstOpRes With.ProgState InstanceErr
        ( PropMethodsTable With.ProgState
        , Fresh.FV ()
        )
propSyms st = do
    st' <- addMethodsToTables
    return (S.fetchMethods st', S.fetchFV st')
    where
        addMethodsToTables :: Raw.AstOpRes With.ProgState InstanceErr InstState
        addMethodsToTables =
            case execStateT mkAllPropSyms st of
                Left err -> Raw.astOpErr err
                Right st' -> return st'

instsCheck :: NecessaryInsts -> Raw.AstOpRes With.ProgState InstanceErr ()
instsCheck nInsts = do
    leftInsts <- Raw.astOpRes $ Raw.safeLookupInst nInsts removeNecessaryInsts
    case leftInsts of
        [] -> return ()
        cs -> Raw.astOpErr $ NoInst cs

build
    :: TypesTable With.ProgState
    -> ConstraintsTable With.ProgState
    -> Fresh.FV ()
    -> Raw.AstOpRes With.ProgState InstanceErr
        ( InstsTable With.ProgState
        , PropMethodsTable With.ProgState
        , ImplTable With.ProgState
        , Fresh.FV ()
        )
build tt ct fv = do
    let st = initState
    propsSt <- visitProp st
    (instsSt, nInsts) <- symsAndImpls propsSt
    (methods, fv') <- propSyms instsSt
    instsCheck nInsts
    let insts = S.fetchInsts instsSt
    let it = S.fetchImpls instsSt
    return (insts, methods, it, fv')
    where
        initState :: InstState
        initState = S.initGenState fv tt noElems ct noElems noElems noElems noElems empty
