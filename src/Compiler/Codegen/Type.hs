{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

{- This module is responsible to generate the corresponding Core types tokens from the language ones. -}

module Compiler.Codegen.Type
    ( monoTyGen
    , polyTyGen
    , valueGen
    , getExprCoreType
) where

import Utils.Fancy
import Utils.Data.Foldable
import Data.Array(assocs)
import Data.String(fromString)
import Compiler.Codegen.Lib
import Compiler.Codegen.Env
import Compiler.State as With
import Compiler.Ast.Common as Ast
import qualified Compiler.Config.Types as BITy
import qualified Compiler.Ast.Typed as Ty
import Type
import TysWiredIn
import TyCon
import DataCon as DC
import Literal
import MkId(mkDataConWorkId)
import Var
import BasicTypes
import Name(Name)
import ForeignCall(CType)

kindGen :: Ty.LangKind -> CodegenEnv Kind
kindGen Ty.LKConst = return liftedTypeKind
kindGen (Ty.SubLK []) = cgErr $ TypePanicErr "Malformed type-to-type kind: no types to apply"
kindGen (Ty.SubLK [_]) = cgErr $ TypePanicErr "Malformed type-to-type kind: just one type to apply"
kindGen (Ty.SubLK [lk1, lk2]) = do
    k1 <- kindGen lk1
    k2 <- kindGen lk2
    return $ k1 `mkVisFunTy` k2
{- Why not using mkVisFunTys? Because its implementation uses foldr and the single Type value (the 2nd parameter)
would be so put as last. It is quite uncomfortable to select the last kind in the list. -}
kindGen (Ty.SubLK (lk : lks)) = do
    k <- kindGen lk
    ks <- kindGen $ Ty.SubLK lks
    return $ k `mkVisFunTy` ks
kindGen (Ty.LKVar _) = cgErr $ TypePanicErr "Kind variables not allowed in type building"

tyVarGen :: Ty.LangVarType With.ProgState -> CodegenEnv TyVar
tyVarGen lvty = do
    let varRep = repOf lvty
    let st = stateOf lvty
    name <- mkTypeName varRep st
    kind <- kindGen $ Ty.kindOf lvty
    return $ mkTyVar name kind

getTyConRepName :: TyCon -> CodegenEnv TyConRepName
getTyConRepName tc =
    case tyConRepName_maybe tc of
        Nothing -> cgErr $ TypePanicErr "Cannot infer TyConRepName value from TyCon value"
        Just name -> return name

{- It searches for a type constructor in the cache (of the state), if it can finds it, it returns it directly; if
it cannot find it, it builds a new TyCon value and it stores it in the cache. -}
cacheTyCon :: Ty.LangNewType With.ProgState -> TyConRep -> CodegenEnv TyCon
cacheTyCon lnty tyConRep = do
    mayTyCon <- getTyCon tyConRep
    case mayTyCon of
        Nothing -> do
            {- Before return, caching the type constructor. -}
            tyCon <- algTyConGen lnty
            putTyCon tyConRep tyCon
            return tyCon
        Just tyCon ->
            return tyCon

{- NB: It does not check the NotedVal value is not a literal. -}
cacheDataCon :: Ast.DataConRep -> Ty.LangNewType With.ProgState -> CodegenEnv DataCon
cacheDataCon dataConRep lnty = do
    let tyConRep = repOf lnty
    mayDataCon <- getDataCon tyConRep dataConRep
    case mayDataCon of
        Nothing -> do
            dataConsInfo <- algDataConsGen lnty
            putDataCons tyConRep dataConsInfo
            case firstThat (\(nValRep, _) -> nValRep == dataConRep) dataConsInfo of
                Nothing -> cgErr . TypePanicErr $
                    "No data constructor found with name " ++ tokenRepToStr dataConRep
                Just (_, dataCon) -> return dataCon
        Just dataCon ->
            return dataCon

{- Instead of a tuple, return this -}
data TyConBuildParams =
    TyConParams
        { tyconName :: Name
        , tyconBinders :: [TyConBinder]
        , tyconKind :: Kind
        , tyconRoles :: [Role]
        , tyconCType :: Maybe CType
        , tyconPreds :: [PredType]
        , tyconFlavour :: AlgTyConFlav
        , tyconIsGADTSyn :: Bool
        }

{- Instead of a tuple, return this -}
data DataConBuildParams =
    DataConParams
        { dataconStrRep :: Ast.DataConRep
        , dataconName :: Name
        , dataconIsInfix :: Bool
        , dataconBang :: [HsSrcBang]
        , dataconFields :: [FieldLabel]
        , dataconUniv :: [TyVar]
        , dataconExist :: [TyCoVar]
        , dataconBinders :: [TyVarBinder]
        , dataconGADTEqs :: [EqSpec]
        , dataconTheta :: ThetaType
        , dataconArgs :: [Type]
        , dataconResInfo :: ([TyVar], [Type])
        , dataconRRInfo :: RuntimeRepInfo
        , dataconTag :: ConTag
        , dataconPreds :: ThetaType
        , dataconWorkerName :: Name
        , dataconRep :: DC.DataConRep
        }

mkTyConParams :: Ty.LangNewType With.ProgState -> CodegenEnv TyConBuildParams
mkTyConParams lnty = do
    let nameRep = repOf lnty
    let st = stateOf lnty
    let args = argsOf lnty
    tyVars <- mapM tyVarGen args
    name <- mkTypeName nameRep st
    --TODO: in case the language allows kind-hinting, using the `Inferred` value constantly is wrong
    let binders = mkNamedTyConBinders Inferred tyVars
    kind <- kindGen $ Ty.kindOf lnty
    let roles = map Ty.roleOf args
    --TODO: not sure about this
    let cType = Nothing
    --TODO: using name???
    let flavour = VanillaAlgTyCon name
    let isGadtSyn = False
    return $ TyConParams
        { tyconName = name
        , tyconBinders = binders
        , tyconKind = kind
        , tyconRoles = roles
        , tyconCType = cType
        , tyconPreds = []
        , tyconFlavour = flavour
        , tyconIsGADTSyn = isGadtSyn
        }

getMonoTypeOf :: Ty.LangTypeScheme With.ProgState -> CodegenEnv (Ty.LangHigherType With.ProgState)
getMonoTypeOf lpty =
    case Ty.instantiateUnqualifying lpty [] of
        Nothing -> constraintsOnlyTypeErr
        Just lhty -> return lhty

mkResType :: TyCon -> ([TyVar], [Type]) -> Type
mkResType tyCon (binders, argsTs) =
    let headTy = mkTyConTy tyCon in
    let resTy = mkAppTys headTy argsTs in
        mkInvForAllTys binders resTy

mkResInfo :: Ty.LangTypeScheme With.ProgState -> CodegenEnv ([TyVar], [Type])
mkResInfo polyTy = do
    let bs = Ty.bVarsOf polyTy
    binders <- mapM tyVarGen bs
    monoTy <- getMonoTypeOf polyTy
    argsTs <- mapM monoTyGen $ argsOf monoTy
    return (binders, argsTs)

mkDataConParams :: Ty.NotedVal With.ProgState -> ConTag -> CodegenEnv DataConBuildParams
mkDataConParams nVal tag = do
    let valRep = repOf nVal
    let st = stateOf nVal
    let n = Ty.nValArgsNumber nVal
    name <- mkDataConName valRep st
    --TODO: really not sure about worker name
    wkrName <- mkDataConName valRep st
    let ts = Ty.unfoldTypeScheme $ Ty.typeOf nVal
    (argsTs, resTy) <- argsAndResTs ts
    resInfo <- mkResInfo resTy
    hsArgsTs <- mapM polyTyGen argsTs
    univTyVars <- mapM tyVarGen $ Ty.tyVarsOf resTy
    userTyVars <- mapM tyVarGen $ Ty.tyVarsOfMany argsTs
    let binders = mkTyVarBinders Inferred userTyVars
    return $ DataConParams
        { dataconStrRep = repOf nVal
        , dataconName = name
        , dataconIsInfix = False
        , dataconBang = replicate n $ HsSrcBang (SourceText $ tokenRepToStr valRep) NoSrcUnpack NoSrcStrict
         --TODO: this must not be empty anymore when records will be added to the language
        , dataconFields = []
        , dataconUniv = univTyVars
        , dataconExist = []
        , dataconBinders = binders
        , dataconGADTEqs = []
        , dataconTheta = []
        , dataconArgs = hsArgsTs
        , dataconResInfo = resInfo
         --TODO: not sure about this
        , dataconRRInfo = NoRRI
        , dataconTag = tag
        , dataconPreds = []
        , dataconWorkerName = wkrName
         --TODO: the alternative should be a wrapper, is it right like this???
        , dataconRep = NoDataConRep
        }
    where
        argsAndResTs ts =
            case headsAndLast ts of
                Nothing -> cgErr $ TypePanicErr "No result type from data constructor"
                Just res -> return res

mkDataConsParams :: [(Ty.NotedVal With.ProgState, ConTag)] -> CodegenEnv [DataConBuildParams]
mkDataConsParams = mapM $ uncurry mkDataConParams

algTyConInfoGenFrom
    :: Ty.LangNewType With.ProgState
    -> CodegenEnv (TyCon, [(Ast.DataConRep, DataCon)])
algTyConInfoGenFrom lnty = do
    let nameRep = repOf lnty
    nVals <- getMatchNVals nameRep
    let tagdNVals = zip nVals [fIRST_TAG..]
    dataConsParams <- mkDataConsParams tagdNVals
    tyConParams <- mkTyConParams lnty
    rec
        { let tyCon =
               mkAlgTyCon
                (tyconName tyConParams)
                (tyconBinders tyConParams)
                (tyconKind tyConParams)
                (tyconRoles tyConParams)
                (tyconCType tyConParams)
                (tyconPreds tyConParams)
                DataTyCon
                    { data_cons = map snd dataConsInfo
                    , data_cons_size = length dataConsInfo
                    , is_enum = not $ null dataConsInfo
                    }
                (tyconFlavour tyConParams)
                (tyconIsGADTSyn tyConParams)
        ; tyConRepName <- getTyConRepName tyCon
        ; let dataConsInfo =
               map
                (\dataConParams ->
                   let dataCon =
                        mkDataCon
                            (dataconName dataConParams)
                            (dataconIsInfix dataConParams)
                            tyConRepName
                            (dataconBang dataConParams)
                            (dataconFields dataConParams)
                            (dataconUniv dataConParams)
                            (dataconExist dataConParams)
                            (dataconBinders dataConParams)
                            (dataconGADTEqs dataConParams)
                            (dataconTheta dataConParams)
                            (dataconArgs dataConParams)
                            (mkResType tyCon $ dataconResInfo dataConParams)
                            (dataconRRInfo dataConParams)
                            tyCon
                            (dataconTag dataConParams)
                            (dataconPreds dataConParams)
                            (mkDataConWorkId (dataconWorkerName dataConParams) dataCon)
                            (dataconRep dataConParams)
                    in (dataconStrRep dataConParams, dataCon)
                ) dataConsParams
        }
    return (tyCon, dataConsInfo)

algTyConGen :: Ty.LangNewType With.ProgState -> CodegenEnv TyCon
algTyConGen lnty = do
    (tyCon, _) <- algTyConInfoGenFrom lnty
    return tyCon

algDataConsGen :: Ty.LangNewType With.ProgState -> CodegenEnv [(Ast.DataConRep, DataCon)]
algDataConsGen lnty = do
    (_, dataConsInfo) <- algTyConInfoGenFrom lnty
    return dataConsInfo

tyConGen :: Ty.LangNewType With.ProgState -> CodegenEnv TyCon
tyConGen lnty =
    let tyRep = repOf lnty in
    let tuplesNamesList = assocs BITy.namesTuple in
        if tyRep == BITy.nameBool
        then return boolTyCon
        else if tyRep == BITy.nameChar
        then return charTyCon
        {- Compiling natural numbers as integers; it's not up to this module to check a natural number is unsigned. -}
        else if tyRep == BITy.nameInt || tyRep == BITy.nameNat
        then return intTyCon
        else if tyRep == BITy.nameDouble
        then return floatTyCon
        else if tyRep == BITy.nameList
        then return listTyCon
        else if tyRep == BITy.nameFunctionApp
        then cgErr $ TypePanicErr "Function type constructor should not exist at Core generation level"
        else case firstThat (\(_, tyTupleRep) -> tyRep == tyTupleRep) tuplesNamesList of
            Just (ar, _) -> return $ tupleTyCon Boxed ar
            Nothing -> cacheTyCon lnty tyRep

monoTyGen :: Ty.LangHigherType With.ProgState -> CodegenEnv Type
monoTyGen lhty =
    Ty.doOnType lhty
        genConcrete
        genFun
        genTyVar
    where
        genConcrete lspty = do
            let lnty = headOf lspty
            tryGenString lnty
                `else'` do
                    tyCon <- tyConGen lnty
                    let headTy = mkTyConTy tyCon
                    let args = argsOf lspty
                    argsTs <- mapM monoTyGen args
                    return $ mkAppTys headTy argsTs

        {- TODO: actually the byte string type is NOT a byte string, but simply a list of characters. -}
        tryGenString lnty cont = do
            if repOf lnty == BITy.nameByteString
            then return stringTy
            else cont

        --TODO: what to do if it necessary to build types like (a ->) or (->)??? Answer: look at `funTyCon` in `TysPrim`
        -- module
        genFun [lhty1, lhty2] = do
            ty1 <- monoTyGen lhty1
            ty2 <- monoTyGen lhty2
            return $ mkVisFunTy ty1 ty2
        genFun _ = cgErr $ TypePanicErr "Illegal number of arguments of function type (different than 2)"

        genTyVar lvcty = do
            let lvty = headOf lvcty
            tyVar <- tyVarGen lvty
            let headTy = mkTyVarTy tyVar
            let args = argsOf lvcty
            argsTs <- mapM monoTyGen args
            return $ mkAppTys headTy argsTs

constraintsOnlyTypeErr :: CodegenEnv a
constraintsOnlyTypeErr =
    cgErr $ TypePanicErr "Constraints-only qualified type in type generation"

polyTyGen :: Ty.LangTypeScheme With.ProgState -> CodegenEnv Type
polyTyGen (Ty.Forall _ (Ty.OnlyConstraints _ _)) = constraintsOnlyTypeErr
polyTyGen (Ty.Forall bs (Ty.Qual _ lhty)) = do
    binders <- mapM tyVarGen bs
    monoTy <- monoTyGen lhty
    --TODO: really not sure on `mkInvForAllTys`
    return $ mkInvForAllTys binders monoTy

dataConGen :: Ast.DataConRep -> Ty.LangHigherType With.ProgState -> ProgState -> CodegenEnv DataCon
dataConGen conRep lhty _
    | conRep == BITy.trueConBool = return trueDataCon
    | conRep == BITy.falseConBool = return falseDataCon
    | conRep == BITy.emptyConList = return nilDataCon
    | conRep == BITy.consConList = return consDataCon
    | otherwise =
        case firstThat (\(_, conTupleRep) -> conRep == conTupleRep) BITy.conTuplesList of
            Just (ar, _) -> return $ tupleDataCon Boxed ar
            Nothing ->
                case last' $ Ty.unfoldType lhty of
                    Nothing -> cgErr $ TypePanicErr "Cannot fetch the last type of a constructor"
                    Just finLhty ->
                        Ty.doOnType finLhty
                            (\lspty -> do
                                let lnty = headOf lspty
                                cacheDataCon conRep lnty
                            )
                            (\_ -> cgErr $ TypePanicErr
                                "Serious error: function type cannot be the final type of any token"
                            )
                            (\_ -> cgErr $ TypePanicErr
                                "The head of a data con. final type cannot be a type variable"
                            )

literalGen :: Ty.NotedLiteral -> Ty.LangHigherType With.ProgState -> ProgState -> CodegenEnv Literal
literalGen (Ty.LitInt n) lhty _ = do
    ty <- monoTyGen lhty
    return $ LitNumber LitNumInt (toInteger n) ty
literalGen (Ty.LitChar c) _ _ = return $ LitChar c
literalGen (Ty.LitDouble d) _ _ = return . LitDouble $ toRational d
literalGen (Ty.LitString s) _ _ = return . LitString $ fromString s

valueGen
    :: Ty.NotedVal With.ProgState
    -> (DataCon -> CodegenEnv a)
    -> (Literal -> CodegenEnv a)
    -> CodegenEnv a
valueGen nVal withCon withLit =
    Ty.doOnVal nVal
        (\valRep lpty st -> do
            lhty <- monoTypeFrom lpty
            dataCon <- dataConGen valRep lhty st
            withCon dataCon
        )
        (\l lpty st -> do
            lhty <- monoTypeFrom lpty
            lit <- literalGen l lhty st
            withLit lit
        )

monoTypeFrom :: Ty.LangTypeScheme With.ProgState -> CodegenEnv (Ty.LangHigherType With.ProgState)
monoTypeFrom lpty = maybe constraintsOnlyTypeErr return $ Ty.instantiateUnqualifying lpty []

getTypeOf :: Ty.HasType tok => tok With.ProgState -> CodegenEnv (Ty.LangTypeScheme With.ProgState)
getTypeOf = return . Ty.typeOf

getExprCoreType :: Ty.NotedExpr With.ProgState -> CodegenEnv Type
getExprCoreType ne = do
    lpty <- getTypeOf ne
    polyTyGen lpty
