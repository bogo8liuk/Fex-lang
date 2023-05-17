module Compiler.Types.Builder.Cons
    ( ConsBuildError(..)
    , build
) where

import Lib.Result
import Utils.Fancy
import Utils.Data.Foldable
import Data.List
import Data.Semigroup(sconcat)
import Data.Map.Strict as M hiding (map, foldl')
import Compiler.Ast.Common
import qualified Compiler.Ast.Tree as Raw
import qualified Compiler.Ast.Typed as Ty
import Compiler.Types.Tables
import qualified Compiler.State as With
import qualified Compiler.Types.Lib.Create as Create
import Compiler.Types.Lib.Error
import qualified Compiler.Types.Lib.FreeVars as Fresh

data ConsBuildError =
      Unexpected String
    | CannotBindTyVar
    | TypeCreationErr (Create.Err String)

unavailableTypeVar :: String
unavailableTypeVar = "Cannot bind a new type variable"

cannotInferKind :: String
cannotInferKind = "Cannot do kind inference during constructors building"

instance UnreachableState ConsBuildError where
    isUnreachable (Unexpected err) = Just err
    isUnreachable CannotBindTyVar = Just unavailableTypeVar
    isUnreachable (TypeCreationErr (Create.ContErr (KindErr _ _))) = Nothing
    isUnreachable err @ (TypeCreationErr _) = Just $ dbgShow err

instance InfoShow ConsBuildError where
    infoShow (Unexpected _) = unexpNoInfo
    infoShow CannotBindTyVar = unexpNoInfo
    infoShow (TypeCreationErr (Create.ContErr kerr @ (KindErr _ _))) = descOf kerr
    infoShow (TypeCreationErr _) = unexpNoInfo

instance DebugShow ConsBuildError where
    dbgShow (Unexpected err) = err
    dbgShow CannotBindTyVar = unavailableTypeVar
    dbgShow (TypeCreationErr (Create.CustomErr reason)) = reason
    dbgShow (TypeCreationErr err) = descOf err

conBuild
    :: TypesTable With.ProgState
    -> DataConsTable With.ProgState
    -> Fresh.FV ()
    -> Ty.LangNewType With.ProgState
    -> Raw.ADTConstructor With.ProgState
    -> Either ConsBuildError (DataConsTable With.ProgState, Fresh.FV ())
conBuild tt ct fv lnty con =
    {- Rebinding the constructor, which have the same type variables of other constructors. -}
    case rewindCon con lnty fv of
        Left err -> Left err
        Right (con', repVars, fv') ->
            let ts = Raw.unConsFromCon con' in
            let cname = Raw.adtConNameFrom con' in
            let st = stateOf con' in
                {- The LangNewType token has to be updated first, because after that there's a call
                to Ty.newLHTyFromLNTy, which does not handle bound variables. -}
                case updateLNTy lnty repVars of
                    Nothing -> Left $ Unexpected "Replacing variable not found"
                    Just lnty' ->
                        case rightmapFst (Create.aType tt $ getKindFrom lnty') ts of
                            Left err -> Left . TypeCreationErr $ fmap (const cannotInferKind) err
                            Right lhts ->
                                case Ty.newLHTyFromLNTy lnty' of
                                    Nothing -> Left $ Unexpected "Maybe a bad construction of LNTy value"
                                    Just lhty ->
                                        let lhts' = lhts `newNELast` lhty in
                                            Right
                                                ( addElem
                                                    (Ty.newNotedVal
                                                        <| repOf cname
                                                        <| Ty.generalize (sconcat lhts')
                                                        <| st
                                                    ) ct
                                                , fv'
                                                )
        where
            getKindFrom
                :: Ty.LangNewType With.ProgState
                -> Raw.ParamTypeName With.ProgState
                -> Raw.UnConType With.ProgState
                -> Either () Ty.LangKind
            getKindFrom lnty pty ty =
                case Ty.kindOfArg <| repOf pty <| lnty of
                    Nothing -> Left ()
                    Just lk -> Right lk

{- A mapping from old vars to new vars. -}
type ReplacingVars = Map TyVarRep TyVarRep

{- Hack to change type variables in a constructor in order not to have many constructors with same
type variables. Look at Ty.newLHTyFromLNTy for a better information. -}
rewindCon
    :: Raw.ADTConstructor With.ProgState
    -> Ty.LangNewType With.ProgState
    -> Fresh.FV ()
    -> Either ConsBuildError (Raw.ADTConstructor With.ProgState, ReplacingVars, Fresh.FV ())
rewindCon con lnty fv =
    let bindings = argsOf lnty in
        changeBindings bindings empty con fv
    where
        changeBindings
            :: [Ty.LangVarType With.ProgState]
            -> ReplacingVars
            -> Raw.ADTConstructor With.ProgState
            -> Fresh.FV ()
            -> Either ConsBuildError (Raw.ADTConstructor With.ProgState, ReplacingVars, Fresh.FV ())
        {- The reverse is due to the head insertion of type variables. -}
        changeBindings [] repVars con fv = Right (con, repVars, fv)
        changeBindings (v : t) repVars con fv =
            let (newV, fv') = Fresh.allocFreeVar () fv in
            let newVarRep = tokenRepFromStr newV in
            let varRep = repOf v in
                case Raw.visitPtsInAdtCon con <| Right () <| updatePty varRep newVarRep of
                    (_, Left err) -> Left err
                    (con', Right _) -> changeBindings t (M.insert varRep newVarRep repVars) con' fv'

        updatePty
            :: TyVarRep
            -> TyVarRep
            -> Raw.ParamTypeName With.ProgState
            -> Either ConsBuildError ()
            -> (Raw.ParamTypeName With.ProgState, Either ConsBuildError ())
        updatePty _ _ pty err @ (Left _) = (pty, err)
        updatePty old new pty (Right _) =
            if old == repOf pty
            then (Raw.buildPtyName new $ stateOf pty, Right ())
            else (pty, Right ())

{- LangNewType value update. -}
updateLNTy :: Ty.LangNewType With.ProgState -> ReplacingVars -> Maybe (Ty.LangNewType With.ProgState)
updateLNTy lnty repVars =
    case Ty.visitVarsInLNTy lnty (Just repVars) visitVars of
        (_, Nothing) -> Nothing
        (lnty, Just _) -> Just lnty
    where
        visitVars
            :: Ty.LangVarType With.ProgState
            -> Maybe ReplacingVars
            -> (Ty.LangVarType With.ProgState, Maybe ReplacingVars)
        visitVars tyVar Nothing = (tyVar, Nothing)
        visitVars tyVar (Just repVars) =
            case M.lookup (repOf tyVar) repVars of
                Nothing -> (tyVar, Nothing)
                Just tyVarRep ->
                    (Ty.newLVTy tyVarRep <| Ty.kindOf tyVar <| Ty.Representational <| stateOf tyVar, Just repVars)

consFromAdt
    :: TypesTable With.ProgState
    -> (DataConsTable With.ProgState, Fresh.FV ())
    -> Raw.AlgebraicDataType With.ProgState
    -> Either ConsBuildError (DataConsTable With.ProgState, Fresh.FV ())
consFromAdt tt (ct, fv) adt =
    let rty = Raw.adtNameFrom adt in
    let rName = repOf rty in
    let cons = Raw.adtConsFrom adt in
        case kFind rName tt of
            Nothing -> Left . Unexpected $ "No type in types table with name " ++ tokenRepToStr rName
            Just lnty -> foldl' <| buildFromRes lnty <| Right (ct, fv) <| cons
        where
            buildFromRes _ err @ (Left _) _ = err
            buildFromRes lnty (Right (ct, fv)) con = conBuild tt ct fv lnty con

build
    :: TypesTable With.ProgState
    -> Fresh.FV ()
    -> Raw.AstOpRes
        With.ProgState
        ConsBuildError
        (DataConsTable With.ProgState, Fresh.FV ())
build tt fv = Raw.lookupAdt (noElems, fv) $ consFromAdt tt
