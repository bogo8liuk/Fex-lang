{- Kind inference takes place in Builder.Kind and Builder.New modules. However, in those modules, no types
table (or other objects which can help to do kind inference) is available (because it has to be built in
Builder.New module): here takes place the kind inference with some extra information which helps the
algorithm and this module gather the API for kind inference in different ways. -}

module Compiler.Types.Lib.InferKind
    ( getKinds
    , getKinds'
    , getKinds''
    , getKinds'''
    , getKindsFromCont
    --Re-exporting these utilities in order to have all in one point
    , KInfr.KindsTable
    , KInfr.TypeGenErr(..)
    , module Fresh
) where

import Utils.Fancy
import Utils.Data.Foldable
import Data.List(foldl')
import Data.Map.Strict as M hiding (map, foldl')
import Compiler.State as With
import Compiler.Ast.Common
import qualified Compiler.Ast.Tree as Raw
import qualified Compiler.Ast.Typed as Ty
import Compiler.Types.Tables
import Compiler.Types.Lib.FreeVars as Fresh
import qualified Compiler.Types.Builder.Kind as KInfr
import qualified Compiler.Types.Builder.New as KInfr

pushKnownKinds
    :: KInfr.KindsTable
    -> [Raw.ADTName With.ProgState]
    -> TypesTable With.ProgState
    -> KInfr.KindsTable
pushKnownKinds kt rs tt =
    let lnts = fltmap (\rty -> kFind <| repOf rty <| tt) rs in
        populate lnts kt
    where
        populate :: [Ty.LangNewType With.ProgState] -> KInfr.KindsTable -> KInfr.KindsTable
        populate [] kt = kt
        populate (lnty : t) kt = populate t $ insert <| repOf lnty <| (Ty.kindOf lnty, stateOf lnty) <| kt 

getKindsInfo
    :: TypesTable With.ProgState
    -> KInfr.KindsTable
    -> Fresh.FV ()
    -> Raw.UnConType With.ProgState
    -> Either KInfr.TypeGenErr (KInfr.KindsTable, Fresh.FV ())
getKindsInfo tt kt fv ty =
    let rs = Raw.realTypes' ty in
    {- Injecting the already discovered kinds (from a TypesTable), in order to help the kind-inference
    algorithm (`kindFromType`). -}
    let kt' = pushKnownKinds kt rs tt in
        case KInfr.kindFromType Nothing kt' fv ty of
            Right (kt'', fv') -> Right (KInfr.cleanVars kt'', fv')
            err -> err

{- `getKinds pty ts tt kt fv` tries to infer the kind of `pty` in the context of `ts` with the help of
types table `tt`, kinds table `kt` and bound variables `fv`. -}
getKinds
    :: Raw.ParamTypeName With.ProgState
    -> [Raw.UnConType With.ProgState]
    -> TypesTable With.ProgState
    -> KInfr.KindsTable
    -> Fresh.FV ()
    -> Either KInfr.TypeGenErr (Ty.LangKind, With.ProgState, KInfr.KindsTable, Fresh.FV ())
getKinds pty ts tt kt fv =
    case foldl' <| tryGetK tt <| Right (kt, fv) <| ts of
        Left err -> Left err
        Right (kt', fv') ->
            case M.lookup (repOf pty) kt' of
                Nothing -> Left $ KInfr.UnreachableState "type variable not found in kinds table"
                Just (lk, st) -> Right (lk, st, kt', fv')
    where
        tryGetK
            :: TypesTable With.ProgState
            -> Either KInfr.TypeGenErr (KInfr.KindsTable, Fresh.FV ())
            -> Raw.UnConType With.ProgState
            -> Either KInfr.TypeGenErr (KInfr.KindsTable, Fresh.FV ())
        tryGetK _ err @ (Left _) _ = err
        tryGetK tt (Right (kt, fv)) ty = getKindsInfo tt kt fv ty

{- Same of `getKinds`, but with empty starting kinds table and empty starting bound variables container. -}
getKinds'
    :: Raw.ParamTypeName With.ProgState
    -> [Raw.UnConType With.ProgState]
    -> TypesTable With.ProgState
    -> Either KInfr.TypeGenErr (Ty.LangKind, With.ProgState, KInfr.KindsTable, Fresh.FV ())
getKinds' pty ts tt = getKinds pty ts tt empty Fresh.newFreeVarsContainer

{- Same of `getKinds`, but returning only the kind. -}
getKinds''
    :: Raw.ParamTypeName With.ProgState
    -> [Raw.UnConType With.ProgState]
    -> TypesTable With.ProgState
    -> KInfr.KindsTable
    -> Fresh.FV ()
    -> Either KInfr.TypeGenErr (Ty.LangKind, With.ProgState)
getKinds'' pty ts tt kt fv =
    case getKinds pty ts tt kt fv of
        Left err -> Left err
        Right (lk, st, _, _) -> Right (lk, st)

{- Same of `getKinds'`, but returning only the kind. -}
getKinds'''
    :: Raw.ParamTypeName With.ProgState
    -> [Raw.UnConType With.ProgState]
    -> TypesTable With.ProgState
    -> Either KInfr.TypeGenErr (Ty.LangKind, With.ProgState)
getKinds''' pty ts tt =
    case getKinds' pty ts tt of
        Left err -> Left err
        Right (lk, st, _, _) -> Right (lk, st)

{- It infers the kind from a constraint token, it takes also a default error value, in case
it cannot infer the kind. -}
getKindsFromCont
    :: Raw.ParamTypeName With.ProgState
    -> Ty.LangNewConstraint With.ProgState
    -> err
    -> Either err Ty.LangKind
getKindsFromCont pty lnc err =
    case firstThat (\lvty -> repOf lvty == repOf pty) $ argsOf lnc of
        Nothing -> Left err
        Just var -> Right $ Ty.kindOf var
