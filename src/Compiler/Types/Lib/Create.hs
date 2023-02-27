{- Given a token of Ast.Tree which should represent a type (just ideally, at that level
the notion of type should not exist yet), this module creates the associated token which
is really a type. -}
module Compiler.Types.Lib.Create
    ( Err(..)
    , aType
    , aConstraint
    , aConstrainedType
) where

import Lib.Utils
import Lib.Result(Description(..))
import Data.List(foldl', nub)
import Compiler.Ast.Common
import Compiler.Ast.Tree as Raw
import Compiler.Ast.Typed as Ty
import Compiler.Types.Tables
import Compiler.Types.Lib.Error
import Compiler.State as With

data Err err =
      CustomErr err
    | ContErr ContSpErr
    | UnexistingCont (Raw.Constraint With.ProgState)
    | UnexistingType (Raw.UnConType With.ProgState)
    | SomeMalformedType
    deriving Show

{- It's not up to this module to define DebugShow, InfoShow and other related stuff. But it's up to the client
of this module to decide the `Err` properties. However, Description instance offers a kind-of default print
function. -}
instance Description (Err err) where
    {- NB: descOf on `CustomErr` values should be quite useless. It is recommended not to use it. -}
    descOf (CustomErr _) = "Custom error"
    descOf (ContErr err) = descOf err
    descOf (UnexistingCont c) = "Constraint at " ++ show (stateOf c) ++ " does not exist in constraints table"
    descOf (UnexistingType ty) = "Type at " ++ show (stateOf ty) ++ " does not exist in types table"
    descOf SomeMalformedType = "Could not create a new type token, some tokens are inconsistent"

instance Functor Err where
    fmap f (CustomErr err) = CustomErr $ f err
    fmap _ (ContErr err) = ContErr err
    fmap _ (UnexistingCont c) = UnexistingCont c
    fmap _ (UnexistingType ty) = UnexistingType ty
    fmap _ SomeMalformedType = SomeMalformedType

fromUnCon
    :: TypesTable With.ProgState
    -> (Raw.ParamTypeName With.ProgState -> Raw.UnConType With.ProgState -> Either err Ty.LangKind)
    -> Raw.UnConType With.ProgState
    -> (Raw.ParamTypeName With.ProgState -> Either (Err err) [Ty.LangSpecConstraint With.ProgState])
    -> Either (Err err) (Ty.LangHigherType With.ProgState, [Ty.LangSpecConstraint With.ProgState])
fromUnCon tt getKind ty mkCs =
    case Raw.buildGenFromBase ty of
        Right pty -> aParamType tt getKind pty ty mkCs
        Left rty -> aRealType tt getKind rty ty mkCs

aSplitType
    :: TypesTable With.ProgState
    -> (Raw.ParamTypeName With.ProgState -> Raw.UnConType With.ProgState -> Either err Ty.LangKind)
    -> Raw.UnConType With.ProgState
    -> Either (Err err) (Ty.LangHigherType With.ProgState)
aSplitType tt getKind ty =
    case fromUnCon tt getKind ty . const $ Right [] of
        Left err -> Left err
        Right (lhty, _) -> Right lhty

aSplitType'
    :: TypesTable With.ProgState
    -> (Raw.ParamTypeName With.ProgState -> Raw.UnConType With.ProgState -> Either err Ty.LangKind)
    -> (Raw.ParamTypeName With.ProgState -> Either (Err err) [Ty.LangSpecConstraint With.ProgState])
    -> Raw.UnConType With.ProgState
    -> Either (Err err) (Ty.LangHigherType With.ProgState, [Ty.LangSpecConstraint With.ProgState])
aSplitType' tt getKind mkCs ty =
    fromUnCon tt getKind ty mkCs

{- Given a Raw.UnConType value and a TypesTable table, it builds a new LangQualType value with inferred kind *
(it should be *, unless strange things). It takes also a callback which, given a ParamTypeName `pty` and an
`UnConType` ty, it returns a result type with customizable error type and a `LangKind` value as success: this
because it's necessary to know the kind of type variable to build it. -}
aType
    :: TypesTable With.ProgState
    {- Kind callback: it's a sort of delegation because there should be many ways to find the kind of a type
    variable and the way to follow may depend from the context. -}
    -> (Raw.ParamTypeName With.ProgState -> Raw.UnConType With.ProgState -> Either err Ty.LangKind)
    -> Raw.UnConType With.ProgState
    -> Either (Err err) (Ty.LangHigherType With.ProgState)
aType tt getKind ty =
    case aSplitType tt getKind ty of
        Left err -> Left err
        Right lhty -> Right lhty

{- Given a raw Constraint, it builds the typed specialized constraints. A list of constraints is returned (and not
just one constraint) because a property may, in some cases, exist only under certain conditions (=predicates) and
these conditions are returned under the form of constraints.
NB: it performs a substitution, so some constraints may result to have no type variable; there is no check on this,
it's not up to this function. -}
aConstraint
    :: TypesTable With.ProgState
    -> ConstraintsTable With.ProgState
    -> (Raw.ParamTypeName With.ProgState -> Raw.UnConType With.ProgState -> Either err Ty.LangKind)
    -> Raw.Constraint With.ProgState
    -> Either (Err err) (Ty.LangSpecConstraint With.ProgState, [Ty.LangSpecConstraint With.ProgState])
aConstraint tt ct getKind c =
    case kFind (strOf $ headOf c) ct of
        Nothing -> Left $ UnexistingCont c
        Just (lnc, cs) ->
            case foldl' tryMkTy <| Right [] <| argsOf c of
                {- The reversing is crucial, because types have been inserted in the head,
                so the last ones are now the first ones. -}
                Right lhts ->
                    case Ty.newLSpCont lnc (reverse lhts) $ stateOf c of
                        Right (lspc, subst) ->
                            let lam = Ty.specLambda subst in
                                Right (lspc, map (`Ty.specTypeWith` lam) cs)
                        Left err -> Left $ ContErr err
                Left err -> Left err
            where
                tryMkTy (Right ts) ty =
                    case aSplitType tt getKind ty of
                        Right lhty -> Right (lhty : ts)
                        Left err -> Left err
                tryMkTy err @ (Left _) _ = err

{- It has the same semantics of `aType`, but it takes a `Raw.Type` value instead of a `Raw.UnConType` one and
a constraints table in order to inject type variables with predicates in the returned type.
NB: it performs a substitution, so some constraints may result to have type variables; there is no check on this,
it's not up to this function. -}
aConstrainedType
    :: TypesTable With.ProgState
    -> ConstraintsTable With.ProgState
    -> (Raw.ParamTypeName With.ProgState -> Raw.UnConType With.ProgState -> Either err Ty.LangKind)
    -> Raw.Type With.ProgState
    -> Either (Err err) (Ty.LangQualType With.ProgState)
aConstrainedType tt ct getKind ty =
    let cs = Raw.contsFromType ty in
    let uty = Raw.unConFromType ty in
        case fromUnCon tt getKind uty $ mkConts cs of
            Left err -> Left err
            Right (lhty, conts) -> Right $ Ty.newQualType conts lhty
    where
        {- Note that constraints returned from this computation compared to the raw ones as inputs are reversed.
        This should not influence the semantics. -}
        mkConts cs pty =
            foldl' <| tryMkCont pty <| Right [] <| cs

        tryMkCont pty (Right cs) c =
            if pty `Raw.ptyOccurCont` c
            then case aConstraint tt ct getKind c of
                Right (lsc, lscs) -> Right ((lsc : lscs) ++ cs)
                Left err -> Left err
            else Right cs
        tryMkCont _ err @ (Left _) _ = err

{- Given a Type token, it builds the typed tokens from its arguments. -}
makeArgs
    :: TypesTable With.ProgState
    -> (Raw.ParamTypeName With.ProgState -> Raw.UnConType With.ProgState -> Either err Ty.LangKind)
    -> Raw.UnConType With.ProgState
    -> (Raw.ParamTypeName With.ProgState -> Either (Err err) [Ty.LangSpecConstraint With.ProgState])
    -> Either (Err err) ([Ty.LangHigherType With.ProgState], [Ty.LangSpecConstraint With.ProgState])
makeArgs tt getKind ty mkCs =
    case rightmapFst <| aSplitType' tt getKind mkCs <| argsOf ty of
        Left err -> Left err
        Right splitTs ->
            let (lhts, contss) = unzip splitTs in
                {- Concatenation can bring duplicates, so using `nub` to remove them. -}
                Right (lhts, nub $ concat contss)

aRealType
    :: TypesTable With.ProgState
    -> (Raw.ParamTypeName With.ProgState -> Raw.UnConType With.ProgState -> Either err Ty.LangKind)
    -> Raw.ADTName With.ProgState
    -> Raw.UnConType With.ProgState
    -> (Raw.ParamTypeName With.ProgState -> Either (Err err) [Ty.LangSpecConstraint With.ProgState])
    -> Either (Err err) (Ty.LangHigherType With.ProgState, [Ty.LangSpecConstraint With.ProgState])
aRealType t getKind rty ty mkCs =
    case kFind (strOf rty) t of
        Nothing -> Left $ UnexistingType ty
        Just lnty ->
            case makeArgs t getKind ty mkCs of
                Left err -> Left err
                Right (margs, argsCs) ->
                    case Ty.newLowLHTy lnty margs $ stateOf ty of
                        Nothing -> Left SomeMalformedType
                        {- No need to use `nub` to remove duplicates, since they remain untouched. -}
                        Just lhty -> Right (lhty, argsCs)

aParamType
    :: TypesTable With.ProgState
    -> (Raw.ParamTypeName With.ProgState -> Raw.UnConType With.ProgState -> Either err Ty.LangKind)
    -> Raw.ParamTypeName With.ProgState
    -> Raw.UnConType With.ProgState
    {- Callback that, given a `Raw.ParamTypeName` value, builds the specialized constraints associated to it. -}
    -> (Raw.ParamTypeName With.ProgState -> Either (Err err) [Ty.LangSpecConstraint With.ProgState])
    -> Either (Err err) (Ty.LangHigherType With.ProgState, [Ty.LangSpecConstraint With.ProgState])
aParamType t getKind pty ty mkCs =
    let pName = strOf pty in
        case getKind pty ty of
            Left err -> Left $ CustomErr err
            Right lk ->
                case makeArgs t getKind ty mkCs of
                    Left err -> Left err
                    Right (margs, argsCs) ->
                        case mkCs pty of
                            Left err -> Left err
                            Right cs ->
                                let var = Ty.newLVTy pName lk Representational $ stateOf pty in
                                    case Ty.newLowLHTy'' var margs $ stateOf pty of
                                        Nothing -> Left SomeMalformedType
                                        {- `mkCs` created new constraints, so using `nub` to remove duplicates. -}
                                        Just lhty -> Right (lhty, nub (cs ++ argsCs))
