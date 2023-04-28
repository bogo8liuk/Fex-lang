{- Given a list of Ast.Tree.Declaration, it builds a table of types fetched from declarations
of ADT or AliasADT. -}
module Compiler.Types.Builder.Kind
    ( UnexpectedKindError
    , BuildInfKindError
    , TooManyArgsError
    , unavKindVar
    , badTypeStruct
    , nameNotFound
    , TypeGenErr(..)
    , KindsTable
    {- exporting this because kind inference can be applied to fields different from types building as well. -}
    , kindFromType
    , deleteWhen
    , kindDiscover
) where

import Lib.Utils
import Lib.Result
import Data.Map.Strict as Map hiding (null, map, filter, foldl')
import Data.List(foldl')
import Compiler.Ast.Common
import Compiler.Ast.Tree as Raw
import Compiler.Ast.Typed as Ty hiding (kindOf)
import Compiler.State as With
import Compiler.Types.Lib.FreeVars as Fresh

type UnexpectedKindError =
    ( String                           --name
    , (Ty.LangKind, With.ProgState)    --expected
    , (Ty.LangKind, With.ProgState)    --actual
    )

{- Trying to build an infinite kind. -}
type BuildInfKindError = (Raw.ParamTypeName With.ProgState, Raw.ParamTypeName With.ProgState)

{- The first is the actual "state of art", the second one is what expected to be (or what previously
found to be). -}
type TooManyArgsError = (Raw.GenTypeName With.ProgState, (Ty.LangKind, With.ProgState))

data TypeGenErr =
      {- Unexpected kind error: a type is "misused". -}
      UeKErr UnexpectedKindError
      {- Too much args: a type has got more arguments than it should have. -}
    | TArgsErr TooManyArgsError
      {- Trying to build an infinite kind. -}
    | InfKErr BuildInfKindError
    | UnreachableState String

unavKindVar :: String
unavKindVar = "Unavailable fresh kind variable"

badTypeStruct :: String
badTypeStruct = "Composite type should have at least one argument"

nameNotFound :: String
nameNotFound = "A (not found) name had to stand in the kinds table"

inconsistentKind :: String
inconsistentKind = "The kind of a type has been inferred in a bad way"

unexpKindErr :: UnexpectedKindError -> String
unexpKindErr (name, (expK, expSt), (curK, curSt)) =
    "Kind inconsistency for symbol " ++ name ++ ": at " ++ show expSt ++ " it has kind " ++ show expK ++
    " but at " ++ show curSt ++ " it has kind " ++ show curK

tooManyArgsErr :: TooManyArgsError -> String
tooManyArgsErr (gName, (lk, st)) =
    "Type " ++ Raw.strOfGenName gName ++ " at " ++ show (Raw.stateOfGenName gName) ++ " is expected to have kind " ++
    show lk ++ " as inferred at " ++ show st ++ ", but it has too many arguments"

infKindErr :: BuildInfKindError -> String
infKindErr (pty, pty') =
    "Cannot build infinite kind between: " ++ repOf pty ++ " at " ++
    show (stateOf pty) ++ " and " ++ repOf pty' ++ " at " ++ show (stateOf pty')

instance DebugShow TypeGenErr where
    dbgShow (UeKErr err) = unexpKindErr err
    dbgShow (TArgsErr err) = tooManyArgsErr err
    dbgShow (InfKErr err) = infKindErr err
    dbgShow (UnreachableState reason) = "Kind inference unreachable state due to: " ++ reason

instance InfoShow TypeGenErr where
    infoShow (UeKErr err) = unexpKindErr err
    infoShow (TArgsErr err) = tooManyArgsErr err
    infoShow (InfKErr err) = infKindErr err
    infoShow (UnreachableState _) = unexpNoInfo

instance UnreachableState TypeGenErr where
    isUnreachable (UnreachableState s) = Just s
    isUnreachable _ = Nothing

type KindsTable = Map String (Ty.LangKind, With.ProgState)

{- It promotes each occurrence of a variable in the kinds table with a new LangKind. -}
updateVar
    :: String       --variable to search
    -> Ty.LangKind
    -> KindsTable
    -> KindsTable
updateVar v k m = fromList . map (\(s, (lk, st)) -> (s, (Ty.promoteVar v lk k, st))) $ toAscList m 

updateKinds
    :: [(String, Ty.LangKind)]
    -> KindsTable
    -> KindsTable
updateKinds [] m = m
updateKinds ((v, k) : t) m = updateKinds t $ updateVar v k m

{- singleReal has the same implementation of singleParam (TODO: unify implementations). -}
singleReal
    :: Maybe (Int, Raw.GenTypeName With.ProgState)
    -> KindsTable
    -> Fresh.FV ()
    -> Raw.ADTName With.ProgState
    -> Either TypeGenErr (KindsTable, Fresh.FV ())
singleReal Nothing m cont rty =
    let rName = repOf rty in
    let st = stateOf rty in
        case Map.lookup rName m of
            Just (LKConst, _) -> Right (m, cont)
            Just (cks' @ (SubLK ks'), st') ->
                Left $ UeKErr (rName, (cks', st'), (LKConst, st))
            Just (LKVar v', st') ->
                Right (Map.insert rName (LKConst, st) $ updateKinds [(v', LKConst)] m, cont)
            Nothing -> Right (Map.insert rName (LKConst, st) m, cont)
singleReal (Just (pos, ty)) m cont rty =
    let name = Raw.strOfGenName ty in
    let rName = repOf rty in
    let st = stateOfGenName ty in
    let rst = stateOf rty in
    let tSearch = Map.lookup name m in
    let rSearch = Map.lookup rName m in
        case (rSearch, tSearch) of
            (_, Just (LKConst, st'')) ->
                let (v, _) = Fresh.allocFreeVar () cont in
                    Left $ UeKErr (name, (LKConst, st''), (SubLK [LKVar v, LKConst], st))
            (Just (lk', st'), Just (SubLK ks'', st'')) ->
                case Lib.Utils.elemAt pos ks'' of
                    Nothing -> Left $ TArgsErr (ty, (SubLK ks'', st''))
                    Just lk -> case lk' Ty.==^ lk of
                        Nothing -> Left $ UeKErr (rName, (SubLK ks'', st''), (lk', st'))
                        Just l -> Right (updateKinds l m, cont)
            (Nothing, Just (SubLK ks'', st'')) ->
                case Lib.Utils.elemAt pos ks'' of
                    Nothing -> Left $ TArgsErr (ty, (SubLK ks'', st''))
                    Just lk -> Right (Map.insert rName (lk, rst) m, cont)
            (Nothing, _) ->
                let (v, cont') = Fresh.allocFreeVar () cont in
                    Right (Map.insert rName (LKVar v, rst) m, cont')
            (_, _) -> Right (m, cont)

singleParam
    :: Maybe (Int, Raw.GenTypeName With.ProgState)
    -> KindsTable
    -> Fresh.FV ()
    -> Raw.ParamTypeName With.ProgState
    -> Either TypeGenErr (KindsTable, Fresh.FV ())
singleParam Nothing m cont pty =
    let pName = repOf pty in
    let st = stateOf pty in
        case Map.lookup pName m of
            Just (LKConst, _) -> Right (m, cont)
            Just (SubLK ks', st') ->
                Left $ UeKErr (pName, (LKConst, st), (SubLK ks', st'))
            Just (LKVar v', st') ->
                Right (Map.insert pName (LKConst, st) $ updateKinds [(v', LKConst)] m, cont)
            Nothing -> Right (Map.insert pName (LKConst, st) m, cont)
singleParam (Just (pos, ty)) m cont pty =
    let name = Raw.strOfGenName ty in
    let pName = repOf pty in
    let st = stateOfGenName ty in
    let pst = stateOf pty in
    let tSearch = Map.lookup name m in
    let pSearch = Map.lookup pName m in
        case (pSearch, tSearch) of
            (_, Just (LKConst, st'')) ->
                let (v, _) = Fresh.allocFreeVar () cont in
                    Left $ UeKErr (name, (LKConst, st''), (SubLK [LKVar v, LKConst], st))
            (Just (lk', st'), Just (SubLK ks'', st'')) ->
                case Lib.Utils.elemAt pos ks'' of
                    Nothing -> Left $ TArgsErr (ty, (SubLK ks'', st''))
                    Just lk -> case lk' Ty.==^ lk of
                        Nothing -> Left $ UeKErr (pName, (SubLK ks'', st''), (lk', st'))
                        Just l -> Right (updateKinds l m, cont)
            (Nothing, Just (SubLK ks'', st'')) ->
                case Lib.Utils.elemAt pos ks'' of
                    Nothing -> Left $ TArgsErr (ty, (SubLK ks'', st''))
                    Just lk -> Right (Map.insert pName (lk, pst) m, cont)
            {- In this case there's nothing to do: the external type (`ty`) should be updated to SubLK,
            but this has not to be performed here. However, a new variable is introduced. -}
            (Nothing, _) ->
                let (v, cont') = Fresh.allocFreeVar () cont in
                    Right (Map.insert pName (LKVar v, pst) m, cont')
            {- The parametric type already exists in the map, so it has not be inserted in the map as
            a new kind variable as above. -}
            (_, _) -> Right (m, cont)

{- To use with fold functions. -}
fetchType
    :: Raw.GenTypeName With.ProgState
    -> (Int, Either TypeGenErr (KindsTable, Fresh.FV ()))
    -> Raw.UnConType With.ProgState
    -> (Int, Either TypeGenErr (KindsTable, Fresh.FV ()))
fetchType from res ty = case res of
    (i, Right (m, cont)) -> (i + 1, kindFromType (Just (i, from)) m cont ty)
    (i, Left err) -> (i, Left err)

diff
    :: [Ty.LangKind]
    -> [Raw.UnConType With.ProgState]
    -> KindsTable
    -> Raw.GenTypeName With.ProgState
    -> Ty.LangKind
    -> With.ProgState
    -> KnowledgeOneOf TypeGenErr ([Ty.LangKind], KindsTable)
diff [] [] kt _ _ _ = That ([], kt)
diff [] _ _ gty expctd st = This $ TArgsErr (gty, (expctd, st))
diff ks [] kt _ _ _ = That (ks, kt)
diff (lk : ks) (ty : ts) kt gty expctd st = case inferKindOfType ty kt of
    None -> None
    This err -> This err
    That (lk', st', kt') -> case lk Ty.==^ lk' of
        {- NB: in this case, the state `st` is not the one which should be associated with `lk`, because
        `st` is the state of the outer type, while `lk` is the kind of the inner type. -}
        Nothing -> This $ UeKErr (Raw.strOfGenName $ Raw.baseNameFromUnCon ty, (lk, st), (lk', st'))
        Just l -> diff ks ts (updateKinds l kt') gty expctd st

{- `getDiffKinds lk st gty ts kt` calculates the actual kind of `gty` in a program, knowing its
kind is `lk` and it has arguments `ts`. The actual kinds of types are inferred from `kt`. The state
`st` is useful to output info in case of errors. -}
getDiffKinds
    :: Ty.LangKind
    -> With.ProgState
    -> Raw.GenTypeName With.ProgState
    -> [Raw.UnConType With.ProgState]
    -> KindsTable
    -> KnowledgeOneOf TypeGenErr (Ty.LangKind, KindsTable)
getDiffKinds (SubLK ks) st gty ts kt = case diff ks ts kt gty (SubLK ks) st of
    None -> None
    This err -> This err
    That ([], kt') -> That (LKConst, kt')
    That (ks', kt') -> That (SubLK ks', kt')
getDiffKinds other _ _ _ _ = This $ UnreachableState inconsistentKind

{- Given a Type token and a kinds table, it infers the kind of the token according to information inside
the table. -}
inferKindOfType
    :: Raw.UnConType With.ProgState
    -> KindsTable
    -> KnowledgeOneOf TypeGenErr (Ty.LangKind, With.ProgState, KindsTable)
inferKindOfType ty kt =
    case inferOn ty kt of
        {- If the returned kind of the form `SubLK [lk]` (so a singleton list), then it must be adjusted,
        by unwrapping it. For instance:
            ((*) -> *)      => This does not make sense
            (* -> *)        => The one above must turned into this
        -}
        That (SubLK [lk], st, kt') -> That (lk, st, kt')
        other -> other
    where
        inferOn ty kt =
            doOnUnCon ty
                (\rty -> case Map.lookup (repOf rty) kt of
                    Nothing -> None
                    Just (lk, st) -> That (lk, st, kt))
                (\pty -> case Map.lookup (repOf pty) kt of
                    Nothing -> None
                    Just (lk, st) -> That (lk, st, kt))
                (\rty ts -> case Map.lookup (repOf rty) kt of
                    Nothing -> None
                    Just (lk, st) -> case getDiffKinds lk st (Raw.buildGenTypeName rty) ts kt of
                        None -> None
                        This err -> This err
                        That (lk', kt') -> That (lk', st, kt'))
                (\pty ts -> case Map.lookup (repOf pty) kt of
                    Nothing -> None
                    Just (lk, st) -> case getDiffKinds lk st (Raw.buildGenTypeName' pty) ts kt of
                        None -> None
                        This err -> This err
                        That (lk', kt') -> That (lk', st, kt'))

{- It builds a kind of the form SubLK, from a list of types and a kinds table. The third list is just
an accumulator. -}
getKinds
    :: [Raw.UnConType With.ProgState]
    -> KindsTable
    -> [Ty.LangKind]
    -> KnowledgeOneOf TypeGenErr (Ty.LangKind, KindsTable)
getKinds [] m accum = That (SubLK $ reverse accum ++ [LKConst], m)
getKinds (ty : t) m accum = case inferKindOfType ty m of
    None -> None
    This err -> This err
    That (lk, _, m') -> getKinds t m' $ lk : accum

compReal
    :: KindsTable
    -> Fresh.FV ()
    -> Raw.ADTName With.ProgState
    -> [Raw.UnConType With.ProgState]
    -> Either TypeGenErr (KindsTable, Fresh.FV ())
{- This case should never happen and it implies an error of parsing. -}
compReal _ _ _ [] = Left $ UnreachableState badTypeStruct
compReal m cont rty ts @ (ty : t) =
    let rName = repOf rty in
    let st = stateOf rty in
    let gty = Raw.buildGenTypeName rty in
        {- First, it infers the kinds of all the type arguments, if all ok, then the check can continue,
        else it stops it and returns the first occurred error. -}
        case foldl' (fetchType gty) (1, kindFromType (Just (0, gty)) m cont ty) t of
            (_, Left err) -> Left err
            (_, Right (m', cont')) -> case getKinds ts m' [] of
                None -> Left $ UnreachableState nameNotFound
                This err -> Left err
                That (infrdk, m'') ->
                    case Map.lookup rName m'' of
                        Just (LKConst, st') -> Left $ UeKErr (rName, (LKConst, st'), (infrdk, st))
                        Just (cks' @ (SubLK ks'), st') ->
                            case infrdk Ty.==^ cks' of
                                Nothing -> Left $ UeKErr (rName, (cks', st'), (infrdk, st))
                                Just l -> Right (updateKinds l m'', cont')
                        Just (LKVar v', st') ->
                            Right (Map.insert rName (infrdk, st) $ updateKinds [(v', infrdk)] m'', cont')
                        Nothing -> Right (Map.insert rName (infrdk, st) m'', cont')

{- It gets the first param type name in a list of types that makes true the (string-)equality with
a string. -}
immediately :: String -> [Raw.UnConType With.ProgState] -> Maybe (Raw.ParamTypeName With.ProgState)
immediately pName ts =
    case firstThat (\ty -> doOnUnCon ty <| sr pName <| sp pName <| cr pName <| cp pName) ts of
        Nothing -> Nothing
        Just ty' -> Raw.paramTNameFromUnCon ty'
    where
        sr _ _ = False
        sp pName pty = repOf pty == pName
        cr _ _ _ = False
        cp pName pty _ = repOf pty == pName

compParam
    :: KindsTable
    -> Fresh.FV ()
    -> Raw.ParamTypeName With.ProgState
    -> [Raw.UnConType With.ProgState]
    -> Either TypeGenErr (KindsTable, Fresh.FV ())
compParam _ _ _ [] = Left $ UnreachableState badTypeStruct
compParam m cont pty ts @ (ty : t) =
    let pName = repOf pty in
    let st = stateOf pty in
    let gty = Raw.buildGenTypeName' pty in
        {- If the same param type (of pty) occurs immediately, the user are trying to build an infinite kind.
        This is represented by the following cases:
            - T (a a)               <- using `a` as a parameter of `a`
            - T (a b (a d e f) c)   <- same as above (the other args do not matter), using `a` as a parameter of `a` -}
        case immediately pName ts of
            Just pty' -> Left $ InfKErr (pty, pty')
            Nothing -> case foldl' (fetchType gty) (1, kindFromType (Just (0, gty)) m cont ty) t of
                (_, Left err) -> Left err
                (_, Right (m', cont')) -> case getKinds ts m' [] of
                    None -> Left $ UnreachableState nameNotFound
                    This err -> Left err
                    That (infrdk, m'') -> case Map.lookup pName m'' of
                        Just (LKConst, st') ->
                            Left $ UeKErr (pName, (LKConst, st'), (infrdk, st))
                        Just (cks' @ (SubLK ks'), st') ->
                            case infrdk Ty.==^ cks' of
                                Nothing -> Left $ UeKErr (pName, (cks', st'), (infrdk, st))
                                Just l -> Right (updateKinds l m'', cont')
                        Just (LKVar v', st') ->
                            Right (Map.insert pName (infrdk, st) $ updateKinds [(v', infrdk)] m'', cont')
                        Nothing -> Right (Map.insert pName (infrdk, st) m'', cont')

kindFromType
    :: Maybe (Int, Raw.GenTypeName With.ProgState)  --(position of the arg, from type...)
    -> KindsTable
    -> Fresh.FV ()
    -> Raw.UnConType With.ProgState
    -> Either TypeGenErr (KindsTable, Fresh.FV ())
kindFromType argTy kt cont ty =
    Raw.doOnUnCon ty
        <| singleReal argTy kt cont
        <| singleParam argTy kt cont
        <| compReal kt cont
        <| compParam kt cont

kindFromCon
    :: KindsTable
    -> Fresh.FV ()
    -> Raw.ADTConstructor With.ProgState
    -> Either TypeGenErr (KindsTable, Fresh.FV ())
kindFromCon kt cont con = case Raw.unConsFromCon con of
    [] -> Right (kt, cont)
    (ty : t) -> foldl' fetchKinds (kindFromType Nothing kt cont ty) t
        where
            fetchKinds (Left err) _ = Left err
            fetchKinds (Right (m, cont)) ty = kindFromType Nothing m cont ty

{- It builds the kind of an adt. It also performs the following checks:
    - for missing parameters in the declaration (which have to be of kind LKConst);
    - for the existence of the current adt in the map and, in case, for promotion.
-}
kindOfType
    :: KindsTable
    -> String
    -> With.ProgState
    -> Fresh.FV ()
    -> [Raw.ParamTypeName With.ProgState]
    -> [Ty.LangKind]
    -> Either TypeGenErr (KindsTable, Fresh.FV ())
kindOfType kt cur st cont [] ks =
    {- Reversing the list of kinds because they have been inserted from the head. -}
    let infrdk =
         if null ks
         then LKConst
         else SubLK $ reverse ks ++ [LKConst] in
        {- This is important: if the current adt has been already inserted in the map, then the equality
        between the just inferred kind and the already inserted kind has to be checked. -}
        case Map.lookup cur kt of
            Nothing -> Right (Map.insert cur (infrdk, st) kt, cont)
            {- If it already exists, then the two kinds have to be checked and, if necessary, promoted. -}
            Just (lk', st') ->
                case infrdk Ty.==^ lk' of
                    Nothing -> Left $ UeKErr (cur, (infrdk, st), (lk', st'))
                    Just l -> Right (updateKinds l $ Map.insert cur (infrdk, st) kt, cont)
kindOfType kt cur st cont (pty : t) ks =
    let pName = repOf pty in
        case Map.lookup pName kt of
            {- If not found, it has to be inserted as LKConst. -}
            Nothing -> kindOfType kt cur st cont t $ LKConst : ks
            Just (lk', _) -> kindOfType kt cur st cont t $ lk' : ks

deleteWhen
    :: KindsTable
    -> (String -> Bool)
    -> KindsTable
deleteWhen kt toDel = removeFrom kt . filter toDel . map fst $ toList kt
    where
        removeFrom kt [] = kt
        removeFrom kt (s : t) = removeFrom <| Map.delete s kt <| t

{- It deletes all param types in a kinds table. It should be used when the kind of an adt has already been
inferred. -}
deleteLocals
    :: KindsTable
    -> [Raw.ParamTypeName With.ProgState]
    -> KindsTable
deleteLocals kt ps = deleteWhen kt (\s -> s `elem` map repOf ps)

kindFrom
    :: KindsTable
    -> String                              --current name of adt declaration
    -> With.ProgState                      --state of the declaration (the current one)
    -> Fresh.FV ()
    -> [Raw.ParamTypeName With.ProgState]
    -> [Raw.ADTConstructor With.ProgState]
    -> Either TypeGenErr (KindsTable, Fresh.FV ())
kindFrom kt cur st cont ps [] =
    case kindOfType kt cur st cont ps [] of
        Right (kt', cont') -> Right (deleteLocals kt' ps, cont')
        err -> err
kindFrom kt cur st cont ps (con : ct) =
    case kindFromCon kt cont con of
        Right (kt', cont') -> kindFrom kt' cur st cont' ps ct
        err -> err

kindOf
    :: (KindsTable, Fresh.FV ())        --A tuple just to keep clear `kindDiscover` function, `kindOf` can be passed directly
    -> Raw.AlgebraicDataType With.ProgState
    -> Either TypeGenErr (KindsTable, Fresh.FV ())
kindOf (kt, cont) adt =
    kindFrom
        kt
        (repOf $ Raw.adtNameFrom adt)
        (stateOf adt)
        cont
        (Raw.boundParamTNamesFromAdt adt)
        (Raw.adtConsFrom adt)

kindDiscover :: KindsTable -> Raw.AstOp With.ProgState TypeGenErr (KindsTable, Fresh.FV ())
kindDiscover kt = Raw.lookupAdt (kt, Fresh.newFreeVarsContainer) kindOf
