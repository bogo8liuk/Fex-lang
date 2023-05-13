{- In this module there are the some data types which should be returned by the type-inference.
NB: Unfortunately, some instances give rise to ambiguities among type variables, because the
compiler cannot infer their types: this implies the client must use type hinting to help the
compiler. -}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Types.Tables
    ( TypesTable
    , DataConsTable
    , ConstraintsTable
    , InstsTable
    , PropMethodsTable
    , ImplTable
    , BindingSingleton
    , TypedBinding(..)
    , TypedProgram
    , contsOfFunType
    , module Lib.Table
) where

import Lib.Utils
import Lib.Table
import Data.List(foldl')
import Data.Map.Strict as Map hiding (map, filter, foldl')
import qualified Compiler.Config.Types as BITy
import Compiler.Ast.Common
import qualified Compiler.Ast.Tree as Raw
import qualified Compiler.Ast.Typed as Ty

{- Table for type-constructors -}
newtype TypesTable a = TyT (Map TyConRep (Ty.LangNewType a))

{- Table for data-constructors -}
newtype DataConsTable a = ConT (Map DataConRep (Ty.NotedVal a))

{- Table for constraints necessary to constraint-constructors (namely property names).
In the implementation, the keys are the properties of a program while the values are the constraints which must exist
for the associated property. -}
newtype ConstraintsTable a = ContsT (Map PropConRep (Ty.LangNewConstraint a, [Ty.LangSpecConstraint a]))

{- Table for instances methods. -}
newtype InstsTable a = InstT (Map SymbolRep [Raw.SDUnion a])

{- Table for properties methods -}
newtype PropMethodsTable a = MhtsT (Map SymbolRep (Ty.NotedVar a))

{- Table for implementations (instances in the program) of properties. This is different from ConstraintsTable which
collects the instances (constraints) which must exist for certain properties. -}
newtype ImplTable a = ImplT (Map PropConRep [Ty.LangSpecConstraint a])

type BindingSingleton a = (Ty.NotedVar a, [Ty.NotedVar a], Ty.NotedExpr a)
data TypedBinding a =
      TyNonRec (BindingSingleton a)
    | TyRec [BindingSingleton a]
    deriving Show

instance Functor TypedBinding where
    fmap f tb =
        case tb of
            TyNonRec b -> TyNonRec $ mapTuple b
            TyRec bs -> TyRec $ map mapTuple bs
        where
            mapTuple (nVar, nVars, nExpr) = (fmap f nVar, map (fmap f) nVars, fmap f nExpr)

{- A cache which is useful for recursive symbols detection. Only one symbol can be a key, but mutually recursive
bindings should have many keys, so a sort of cache is kept to know all the possible keys. -}
type RecSymsCache = Map SymbolRep SymbolRep
data TypedProgram a = TyProg (Map SymbolRep (TypedBinding a)) RecSymsCache

instance Show a => Show (TypesTable a) where
    show (TyT m) = show m

instance Show a => Show (DataConsTable a) where
    show (ConT m) = show m

instance Show a => Show (ConstraintsTable a) where
    show (ContsT m) = show m

instance Show a => Show (TypedProgram a) where
    show (TyProg m _) = show m

instance Show a => Show (InstsTable a) where
    show (InstT m) = show m

instance Show a => Show (PropMethodsTable a) where
    show (MhtsT m) = show m

instance Show a => Show (ImplTable a) where
    show (ImplT m) = show m

tableMapMapVal :: (Ord k, Functor t) => (a -> b) -> Map k [t a] -> Map k [t b]
tableMapMapVal f m =
    fromList . map (\(s, t) -> (s, map (fmap f) t)) $ toList m

tableMapVal :: (Ord k, Functor t) => (a -> b) -> Map k (t a) -> Map k (t b)
tableMapVal f m =
    fromList . map (\(s, t) -> (s, fmap f t)) $ toList m

tableMap :: (Ord (tk b), Functor tk, Functor t) => (a -> b) -> Map (tk a) (t a) -> Map (tk b) (t b)
tableMap f m =
    fromList . map (\(x, y) -> (fmap f x, fmap f y)) $ toList m

tableMapListCouple :: (Ord k, Functor t1, Functor t2) => (a -> b) -> Map k [(t1 a, t2 a)] -> Map k [(t1 b, t2 b)]
tableMapListCouple f m =
    fromList . map (\(k, l) -> (k, map (\(x, y) -> (fmap f x, fmap f y)) l)) $ toList m

tableMapCoupleList :: (Ord k, Functor t1, Functor t2) => (a -> b) -> Map k (t1 a, [t2 a]) -> Map k (t1 b, [t2 b])
tableMapCoupleList f m =
    fromList . map (\(k, (x, y)) -> (k, (fmap f x, map (fmap f) y))) $ toList m

tableMapValTriple
    :: (Ord k, Functor t1, Functor t2, Functor t3)
    => (a -> b)
    -> Map k (t1 a, [t2 a], t3 a)
    -> Map k (t1 b, [t2 b], t3 b)
tableMapValTriple f m =
    fromList . map (\(k, (x, y, z)) -> (k, (fmap f x, map (fmap f) y, fmap f z))) $ toList m

instance Functor TypesTable where
    fmap f (TyT m) =
        TyT $ tableMapVal f m

instance Functor DataConsTable where
    fmap f (ConT m) =
        ConT $ tableMapVal f m

instance Functor ConstraintsTable where
    fmap f (ContsT m) =
        ContsT $ tableMapCoupleList f m

instance Functor TypedProgram where
    fmap f (TyProg m cache) =
        TyProg (tableMapVal f m) cache

instance Functor InstsTable where
    fmap f (InstT m) =
        InstT $ tableMapMapVal f m

instance Functor PropMethodsTable where
    fmap f (MhtsT m) =
        MhtsT $ tableMapVal f m

instance Functor ImplTable where
    fmap f (ImplT m) =
        ImplT $ tableMapMapVal f m

getValues :: Map k e -> [e]
getValues m = map snd $ toList m

instance Emptiness (TypesTable a) where
    noElems = TyT empty

instance Adder (TypesTable a) (Ty.LangNewType a) where
    addElem lnty (TyT t) = TyT $ insert (repOf lnty) lnty t

instance Existence (TypesTable a) (Ty.LangNewType a) where
    existIn lnty (TyT t) = repOf lnty `member` t

instance KeyFinding (TypesTable a) TyConRep (Ty.LangNewType a) where
    kFind tcRep (TyT t) = Map.lookup tcRep t

instance KeyFinding (TypesTable a) (Ty.LangNewType a) (Ty.LangNewType a) where
    kFind lnty tt = kFind (repOf lnty) tt

instance AllGetter (TypesTable a) (Ty.LangNewType a) where
    getAllElems (TyT t) = getValues t

instance Emptiness (DataConsTable a) where
    noElems = ConT empty

instance Adder (DataConsTable a) (Ty.NotedVal a) where
    addElem con (ConT t) = ConT $ insert (repOf con) con t

instance Existence (DataConsTable a) (Ty.NotedVal a) where
    existIn con (ConT t) = repOf con `member` t

instance KeyFinding (DataConsTable a) DataConRep (Ty.NotedVal a) where
    kFind conRep (ConT t) = Map.lookup conRep t

instance KeyFinding (DataConsTable a) (Ty.NotedVal a) (Ty.NotedVal a) where
    kFind con (ConT t) = Map.lookup (repOf con) t

instance AllGetter (DataConsTable a) (Ty.NotedVal a) where
    getAllElems (ConT t) = getValues t

instance Emptiness (ConstraintsTable a) where
    noElems = ContsT empty

instance Adder (ConstraintsTable a) (Ty.LangNewConstraint a) where
    addElem cont (ContsT t) = ContsT $ insert (repOf cont) (cont, []) t

instance Adder (ConstraintsTable a) (Ty.LangNewConstraint a, [Ty.LangSpecConstraint a]) where
    addElem contCs @ (cont, _) (ContsT t) = ContsT $ insert (repOf cont) contCs t

instance Existence (ConstraintsTable a) (Ty.LangNewConstraint a) where
    existIn cont (ContsT t) = repOf cont `member` t

instance
    KeyFinding
        (ConstraintsTable a)
        (Ty.LangNewConstraint a)
        (Ty.LangNewConstraint a, [Ty.LangSpecConstraint a]) where
    kFind cont ct = kFind (repOf cont) ct

instance KeyFinding (ConstraintsTable a) PropConRep (Ty.LangNewConstraint a, [Ty.LangSpecConstraint a]) where
    kFind contRep (ContsT t) = Map.lookup contRep t

instance KeyValUpdate' (ConstraintsTable a) PropConRep [Ty.LangSpecConstraint a] where
    kValUpdate' contRep cs table @ (ContsT t) =
        case Map.lookup contRep t of
            Nothing -> table
            Just (lnc, lscs) -> ContsT $ Map.insert contRep (lnc, cs ++ lscs) t

instance KeyValUpdate' (ConstraintsTable a) (Ty.LangNewConstraint a) [Ty.LangSpecConstraint a] where
    kValUpdate' cont cs ct = kValUpdate' (repOf cont) cs ct

instance AllGetter (ConstraintsTable a) (Ty.LangNewConstraint a, [Ty.LangSpecConstraint a]) where
    getAllElems (ContsT t) = getValues t

instance Emptiness (TypedProgram a) where
    noElems = TyProg empty empty

updateCache :: RecSymsCache -> [BindingSingleton a] -> SymbolRep -> RecSymsCache
updateCache cache bs nVarRep =
    let symReps = map (repOf . fst') bs in
        forAll symReps insertIn cache
    where
        fst' (x, _, _) = x

        insertIn cache' otherSym =
            {- NB: if the symbol already exists, it is not inserted. This because it can happen that different version
            of bindings can be inserted in the cache like, for example:
                f, g, h
                f, g1, h1
            If the client tries to insert the second recursive binding (f, g1, h1), f will not be inserted in the cache. -}
            if otherSym `Map.member` cache'
            then cache'
            else Map.insert otherSym nVarRep cache'

findNotInCache :: [BindingSingleton a] -> RecSymsCache -> Maybe SymbolRep
findNotInCache [] _ = Nothing
findNotInCache ((v, _, _) : t) cache =
    let nVarRep = repOf v in
        if nVarRep `Map.member` cache
        then findNotInCache t cache
        else Just nVarRep

{- NB: if the binding to insert already exists, it is not inserted. -}
instance Adder (TypedProgram a) (TypedBinding a) where
    addElem (TyRec []) tp = tp
    addElem (TyRec bs) tp @ (TyProg m cache) =
        {- Finding the one not in cache before inserting. -}
        case findNotInCache bs cache of
            {- If all in cache, then it is not inserted. -}
            Nothing -> tp
            Just nVarRep ->
                TyProg <| insert nVarRep (TyRec bs) m <| updateCache cache bs nVarRep
    addElem (TyNonRec b @ (v, _, _)) tp @ (TyProg m cache) =
        let nVarRep = repOf v in
            if nVarRep `Map.member` m
            then tp
            else TyProg (insert nVarRep (TyNonRec b) m) cache

instance Existence (TypedProgram a) SymbolRep where
    existIn vRep (TyProg m cache) = vRep `member` m || vRep `member` cache

instance Existence (TypedProgram a) (Ty.NotedVar a) where
    existIn nVar tp = repOf nVar `existIn` tp

instance KeyFinding (TypedProgram a) (Ty.NotedVar a) (TypedBinding a) where
    kFind nVar tp = kFind (repOf nVar) tp

instance KeyFinding (TypedProgram a) SymbolRep (TypedBinding a) where
    kFind vRep (TyProg m cache) =
        case Map.lookup vRep m of
            res @ (Just _) -> res
            Nothing ->
                {- Looking at the cache to see if it is a mutually recursive symbol. -}
                case Map.lookup vRep cache of
                    Nothing -> Nothing
                    Just vRep' -> Map.lookup vRep' m

instance KeyValUpdate (TypedProgram a) SymbolRep (TypedBinding a) where
    kValUpdate vRep f tp @ (TyProg m cache) =
        if vRep `Map.member` m
        then TyProg (Map.adjust f vRep m) cache
        else case Map.lookup vRep cache of
            Nothing -> tp
            Just vRep' -> TyProg (Map.adjust f vRep' m) cache

instance KeyValUpdate (TypedProgram a) (Ty.NotedVar a) (TypedBinding a) where
    kValUpdate nVar f tp = kValUpdate (repOf nVar) f tp

instance KeyValUpdate (TypedProgram a) SymbolRep (BindingSingleton a) where
    kValUpdate vRep f = kValUpdate vRep builtF
        where
            builtF (TyNonRec bSing) = TyNonRec $ f bSing
            builtF (TyRec bs) = TyRec $ updateRecs bs

            updateRecs [] = []
            updateRecs (bSing @ (nVar', _, _) : t) =
                if repOf nVar' == vRep
                then f bSing : t
                else bSing : updateRecs t

instance KeyValUpdate (TypedProgram a) (Ty.NotedVar a) (BindingSingleton a) where
    kValUpdate nVar f = kValUpdate nVar builtF
        where
            builtF (TyNonRec bSing) = TyNonRec $ f bSing
            builtF (TyRec bs) = TyRec $ updateRecs bs

            updateRecs [] = []
            updateRecs (bSing @ (nVar', _, _) : t) =
                if repOf nVar' == vRep
                then f bSing : t
                else bSing : updateRecs t

            vRep = repOf nVar

instance ListSource (TypedProgram a) (TypedBinding a) where
    toList' (TyProg m _) = elems m
    fromList' l =
        let (m, cache) = foldl' insertSym (empty, empty) l in
            TyProg m cache
        where
            insertSym (m, cache) b @ (TyNonRec (nVar, _, _)) =
                (Map.insert (repOf nVar) b m, cache)
            {- Very strange case, the client should not pass something like this. -}
            insertSym (m, cache) (TyRec []) =
                (m, cache)
            {- Just taking the first element to fetch a symbol name. -}
            insertSym (m, cache) b @ (TyRec bs @ ((nVar, _, _) : _)) =
                let symRep = repOf nVar in
                    (Map.insert symRep b m, updateCache cache bs symRep)

instance Emptiness (InstsTable a) where
    noElems = InstT empty

{- NB: it overwrites the content of the table. -}
instance KeyValueAdder (InstsTable a) (Raw.SymbolName a) [Raw.SDUnion a] where
    kAddElem sym sds (InstT m) =
        let sRep = repOf sym in
            case Map.lookup sRep m of
                Nothing -> InstT $ insert sRep sds m
                Just sds' -> InstT $ insert sRep (sds ++ sds') m

{- It does not overwrite the content of the table, but it accumulates the variables. -}
instance Adder (InstsTable a) (Raw.SDUnion a) where
    addElem sd (InstT m) =
        let symRep = repOf $ Raw.symNameFromSD sd in
            case Map.lookup symRep m of
                Nothing -> InstT $ Map.insert symRep [sd] m
                Just bs -> InstT $ Map.insert symRep (sd : bs) m

instance Existence (InstsTable a) (Raw.SymbolName a) where
    existIn sym (InstT m) = repOf sym `member` m

{- TODO: rm this
instance Existence (InstsTable a) (Symbol, Ty.LangHigherType a) where
    existIn (s, ty) (InstT m) =
        case Map.lookup s m of
            Nothing -> False
            Just vars -> any (\var -> Ty.typeOf var == ty) vars
                        -}

instance KeyFinding (InstsTable a) (Raw.SymbolName a) [Raw.SDUnion a] where
    kFind sym (InstT m) = Map.lookup (repOf sym) m

{- TODO: rm this
instance KeyFinding (InstsTable a) (Symbol, Ty.LangHigherType a) (Ty.NotedVar a) where
    kFind (s, ty) (InstT m) =
        case Map.lookup s m of
            Nothing -> Nothing
            --TODO: equality between types???
            Just vars -> firstThat (\var -> Ty.typeOf var == ty) vars
                        -}

instance AllGetter (InstsTable a) (Raw.SDUnion a) where
    getAllElems (InstT m) = concat $ elems m

instance Emptiness (PropMethodsTable a) where
    noElems = MhtsT empty

instance Existence (PropMethodsTable a) (Ty.NotedVar a) where
    existIn nVar mths = repOf nVar `existIn` mths

instance Existence (PropMethodsTable a) SymbolRep where
    existIn vRep (MhtsT m) = vRep `member` m

instance KeyFinding (PropMethodsTable a) SymbolRep (Ty.NotedVar a) where
    kFind symRep (MhtsT m) = Map.lookup symRep m

instance KeyFinding (PropMethodsTable a) (Ty.NotedVar a) (Ty.NotedVar a) where
    kFind nVar mhts = kFind (repOf nVar) mhts

instance Adder (PropMethodsTable a) (Ty.NotedVar a) where
    addElem nVar (MhtsT m) = MhtsT $ Map.insert (repOf nVar) nVar m

instance Emptiness (ImplTable a) where
    noElems = ImplT empty

{- NB: it overwrites the content of the table. -}
instance KeyValueAdder (ImplTable a) (Ty.LangNewConstraint a) [Ty.LangSpecConstraint a] where
    kAddElem cont cs (ImplT m) = ImplT $ Map.insert (repOf cont) cs m

{- It does not overwrite the content of the table, but it accumulates the constraints. -}
instance Adder (ImplTable a) (Ty.LangSpecConstraint a) where
    addElem c (ImplT m) =
        let pRep = repOf c in
            case Map.lookup pRep m of
                Nothing -> ImplT $ Map.insert pRep [c] m
                Just cs -> ImplT $ Map.insert pRep (c : cs) m

instance Adder (ImplTable a) [Ty.LangSpecConstraint a] where
    addElem cs it =
        foldl' (flip addElem) it cs

instance Existence (ImplTable a) (Ty.LangNewConstraint a) where
    existIn cont (ImplT m) = repOf cont `member` m

{- FIXME
instance Existence (ImplTable a) (Property, Ty.LangSpecConstraint a) where
    existIn (p, c) (ImplT m) =
        case Map.lookup p m of
            Nothing -> False
            Just cs -> any (c `Ty.satisfy`) cs
                        -}

instance KeyFinding (ImplTable a) PropConRep [Ty.LangSpecConstraint a] where
    kFind contRep (ImplT m) = Map.lookup contRep m

instance KeyFinding (ImplTable a) (Ty.LangNewConstraint a) [Ty.LangSpecConstraint a] where
    kFind cont it = kFind (repOf cont) it

{- FIXME
instance KeyFinding (ImplTable a) (Property, Ty.LangSpecConstraint a) [Ty.LangSpecConstraint a] where
    kFind (p, c) (ImplT m) =
        case Map.lookup p m of
            Nothing -> Nothing
            Just cs -> Just $ filter (c `Ty.satisfy`) cs
                        -}

getMatchingConts :: Ty.LangHigherType a -> [Ty.LangSpecConstraint a] -> [Ty.LangSpecConstraint a]
getMatchingConts ty = filter $ any (`Ty.sameBaseOf` ty) . argsOf

{- TODO: legacy
rawGetMatchingConts :: TypeRep -> [Ty.LangSpecConstraint a] -> [Ty.LangSpecConstraint a]
rawGetMatchingConts tyRep = filter $ any (Ty.rawSameBaseOf tyRep) . argsOf
-}

instance KeyFinding (ImplTable a) (Ty.LangNewConstraint a, Ty.LangHigherType a) [Ty.LangSpecConstraint a] where
    kFind (cont, ty) (ImplT m) =
        case Map.lookup (repOf cont) m of
            Nothing -> Nothing
            Just cs -> Just $ getMatchingConts ty cs

instance SafeKeyFinding (ImplTable a) (Ty.LangHigherType a) [Ty.LangSpecConstraint a] where
    kSafeFind ty (ImplT m) =
        concatMap (getMatchingConts ty) $ elems m

{- TODO: legacy
instance SafeKeyFinding (ImplTable a) (Ty.LangHigherType a) [Ty.LangSpecConstraint a] where
    kSafeFind ty (ImplT m) =
        concatMap (rawGetMatchingConts ty) $ elems m
-}

{- Shorthand to find the constraints of function type. -}
contsOfFunType :: ImplTable a -> [Ty.LangSpecConstraint a]
contsOfFunType (ImplT m) =
    case Map.lookup BITy.nameFunctionApp m of
        Nothing -> []
        Just cs -> cs

instance AllGetter (ImplTable a) (Ty.LangSpecConstraint a) where
    getAllElems (ImplT m) = concat $ elems m
