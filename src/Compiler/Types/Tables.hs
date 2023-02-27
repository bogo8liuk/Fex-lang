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

{- These aliases are just for a better readability. -}
type Symbol = String
type Type = String
type Constructor = String
type Constraint = String
type Property = String

newtype TypesTable a = TyT (Map Type (Ty.LangNewType a))

newtype DataConsTable a = ConT (Map Constructor (Ty.NotedVal a))

newtype ConstraintsTable a = ContsT (Map Constraint (Ty.LangNewConstraint a, [Ty.LangSpecConstraint a]))

newtype InstsTable a = InstT (Map Symbol [Raw.SDUnion a])

newtype PropMethodsTable a = MhtsT (Map Symbol (Ty.NotedVar a))

newtype ImplTable a = ImplT (Map Property [Ty.LangSpecConstraint a])

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
type RecSymsCache = Map Symbol Symbol
data TypedProgram a = TyProg (Map Symbol (TypedBinding a)) RecSymsCache

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
    addElem lnty (TyT t) = TyT $ insert (strOf lnty) lnty t

instance Existence (TypesTable a) Type where
    existIn s (TyT t) = s `member` t

instance KeyFinding (TypesTable a) Type (Ty.LangNewType a) where
    kFind s (TyT t) = Map.lookup s t

instance AllGetter (TypesTable a) (Ty.LangNewType a) where
    getAllElems (TyT t) = getValues t

instance Emptiness (DataConsTable a) where
    noElems = ConT empty

instance Adder (DataConsTable a) (Ty.NotedVal a) where
    addElem con (ConT t) = ConT $ insert (strOf con) con t

instance Existence (DataConsTable a) Constructor where
    existIn s (ConT t) = s `member` t

instance KeyFinding (DataConsTable a) Constructor (Ty.NotedVal a) where
    kFind s (ConT t) = Map.lookup s t

instance AllGetter (DataConsTable a) (Ty.NotedVal a) where
    getAllElems (ConT t) = getValues t

instance Emptiness (ConstraintsTable a) where
    noElems = ContsT empty

instance Adder (ConstraintsTable a) (Ty.LangNewConstraint a) where
    addElem cont (ContsT t) = ContsT $ insert (strOf cont) (cont, []) t

instance Adder (ConstraintsTable a) (Ty.LangNewConstraint a, [Ty.LangSpecConstraint a]) where
    addElem contCs @ (cont, _) (ContsT t) = ContsT $ insert (strOf cont) contCs t

instance Existence (ConstraintsTable a) Constraint where
    existIn s (ContsT t) = s `member` t

instance KeyFinding (ConstraintsTable a) Constraint (Ty.LangNewConstraint a, [Ty.LangSpecConstraint a]) where
    kFind s (ContsT t) = Map.lookup s t

instance KeyValUpdate' (ConstraintsTable a) Constraint [Ty.LangSpecConstraint a] where
    kValUpdate' cont cs table @ (ContsT t) =
        case Map.lookup cont t of
            Nothing -> table
            Just (lnc, lscs) -> ContsT $ Map.insert cont (lnc, cs ++ lscs) t

instance AllGetter (ConstraintsTable a) (Ty.LangNewConstraint a, [Ty.LangSpecConstraint a]) where
    getAllElems (ContsT t) = getValues t

instance Emptiness (TypedProgram a) where
    noElems = TyProg empty empty

updateCache :: RecSymsCache -> [BindingSingleton a] -> Symbol -> RecSymsCache
updateCache cache bs nVarRep =
    let symReps = map (strOf . fst') bs in
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

findNotInCache :: [BindingSingleton a] -> RecSymsCache -> Maybe Symbol
findNotInCache [] _ = Nothing
findNotInCache ((v, _, _) : t) cache =
    let nVarRep = strOf v in
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
        let nVarRep = strOf v in
            if nVarRep `Map.member` m
            then tp
            else TyProg (insert nVarRep (TyNonRec b) m) cache

instance Existence (TypedProgram a) Symbol where
    existIn v (TyProg m cache) = v `member` m || v `member` cache

instance KeyFinding (TypedProgram a) Symbol (TypedBinding a) where
    kFind v (TyProg m cache) =
        case Map.lookup v m of
            res @ (Just _) -> res
            Nothing ->
                {- Looking at the cache to see if it is a mutually recursive symbol. -}
                case Map.lookup v cache of
                    Nothing -> Nothing
                    Just nVarRep -> Map.lookup nVarRep m

instance KeyValUpdate (TypedProgram a) Symbol (TypedBinding a) where
    kValUpdate v f tp @ (TyProg m cache) =
        if v `Map.member` m
        then TyProg (Map.adjust f v m) cache
        else case Map.lookup v cache of
            Nothing -> tp
            Just nVarRep -> TyProg (Map.adjust f nVarRep m) cache

instance KeyValUpdate (TypedProgram a) Symbol (BindingSingleton a) where
    kValUpdate v f = kValUpdate v builtF
        where
            builtF (TyNonRec bSing) = TyNonRec $ f bSing
            builtF (TyRec bs) = TyRec $ updateRecs bs

            updateRecs [] = []
            updateRecs (bSing @ (nVar, _, _) : t) =
                if strOf nVar == v
                then f bSing : t
                else bSing : updateRecs t

instance ListSource (TypedProgram a) (TypedBinding a) where
    toList' (TyProg m _) = elems m
    fromList' l =
        let (m, cache) = foldl' insertSym (empty, empty) l in
            TyProg m cache
        where
            insertSym (m, cache) b @ (TyNonRec (nVar, _, _)) =
                (Map.insert (strOf nVar) b m, cache)
            {- Very strange case, the client should not pass something like this. -}
            insertSym (m, cache) (TyRec []) =
                (m, cache)
            {- Just taking the first element to fetch a symbol name. -}
            insertSym (m, cache) b @ (TyRec bs @ ((nVar, _, _) : _)) =
                let symRep = strOf nVar in
                    (Map.insert symRep b m, updateCache cache bs symRep)

instance Emptiness (InstsTable a) where
    noElems = InstT empty

{- NB: it overwrites the content of the table. -}
instance KeyValueAdder (InstsTable a) Symbol [Raw.SDUnion a] where
    kAddElem s sds (InstT m) =
        case Map.lookup s m of
            Nothing -> InstT $ insert s sds m
            Just sds' -> InstT $ insert s (sds ++ sds') m

{- It does not overwrite the content of the table, but it accumulates the variables. -}
instance Adder (InstsTable a) (Raw.SDUnion a) where
    addElem sd (InstT m) =
        let symRep = strOf $ Raw.symNameFromSD sd in
            case Map.lookup symRep m of
                Nothing -> InstT $ Map.insert symRep [sd] m
                Just bs -> InstT $ Map.insert symRep (sd : bs) m

instance Existence (InstsTable a) Symbol where
    existIn s (InstT m) = s `member` m

{- TODO: rm this
instance Existence (InstsTable a) (Symbol, Ty.LangHigherType a) where
    existIn (s, ty) (InstT m) =
        case Map.lookup s m of
            Nothing -> False
            Just vars -> any (\var -> Ty.typeOf var == ty) vars
                        -}

instance KeyFinding (InstsTable a) Symbol [Raw.SDUnion a] where
    kFind s (InstT m) = Map.lookup s m

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

instance Existence (PropMethodsTable a) Symbol where
    existIn s (MhtsT m) = s `member` m

instance KeyFinding (PropMethodsTable a) Symbol (Ty.NotedVar a) where
    kFind s (MhtsT m) = Map.lookup s m

instance Adder (PropMethodsTable a) (Ty.NotedVar a) where
    addElem nVar (MhtsT m) = MhtsT $ Map.insert (strOf nVar) nVar m

instance Emptiness (ImplTable a) where
    noElems = ImplT empty

{- NB: it overwrites the content of the table. -}
instance KeyValueAdder (ImplTable a) Property [Ty.LangSpecConstraint a] where
    kAddElem p cs (ImplT m) = ImplT $ Map.insert p cs m

{- It does not overwrite the content of the table, but it accumulates the constraints. -}
instance Adder (ImplTable a) (Ty.LangSpecConstraint a) where
    addElem c (ImplT m) =
        let pRep = strOf c in
            case Map.lookup pRep m of
                Nothing -> ImplT $ Map.insert pRep [c] m
                Just cs -> ImplT $ Map.insert pRep (c : cs) m

instance Adder (ImplTable a) [Ty.LangSpecConstraint a] where
    addElem cs it =
        foldl' (flip addElem) it cs

instance Existence (ImplTable a) Property where
    existIn p (ImplT m) = p `member` m

{- FIXME
instance Existence (ImplTable a) (Property, Ty.LangSpecConstraint a) where
    existIn (p, c) (ImplT m) =
        case Map.lookup p m of
            Nothing -> False
            Just cs -> any (c `Ty.satisfy`) cs
                        -}

instance KeyFinding (ImplTable a) Property [Ty.LangSpecConstraint a] where
    kFind p (ImplT m) = Map.lookup p m

{- FIXME
instance KeyFinding (ImplTable a) (Property, Ty.LangSpecConstraint a) [Ty.LangSpecConstraint a] where
    kFind (p, c) (ImplT m) =
        case Map.lookup p m of
            Nothing -> Nothing
            Just cs -> Just $ filter (c `Ty.satisfy`) cs
                        -}

getMatchingConts :: Ty.LangHigherType a -> [Ty.LangSpecConstraint a] -> [Ty.LangSpecConstraint a]
getMatchingConts ty = filter $ any (`Ty.sameBaseOf` ty) . argsOf

rawGetMatchingConts :: String -> [Ty.LangSpecConstraint a] -> [Ty.LangSpecConstraint a]
rawGetMatchingConts tyRep = filter $ any (Ty.rawSameBaseOf tyRep) . argsOf

instance KeyFinding (ImplTable a) (Property, Ty.LangHigherType a) [Ty.LangSpecConstraint a] where
    kFind (p, ty) (ImplT m) =
        case Map.lookup p m of
            Nothing -> Nothing
            Just cs -> Just $ getMatchingConts ty cs

instance SafeKeyFinding (ImplTable a) (Ty.LangHigherType a) [Ty.LangSpecConstraint a] where
    kSafeFind ty (ImplT m) =
        concatMap (getMatchingConts ty) $ elems m

instance SafeKeyFinding (ImplTable a) Type [Ty.LangSpecConstraint a] where
    kSafeFind ty (ImplT m) =
        concatMap (rawGetMatchingConts ty) $ elems m

{- Shorthand to find the constraints of function type. -}
contsOfFunType :: ImplTable a -> [Ty.LangSpecConstraint a]
contsOfFunType = kSafeFind BITy.nameFunctionApp

instance AllGetter (ImplTable a) (Ty.LangSpecConstraint a) where
    getAllElems (ImplT m) = concat $ elems m
