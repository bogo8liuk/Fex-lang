module Compiler.Codegen.Env
    ( TyConStrRep
    , DataConStrRep
    , CodegenErr(..)
    , CodegenEnv
    , initState
    , cgErr
    , runCounterOnly
    , withCounterOnly
    , getCounter
    , getDataCon
    , getTyCon
    , getAllTyCons
    , getMatchNVals
    , getUnique
    , putCounter
    , putDataCon
    , putDataCons
    , putTyCon
    , putUnique
) where

import Lib.Utils
import Lib.Result
import Control.Monad.State.Lazy
import qualified Lib.Counter as C
import Data.Map.Lazy as M hiding (filter)
import Compiler.State as With
import Compiler.Ast.Common
import qualified Compiler.Ast.Typed as Ty
import Compiler.Types.Tables
import DataCon
import TyCon
import OccName
import Unique hiding (getUnique)

data CodegenErr =
      TypePanicErr String
    | PanicErr String

instance InfoShow CodegenErr where
    infoShow (TypePanicErr _) = unexpNoInfo
    infoShow (PanicErr _) = unexpNoInfo

instance DebugShow CodegenErr where
    dbgShow (TypePanicErr reason) = reason
    dbgShow (PanicErr reason) = reason

instance UnreachableState CodegenErr where
    isUnreachable err @ (TypePanicErr _) = Just $ dbgShow err
    isUnreachable err @ (PanicErr _) = Just $ dbgShow err

type TyConStrRep = String
type DataConStrRep = String

type CoreDataConsTable = Map TyConStrRep (Map DataConStrRep DataCon)

type CoreTyConsTable = Map TyConStrRep TyCon

type UniquesTable = Map (String, NameSpace) Unique

data CodegenState =
    CGSt
        {- Useful to create Unique objects. -}
        C.AlphabeticCounterObj
        {- This should be used as a cache, namely: every time a data constructor occurs, we have to look for it
        in the table. If it is found, then it is returned, else a new one has to be created and inserted into the
        table to speed up the future compilation of other data constructors. -}
        CoreDataConsTable
        {- Same of CoreDataConsTable, just a cache for type constructors. -}
        CoreTyConsTable
        {- List of data constructors in the program. -}
        [Ty.NotedVal With.ProgState]
        {- It stores the bindings of symbol (in string representation) along with the `NameSpace` to know the category of
        token and its Unique value, in order to make coherent names. -}
        UniquesTable

initState :: C.AlphabeticCounterObj -> DataConsTable With.ProgState -> CodegenState
initState c dct =
    CGSt c empty empty (getAllElems dct) empty

stGetCounter :: CodegenState -> C.AlphabeticCounterObj
stGetCounter (CGSt c _ _ _ _) = c

stGetDataConsTable :: CodegenState -> CoreDataConsTable
stGetDataConsTable (CGSt _ dct _ _ _) = dct

stGetTyConsTable :: CodegenState -> CoreTyConsTable
stGetTyConsTable (CGSt _ _ tct _ _) = tct

stGetNotedValues :: CodegenState -> [Ty.NotedVal With.ProgState]
stGetNotedValues (CGSt _ _ _ nVals _) = nVals

stGetUniques :: CodegenState -> UniquesTable
stGetUniques (CGSt _ _ _ _ uqs) = uqs

stUpdateCounter :: CodegenState -> C.AlphabeticCounterObj -> CodegenState
stUpdateCounter (CGSt _ dct tct nVals uqs) c = CGSt c dct tct nVals uqs

stUpdateDataConsTable :: CodegenState -> CoreDataConsTable -> CodegenState
stUpdateDataConsTable (CGSt c _ tct nVals uqs) dct = CGSt c dct tct nVals uqs

stUpdateTyConsTable :: CodegenState -> CoreTyConsTable -> CodegenState
stUpdateTyConsTable (CGSt c dct _ nVals uqs) tct = CGSt c dct tct nVals uqs

stUpdateNotedValues :: CodegenState -> [Ty.NotedVal With.ProgState] -> CodegenState
stUpdateNotedValues (CGSt c dct tct _ uqs) nVals = CGSt c dct tct nVals uqs

stUpdateUniques :: CodegenState -> UniquesTable -> CodegenState
stUpdateUniques (CGSt c dct tct nVals _) uqs = CGSt c dct tct nVals uqs

type CodegenEnv = StateT CodegenState (Either CodegenErr)

cgErr :: CodegenErr -> CodegenEnv a
cgErr err = lift $ Left err

runCounterOnly :: CodegenEnv res -> C.AlphabeticCounterObj -> Either CodegenErr (res, C.AlphabeticCounterObj)
runCounterOnly op c =
    case runStateT op $ CGSt c empty empty [] empty of
        Left err -> Left err
        Right (res, st) -> Right (res, stGetCounter st)

withCounterOnly :: CodegenEnv res -> C.AlphabeticCounterObj -> CodegenEnv res
withCounterOnly op c = do
    put $ CGSt c empty empty [] empty
    op

getCounter :: CodegenEnv C.AlphabeticCounterObj
getCounter = gets stGetCounter

getDataCon :: TyConStrRep -> DataConStrRep -> CodegenEnv (Maybe DataCon)
getDataCon tyConRep dataConRep = do
    dct <- gets stGetDataConsTable
    case M.lookup tyConRep dct of
        Nothing -> return Nothing
        Just subm -> return $ M.lookup dataConRep subm

getTyCon :: TyConStrRep -> CodegenEnv (Maybe TyCon)
getTyCon tyConRep = do
    tct <- gets stGetTyConsTable
    return $ M.lookup tyConRep tct

getAllTyCons :: CodegenEnv [TyCon]
getAllTyCons = do
    tct <- gets stGetTyConsTable
    return $ elems tct

getMatchNVals :: String -> CodegenEnv [Ty.NotedVal With.ProgState]
getMatchNVals tyRep = do
    nVals <- gets stGetNotedValues
    filterM matchTyRep nVals
    where
        matchTyRep nVal = do
            ty <- resTypeOf nVal
            Ty.doOnType ty
                (\lspty -> return $ tyRep == strOf lspty)
                (const $ return False)
                (const $ return False)

        noResTyFor nVal = cgErr $ TypePanicErr ("No result type for noted value " ++ strOf nVal)

        resTypeOf nVal =
            case Ty.instantiateUnqualifying (Ty.typeOf nVal) [] of
                Nothing -> noResTyFor nVal
                Just instTy ->
                    case last' $ Ty.unfoldType instTy of
                        Nothing -> noResTyFor nVal
                        Just ty -> return ty

getUniques :: CodegenEnv UniquesTable
getUniques = gets stGetUniques

getUnique :: String -> NameSpace -> CodegenEnv (Maybe Unique)
getUnique rep sp = M.lookup (rep, sp) <$> getUniques

putCounter :: C.AlphabeticCounterObj -> CodegenEnv ()
putCounter c = do
    st <- get
    put $ stUpdateCounter st c 

putDataCon :: TyConStrRep -> DataConStrRep -> DataCon -> CodegenEnv ()
putDataCon tyConRep dataConRep dataCon = do
    st <- get
    let dct = stGetDataConsTable st
    case M.lookup tyConRep dct of
        Nothing ->
            put . stUpdateDataConsTable st $ M.insert tyConRep (fromList [(dataConRep, dataCon)]) dct
        Just m ->
            put . stUpdateDataConsTable st $ M.insert tyConRep (M.insert dataConRep dataCon m) dct

putDataCons :: TyConStrRep -> [(DataConStrRep, DataCon)] -> CodegenEnv ()
putDataCons tyConRep dataCons = do
    st <- get
    let dct = stGetDataConsTable st
    put . stUpdateDataConsTable st $ M.insert tyConRep (fromList dataCons) dct

putTyCon :: TyConStrRep -> TyCon -> CodegenEnv ()
putTyCon tyConRep tyCon = do
    st <- get
    let tct = stGetTyConsTable st
    put . stUpdateTyConsTable st $ M.insert tyConRep tyCon tct

putUnique :: String -> NameSpace -> Unique -> CodegenEnv ()
putUnique rep sp uq = do
    st <- get
    let uqs = stGetUniques st
    put . stUpdateUniques st $ M.insert (rep, sp) uq uqs
