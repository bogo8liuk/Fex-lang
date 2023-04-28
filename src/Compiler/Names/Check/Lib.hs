module Compiler.Names.Check.Lib
    ( Keeper
    , new
    , insertSymbol
    , insertAdt
    , insertInterface
    , insertConstructor
    , insertSignature
    , insertPropSignature
    , isSymbolMember
    , isAdtMember
    , isInterfaceMember
    , isConstructorMember
    , isSignatureMember
    , isSignaturePropMember
    , getPropMethodsRep
    , isAnyPropMethod
    , Op
    , Err(..)
    , err
) where

import Lib.Result
import Data.Map.Strict as Map
import Control.Monad.State.Lazy
import Compiler.State as With
import Compiler.Ast.Common
import Compiler.Ast.Tree

data Keeper =
    Tables
        { symbolTable :: Map SymbolNameRep ()
        , adtTable :: Map ADTNameRep ()
        , adtConTable :: Map ADTConNameRep ()
        , sigsTable :: Map SymbolNameRep ()
        , sigsIntfTable :: Map PropNameRep (Map SymbolNameRep ())
        }

instance Show Keeper where
    show tables =
        "+--- SYMBOLS ---+\n" ++
        show (symbolTable tables) ++
        "\n+--- ADTs ---+\n" ++
        show (adtTable tables) ++
        "\n+--- CONSTRUCTORS ---+\n" ++
        show (adtConTable tables) ++
        "\n+--- INTERFACES and SIGNATURES ---+\n" ++
        show (sigsIntfTable tables)

insertNonExistent :: (Ord k) => k -> v -> Map k v -> Maybe (Map k v)
insertNonExistent key val m =
    if key `member` m
    then Nothing
    else Just $ insert key val m

new :: Keeper
new =
    Tables
        { symbolTable = empty
        , adtTable = empty
        , adtConTable = empty
        , sigsTable = empty
        , sigsIntfTable = empty
        }

insertSymbol :: SymbolNameRep -> Keeper -> Maybe Keeper
insertSymbol s tables =
    case insertNonExistent s () $ symbolTable tables of
        Just m ->
            Just $ tables
                { symbolTable = m
                }
        Nothing -> Nothing

insertAdt :: ADTNameRep -> Keeper -> Maybe Keeper
insertAdt t tables =
    case insertNonExistent t () $ adtTable tables of
        Just m ->
            Just $ tables
                { adtTable = m
                }
        Nothing -> Nothing

insertInterface :: PropNameRep -> Keeper -> Maybe Keeper
insertInterface i tables =
    case insertNonExistent i empty $ sigsIntfTable tables of
        Just m ->
            Just $ tables
                { sigsIntfTable = m
                }
        Nothing -> Nothing

insertConstructor :: ADTConNameRep -> Keeper -> Maybe Keeper
insertConstructor c tables =
    case insertNonExistent c () $ adtConTable tables of
        Just m ->
            Just $ tables
                { adtConTable = m
                }
        Nothing -> Nothing

insertSignature :: SymbolNameRep -> Keeper -> Maybe Keeper
insertSignature s tables =
    case insertNonExistent s () $ sigsTable tables of
        Just m ->
            Just $ tables
                { sigsTable = m
                }
        Nothing -> Nothing

insertPropSignature :: PropNameRep -> SymbolNameRep -> Keeper -> Maybe Keeper
insertPropSignature i s tables =
    let sigsIntf = sigsIntfTable tables in
        case Map.lookup i sigsIntf of
            Nothing -> Nothing
            Just propSigsTable ->
                case insertNonExistent s () propSigsTable of
                    Just m ->
                        let tables' =
                             tables
                                { sigsIntfTable = insert i m sigsIntf
                                } in
                            insertSignature s tables'
                    Nothing -> Nothing

isSymbolMember :: SymbolNameRep -> Keeper -> Bool
isSymbolMember s ts = member s $ symbolTable ts

isAdtMember :: ADTNameRep -> Keeper -> Bool
isAdtMember t ts = member t $ adtTable ts

isInterfaceMember :: PropNameRep -> Keeper -> Bool
isInterfaceMember i ts = member i $ sigsIntfTable ts

isConstructorMember :: ADTConNameRep -> Keeper -> Bool
isConstructorMember c ts = member c $ adtConTable ts

isSignatureMember :: SymbolNameRep -> Keeper -> Bool
isSignatureMember s ts = member s $ sigsTable ts

isSignaturePropMember :: PropNameRep -> SymbolNameRep -> Keeper -> Bool
isSignaturePropMember i s ts =
    case Map.lookup i $ sigsIntfTable ts of
        Nothing -> False
        Just propSigsTable -> member s propSigsTable

getPropMethodsRep :: PropNameRep -> Keeper -> Maybe [SymbolNameRep]
getPropMethodsRep p ts =
    case Map.lookup p $ sigsIntfTable ts of
        Nothing -> Nothing
        Just m -> Just $ keys m

isAnyPropMethod :: SymbolNameRep -> Keeper -> Bool
isAnyPropMethod s ts =
    let ms = toList $ sigsIntfTable ts in
        any (\(_, t) -> s `member` t) ms

type Op = StateT Keeper (Either Err)

data Err =
      DupAdt (ADTName With.ProgState)
    | DupSym (SymbolName With.ProgState)
    | DupSymProp (SymbolName With.ProgState)
    | DupSymArgs (SymbolName With.ProgState)
    | DupSymMatchExpr (SymbolName With.ProgState)
    | DupCon (ADTConName With.ProgState)
    | DupIntf (IntfName With.ProgState)
    | DupSig (SymbolName With.ProgState)
    | NoAdt (ADTName With.ProgState)
    | NoCon (ADTConName With.ProgState)
    | UnboundParamType (ParamTypeName With.ProgState)
    | NoSym (SymbolName With.ProgState)
    | NoSymSig (SymbolName With.ProgState)
    | NoProp (IntfName With.ProgState)
    | NoImpl SymbolNameRep (Instance With.ProgState)
    | UnreachableState String

instance InfoShow Err where
    infoShow (DupAdt adt) = "Type " ++ repOf adt ++ " at " ++ show (stateOf adt) ++ " already exists"
    infoShow (DupSym sym) = "Symbol " ++ repOf sym ++ " at "++ show (stateOf sym) ++ " already exists"
    infoShow (DupSymProp sym) =
        "Symbol " ++ repOf sym ++ " at " ++ show (stateOf sym) ++ " already exists as property method"
    infoShow (DupSymArgs sym) =
        "Multiple occurrences in arguments of symbol " ++ repOf sym ++ " at " ++ show (stateOf sym)
    infoShow (DupSymMatchExpr sym) =
        "Multiple occurrences in case expressions of symbol " ++ repOf sym ++ " at " ++ show (stateOf sym)
    infoShow (DupCon con) = "Type constructor " ++ repOf con ++ show (stateOf con) ++ " already exists"
    infoShow (DupIntf prop) = "Property " ++ repOf prop ++ " at " ++ show (stateOf prop) ++ " already exists"
    infoShow (DupSig sym) =
        "Signature for symbol " ++ repOf sym ++ " at " ++ show (stateOf sym) ++ " already exists"
    infoShow (NoAdt adt) = "Unknown type " ++ repOf adt ++ " at " ++ show (stateOf adt)
    infoShow (NoCon con) = "Unknown data constructor " ++ repOf con ++ " at " ++ show (stateOf con)
    infoShow (UnboundParamType pty) = "Unbound type variable " ++ repOf pty ++ " at " ++ show (stateOf pty)
    infoShow (NoSym sym) = "Unknown symbol " ++ repOf sym ++ " at " ++ show (stateOf sym)
    infoShow (NoSymSig sym) = "No implementation for " ++ repOf sym ++ " at " ++ show (stateOf sym)
    infoShow (NoProp prop) = "No property with name " ++ repOf prop ++ " at " ++ show (stateOf prop)
    infoShow (NoImpl symRep inst) =
        "No implementation for method " ++ symRep ++ " in instance at " ++ show (stateOf inst)
    infoShow (UnreachableState _) = unexpNoInfo

instance DebugShow Err where
    dbgShow e @ (DupAdt _) = infoShow e
    dbgShow e @ (DupSym _) = infoShow e
    dbgShow e @ (DupSymProp _) = infoShow e
    dbgShow e @ (DupSymArgs _) = infoShow e
    dbgShow e @ (DupSymMatchExpr _) = infoShow e
    dbgShow e @ (DupCon _) = infoShow e
    dbgShow e @ (DupIntf _) = infoShow e
    dbgShow e @ (DupSig _) = infoShow e
    dbgShow e @ (NoAdt _) = infoShow e
    dbgShow e @ (NoCon _) = infoShow e
    dbgShow e @ (UnboundParamType _) = infoShow e
    dbgShow e @ (NoSym _) = infoShow e
    dbgShow e @ (NoSymSig _) = infoShow e
    dbgShow e @ (NoProp _) = infoShow e
    dbgShow e @ (NoImpl _ _) = infoShow e
    dbgShow (UnreachableState reason) = reason

instance UnreachableState Err where
    isUnreachable (DupAdt _) = Nothing
    isUnreachable (DupSym _) = Nothing
    isUnreachable (DupSymProp _) = Nothing
    isUnreachable (DupSymArgs _) = Nothing
    isUnreachable (DupSymMatchExpr _) = Nothing
    isUnreachable (DupCon _) = Nothing
    isUnreachable (DupIntf _) = Nothing
    isUnreachable (DupSig _) = Nothing
    isUnreachable (NoAdt _) = Nothing
    isUnreachable (NoCon _) = Nothing
    isUnreachable (UnboundParamType _) = Nothing
    isUnreachable (NoSym _) = Nothing
    isUnreachable (NoSymSig _) = Nothing
    isUnreachable (NoProp _) = Nothing
    isUnreachable (NoImpl _ _) = Nothing
    isUnreachable e @ (UnreachableState _) = Just $ dbgShow e

err :: Err -> Op a
err e = lift $ Left e
