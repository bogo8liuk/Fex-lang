module Compiler.Names.Check.Fetch
    ( addNamesFrom
) where

import Lib.Utils
import Lib.Monad.Utils
import Control.Monad.State.Lazy
import Compiler.Ast.Common
import Compiler.Ast.Tree
import Compiler.Names.Check.Lib as Names
import qualified Compiler.State as With

addSymbol :: SymbolName With.ProgState -> Names.Op ()
addSymbol sym = do
    tables <- get
    case Names.insertSymbol (repOf sym) tables of
        Nothing -> Names.err $ DupSym sym
        Just tables' -> put tables'

addAdt :: ADTName With.ProgState -> Names.Op ()
addAdt adt = do
    tables <- get
    case Names.insertAdt (repOf adt) tables of
        Nothing -> Names.err $ DupAdt adt
        Just tables' -> put tables'

addCon :: ADTConName With.ProgState -> Names.Op ()
addCon con = do
    tables <- get
    case Names.insertConstructor (repOf con) tables of
        Nothing -> Names.err $ DupCon con
        Just tables' -> put tables'

addCons :: [ADTConName With.ProgState] -> Names.Op ()
addCons = mapM_ addCon

addProp :: IntfName With.ProgState -> Names.Op ()
addProp prop = do
    tables <- get
    case Names.insertInterface (repOf prop) tables of
        Nothing -> Names.err $ DupIntf prop
        Just tables' -> put tables'

addPropSig :: IntfName With.ProgState -> Signature With.ProgState -> Names.Op ()
addPropSig prop sig = do
    tables <- get
    let sym = symNameFromSig sig
    case Names.insertPropSignature <| repOf prop <| repOf sym <| tables of
        Nothing -> Names.err $ DupSig sym
        Just tables' -> put tables'

addPropSigs :: IntfName With.ProgState -> [Signature With.ProgState] -> Names.Op ()
addPropSigs prop = mapM_ $ addPropSig prop

addSig :: Signature With.ProgState -> Names.Op ()
addSig sig = do
    tables <- get
    let sym = symNameFromSig sig
    case Names.insertSignature (repOf sym) tables of
        Nothing -> Names.err $ DupSig sym
        Just tables' -> put tables'

add :: Declaration With.ProgState -> Names.Op ()
add (Let d) = do
    let sym = symNameFrom d
    addSymbol sym
add (LetMulti d) = do
    let sym = symNameFromMultiSymDecl d
    addSymbol sym
add (ADT d) = do
    let adt = adtNameFrom d
    addAdt adt
    let cons = adtConsNameFrom d
    addCons cons
add (AliasADT d) = do
    let alias = aliasNameFrom d
    addAdt alias
add (Intf d) = do
    let prop = intfNameFrom d
    addProp prop
    let sigs = sigsFromIntf d
    addPropSigs prop sigs
add (Sig d) =
    addSig d
add (Ins _) = doNothing'

addNamesFrom :: [Declaration With.ProgState] -> Names.Op ()
addNamesFrom = mapM_ add
