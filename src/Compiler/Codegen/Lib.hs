module Compiler.Codegen.Lib
    ( mkUniqueVal
    , mkName
    , mkTypeName
    , mkDataConName
    , mkBindingName
    , mkFoolBindingName
) where

import qualified ModuleSys.HsModl as HsModl
import Compiler.State as With
import qualified Compiler.Desugar.Names as Desugar
import Compiler.Codegen.Env
import Unique
import Name
import SrcLoc

mkUniqueVal :: CodegenEnv Unique
mkUniqueVal = do
    c <- getCounter
    let (unq, newC) = Desugar.mkUniqueObj c
    putCounter newC
    return unq

mkName :: NameSpace -> String -> ProgState -> CodegenEnv Name
mkName nameSpace nameRep st = do
    let startLoc = startSrcLocFromState st
    let endLoc = endSrcLocFromState st
    let srcSpan = mkSrcSpan startLoc endLoc
    let name = mkOccName nameSpace nameRep
    unq <- mkUniqueVal
    return $ mkExternalName unq HsModl.topLevelModl name srcSpan

mkTypeName :: String -> ProgState -> CodegenEnv Name
mkTypeName = mkName tcName

mkDataConName :: String -> ProgState -> CodegenEnv Name
mkDataConName = mkName srcDataName    --TODO: should I use `dataName` instead of `srcDataName`???

mkBindingName :: String -> ProgState -> CodegenEnv Name
mkBindingName = mkName varName

{- It creates a new name which should be useless in the program. -}
mkFoolBindingName :: ProgState -> CodegenEnv Name
mkFoolBindingName st = do
    c <- getCounter
    let (varRep, newC) = Desugar.mkProgUniqueName c
    putCounter newC
    mkBindingName varRep st