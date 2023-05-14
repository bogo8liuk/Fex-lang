module Compiler.Codegen.Lib
    ( newUniqueVal
    , mkName
    , mkTypeName
    , mkDataConName
    , mkBindingName
    , mkFoolBindingName
) where

import qualified ModuleSys.HsModl as HsModl
import Compiler.State as With
import Compiler.Ast.Common
import qualified Compiler.Desugar.Names as Desugar
import Compiler.Codegen.Env
import Unique hiding (getUnique)
import Name hiding (getSrcSpan)
import SrcLoc

newUniqueVal :: CodegenEnv Unique
newUniqueVal = do
    c <- getCounter
    let (unq, newC) = Desugar.mkUniqueObj c
    putCounter newC
    return unq

getOrMkUniqueVal :: TokenRep -> NameSpace -> CodegenEnv Unique
getOrMkUniqueVal rep sp = do
    mayuq <- getUnique rep sp
    case mayuq of
        Nothing -> do
            uq <- newUniqueVal
            putUnique rep sp uq
            return uq
        Just uq ->
            return uq

getSrcSpan :: ProgState -> CodegenEnv SrcSpan
getSrcSpan st = do
    let startLoc = startSrcLocFromState st
    let endLoc = endSrcLocFromState st
    return $ mkSrcSpan startLoc endLoc

mkName :: NameSpace -> TokenRep -> ProgState -> CodegenEnv Name
mkName nameSpace nameRep st = do
    srcSpan <- getSrcSpan st
    let name = mkOccName nameSpace $ tokenRepToStr nameRep
    unq <- getOrMkUniqueVal nameRep nameSpace
    return $ mkExternalName unq HsModl.topLevelModl name srcSpan

mkTypeName :: TypeRep -> ProgState -> CodegenEnv Name
mkTypeName = mkName tcName

mkDataConName :: DataConRep -> ProgState -> CodegenEnv Name
mkDataConName = mkName srcDataName    --TODO: should I use `dataName` instead of `srcDataName`???

mkBindingName :: SymbolRep -> ProgState -> CodegenEnv Name
mkBindingName = mkName varName

mkSystemIOName :: TokenRep -> ProgState -> NameSpace -> CodegenEnv Name
mkSystemIOName nameRep st nameSpace = undefined

{- It creates a new name which should be useless in the program. -}
mkFoolBindingName :: ProgState -> CodegenEnv Name
mkFoolBindingName st = do
    c <- getCounter
    let (varRep, newC) = Desugar.mkProgUniqueName c
    putCounter newC
    mkBindingName varRep st