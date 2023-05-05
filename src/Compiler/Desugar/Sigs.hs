{- Module to remove all occurrences of signatures and adding hints to variables,
according to associated signatures. -}
module Compiler.Desugar.Sigs
    ( perform
) where

import Lib.Utils
import Compiler.Ast.Common
import Compiler.Ast.Tree as Raw
import Compiler.State as With
import Data.Map.Strict as Map hiding (map)

type HintMap = Map String (Type With.ProgState)

splitSigs :: [Raw.Declaration With.ProgState] -> ([Raw.Signature With.ProgState], [Raw.Declaration With.ProgState])
splitSigs l = onFst (map unWrap) . splitDecls l $ AOFSome [AOFSig]
    where
        unWrap (Sig d) = d

fetchTypes :: [Raw.Signature With.ProgState] -> HintMap
fetchTypes sigs = __fetch sigs empty
    where
        __fetch [] m = m
        __fetch (s : t) m =
            let ty = Raw.typeFromSig s in
            let sName = repOf $ Raw.symNameFromSig s in
                __fetch t $ insert sName ty m

updateHints :: HintMap -> Raw.AstOp With.ProgState ()
updateHints m = do
    Raw.safeUpdateHintsInHeadSymDecl update
    Raw.safeUpdateHintsInHeadMultiSymDecl update
    where
        update n h =
            let sName = repOf n in
                case Map.lookup sName m of
                    {- This is the case where the symbol name has not been found in the map, but this is not
                    an error, because it is necessary for a variable to have an associated signature.
                    Thus, the hint remains the one which has been run into, namely `h`. -}
                    Nothing -> h
                    Just ty -> Raw.buildHint ty

perform :: Raw.Program With.ProgState -> Raw.Program With.ProgState
perform p =
    let (sigs, ds) = splitSigs $ Raw.declarationsFrom p in
    let m = fetchTypes sigs in
    {- Discarding signatures. -}
    let p' = Raw.buildProgram ds in
        snd . Raw.runAstOp p' $ updateHints m
