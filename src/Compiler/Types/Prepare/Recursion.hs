module Compiler.Types.Prepare.Recursion
    ( splitRecDefs
) where

import qualified Data.Graph as G
import Compiler.State as With
import qualified Compiler.Ast.Tree as Raw
import Compiler.Types.Tables(PropMethodsTable)
import Compiler.Types.Prepare.Lib

type Node = (Raw.SDUnion With.ProgState, String, [String])

createNodes :: [Raw.SDUnion With.ProgState] -> PropMethodsTable With.ProgState -> [Node]
createNodes sds mhts = map createNode sds
    where
        createNode sd =
            let key = strRep sd in
            let archs = depsOf sd mhts in
                (sd, key, archs)

createSCCs :: [Node] -> [G.SCC (Raw.SDUnion With.ProgState)]
createSCCs = G.stronglyConnComp

createBindings
    :: [G.SCC (Raw.SDUnion With.ProgState)]
    -> [RawBinding]
createBindings = map createBinding
    where
        createBinding (G.AcyclicSCC sd) = RawNonRec sd
        createBinding (G.CyclicSCC sds) = RawRec sds

splitRecDefs :: [Raw.SDUnion With.ProgState] -> PropMethodsTable With.ProgState -> [RawBinding]
splitRecDefs sds mhts =
    let graph = createNodes sds mhts in
    let sccs = createSCCs graph in
        createBindings sccs
