module Compiler.Types.Prepare
    ( RawBinding(..)
    , SortedBindings
    , splitRecDefs
    , sortDefs
    , depsOf
) where

import Compiler.State as With
import qualified Compiler.Ast.Tree as Raw
import Compiler.Types.Tables(PropMethodsTable)
import Compiler.Types.Prepare.Lib
import qualified Compiler.Types.Prepare.Recursion as Rec
import qualified Compiler.Types.Prepare.Sort as Sort

splitRecDefs :: [Raw.SDUnion With.ProgState] -> PropMethodsTable With.ProgState -> [RawBinding]
splitRecDefs = Rec.splitRecDefs

sortDefs :: [RawBinding] -> PropMethodsTable With.ProgState -> SortedBindings
sortDefs = Sort.sort
