module Compiler.Desugar.Alias.Replace
    ( CycleAliasesErrorInfo
    , AliasSubstRes
    , perform
    , errPrint
) where

import Compiler.Ast.Tree as Raw
import Compiler.Desugar.Alias.Lib.Alias
import Compiler.Desugar.Alias.Lib.Error
import Compiler.State as With

{- Just using a better name of `aliasSubstitution` -}
perform :: Raw.Program With.ProgState -> AliasSubstRes (Raw.Program With.ProgState)
perform = aliasSubstitution
