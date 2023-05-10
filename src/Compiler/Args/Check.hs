module Compiler.Args.Check
    {- Re-exporting ArgsError to pattern match on it. -}
    ( Correct.ArgsError(..)
    , perform
) where

import Compiler.Ast.Tree as Raw
import Compiler.Args.Check.Correct as Correct
import Compiler.State as With

perform :: Raw.Program With.ProgState -> Either ArgsError ((), Raw.Program With.ProgState)
perform p = Raw.runAstOpRes p Correct.argsCheck
