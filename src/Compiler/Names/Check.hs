module Compiler.Names.Check
    ( Names.Err(..)
    , perform
) where

import Control.Monad.State.Lazy
import Compiler.Ast.Tree as Raw
import Compiler.Names.Check.Lib as Names
import Compiler.Names.Check.Fetch
import Compiler.Names.Check.Correct
import Compiler.State as With

doCheck :: Raw.Program With.ProgState -> Names.Op ()
doCheck p = do
    let decls = Raw.declarationsFrom p
    addNamesFrom decls
    namesCheck decls

perform :: Raw.Program With.ProgState -> Either Names.Err ()
perform p = evalStateT (doCheck p) Names.new
