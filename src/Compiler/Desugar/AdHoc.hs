module Compiler.Desugar.AdHoc
    ( staticDispatch
) where

import Compiler.State as With
import Compiler.Types.Tables
import Compiler.Types.Make(BindingToRefine)
import Compiler.Desugar.AdHoc.Lib
import Compiler.Desugar.AdHoc.Accumulate
import Compiler.Desugar.AdHoc.Create

staticDispatch
    :: TypedProgram With.ProgState
    -> PropMethodsTable With.ProgState
    -> [BindingToRefine]
    -> Either DispatchErr (TypedProgram With.ProgState)
staticDispatch tp mhts bstr =
    case bindingsToDispatch tp mhts bstr of
        Left err -> Left err
        Right (repls, tp') -> instantiateBindings repls tp' mhts
