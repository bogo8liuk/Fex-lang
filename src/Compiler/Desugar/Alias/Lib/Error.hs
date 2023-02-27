module Compiler.Desugar.Alias.Lib.Error
    ( errPrint
) where

import Lib.Utils
import Compiler.Ast.Common
import Compiler.Desugar.Alias.Lib.Alias

errPrint :: AliasSubstRes a -> IO ()
errPrint (That aliases) = cyPrint aliases
errPrint None = print "Unreachable state"
errPrint _ = return ()

cyPrint :: CycleAliasesErrorInfo -> IO ()
cyPrint l = do
    print "A cycle among the following aliases has been found:"
    pAlias l
        where
            pAlias [] = return ()
            pAlias (alias : t) = do
                {- TODO: better output, by printing also how aliases have been written. -}
                print $ stateOf alias
                pAlias t
