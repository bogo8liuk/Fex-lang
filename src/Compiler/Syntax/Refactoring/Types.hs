{- |
Module : Compiler.Syntax.Refactoring.Types
Description : Types for parsing
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Utility types for parsing.
-}

module Compiler.Syntax.Refactoring.Types
    ( LangParser
) where
import Text.Parsec (Parsec)
import Data.Text (Text)

{- |
A `LangParser` is a special case of `Parsec` where the stream is a `Text` and
the user state is empty.
-}
type LangParser = Parsec Text ()
