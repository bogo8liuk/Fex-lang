{- |
Module : Compiler.Syntax.Refactoring.Lib
Description : Utilities for parsing
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Utilities for parsing. Usually wrappers to `Parsec` library.
-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.Syntax.Refactoring.Lib
    ( nextMustBe
) where
import Text.Parsec (ParsecT, lookAhead, try, Stream)

{- |
`nextMustBe p` pretends `p` to not fail without consuming input regardless it
fails or not.
-}
nextMustBe :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
nextMustBe = try . lookAhead
