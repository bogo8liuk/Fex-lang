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
    -- * Applications
    , application
    , applicationSepBy
    , applicationLast
) where

import Text.Parsec (ParsecT, lookAhead, try, Stream, many)

{- |
`nextMustBe p` pretends `p` to not fail without consuming input regardless it
fails or not.
-}
nextMustBe :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
nextMustBe = try . lookAhead

{- |
`application p q` parses `p` followed by `many q`.
-}
application
    :: ParsecT s u m a
    -> ParsecT s u m b
    -> ParsecT s u m (a, [b])
application applier applied = do
    h <- applier
    t <- many applied
    return (h, t)

{- |
`applicationSepBy sep p q` parses `p` followed by `many q`, where each `q` is
preceeded by `sep`.
-}
applicationSepBy
    :: ParsecT s u m sep
    -> ParsecT s u m a
    -> ParsecT s u m b
    -> ParsecT s u m (a, [b])
applicationSepBy sep applier applied = do
    h <- applier
    t <- many applied'
    return (h, t)
    where
        applied' = sep >> applied

{- |
`applicationLast p q` parses `many p` followed `q`
-}
applicationLast
    :: ParsecT s u m a
    -> ParsecT s u m b
    -> ParsecT s u m ([a], b)
applicationLast applier applied = do
    h <- many applier
    t <- applied
    return (h, t)
