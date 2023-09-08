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

import Text.Parsec (ParsecT, lookAhead, try, Stream, many, sourceLine, sourceColumn, sourceName)
import Compiler.Lib.Pos (ProgramPos (Pos), PhysicalPos(..))
import Text.ParserCombinators.Parsec (getPosition)

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

{- |
`withPos p` behaves as `p`, returning also the "ProgramPos" which encloses what
`p` parsed.
-}
withPos
    :: Monad m
    => ParsecT s u m a
    -> ParsecT s u m (a, ProgramPos)
withPos parse = do
    -- Getting the starting position
    srcPosStart <- getPosition
    let startLine = sourceLine srcPosStart
    let startColumn = sourceColumn srcPosStart
    -- Consuming the input
    x <- parse
    -- Getting the ending position
    srcPosEnd <- getPosition
    let endLine = sourceLine srcPosEnd
    let endColumn = sourceColumn srcPosEnd
    let srcName = sourceName srcPosEnd
    let startPos = (startLine, startColumn)
    let endPos = (endLine, endColumn)
    let progPos = Pos $ PhysicalPos srcName startPos endPos
    return (x, progPos)

{- |
`track t` behaves like `t` and a `ProgramPos` position value is injected into
the token parsed by `t`. A common usage can be building the token parsed by `t`
with an inner value of type `()`, since it will be discarded by this function.
-}
track
    :: (Functor t, Monad m)
    => ParsecT s u m (t a)
    -- ^ The token parser, it could be thought as `ParsecT s u m (t ())`
    -> ParsecT s u m (t ProgramPos)
track parse = do
    (token, pos) <- withPos parse
    return $ fmap (const pos) token
