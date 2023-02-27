{-# LANGUAGE FlexibleContexts #-}

{- This file should represent the most low-level module of parsing, from here the other
modules should build more complex parsers. -}
module Compiler.Syntax.Lib.Lex
    ( asciiChar
    , asciiLetter
    , asciiLower
    , asciiUpper
    , manySpaces
    , many1Spaces
    , manyN
    , many2
    , startBy1
    , identifierStart
    , identifierStartUpper
    , identifierStartLower
    , identifierEnd
    , identifier'
    , setState'
    , lexer
) where

import Data.Functor.Identity
import Data.Char
import Text.Parsec
import Text.Parsec.Token as T
import qualified Text.Parsec.Char as Char
import qualified Compiler.Config.Lexer as Keys

--parser that fails if the parsed character is not ASCII (see Data.Char.isAscii)
asciiChar :: ParsecT s u m Char -> ParsecT s u m Char
asciiChar pars = do
    c <- pars
    if not (isAscii c) then parserZero else return c

asciiLetter :: Stream s m Char => ParsecT s u m Char 
asciiLetter = asciiChar letter

asciiLower :: Stream s m Char => ParsecT s u m Char
asciiLower = asciiChar lower

asciiUpper :: Stream s m Char => ParsecT s u m Char
asciiUpper = asciiChar upper

manySpaces :: Stream s m Char => ParsecT s u m ()
manySpaces = spaces

many1Spaces :: Stream s m Char => ParsecT s u m ()
many1Spaces = do
    many1 Char.space
    return ()

manyN :: ParsecT s u m a -> Int -> ParsecT s u m [a]
manyN p n
    | n <= 0 = return []
    | otherwise = doN n
    where
        doN 1 = do
            res <- p
            return [res]
        doN n = do
            h <- p
            t <- doN $ n - 1
            return $ h : t

many2 :: ParsecT s u m a -> ParsecT s u m [a]
many2 p = manyN p 2

{- `startBy p sep` parses one or more occurrences of `p` separated by `sep`, with a starting `sep`. -}
startBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
startBy1 p sep = do
    sep
    p `sepBy1` sep

identifierStart :: Stream s m Char => ParsecT s u m Char
identifierStart = try asciiLetter <|> char '_'

{- identifierStartUpper and identifierStartLower are useful if it comes necessary to distinguish identifiers
with different semantics -}
identifierStartUpper :: Stream s m Char => ParsecT s u m Char
identifierStartUpper = try asciiUpper <|> char '_'

identifierStartLower :: Stream s m Char => ParsecT s u m Char
identifierStartLower = try asciiLower <|> char '_'

identifierEnd :: Stream s m Char => ParsecT s u m Char
identifierEnd = try asciiLetter <|> try digit <|> oneOf "_'"

{- This parser parses a legal identifier, without any trailing whitespaces or other semantic-less stuff.
It is useful to parse function application -}
identifier' :: Stream s m Char => ParsecT s u m String
identifier' = do
    c <- identifierStart
    tail <- many identifierEnd
    return (c : tail)

{- This parser is useful to set a user state and then to continue to use the user state -}
setState' :: Monad m => u -> ParsecT s u m u
setState' st = do
    putState st
    return st

definition :: T.GenLanguageDef String u Identity
definition = T.LanguageDef {
    commentStart = Keys.multilineStartCommentKeyword,
    commentEnd = Keys.multilineEndCommentKeyword,
    commentLine = Keys.inlineCommentKeyword,
    nestedComments = True,
    identStart = identifierStart,
    identLetter = identifierEnd,
    opStart = oneOf Keys.possibleOpsHead,
    opLetter = oneOf Keys.possibleOpsTail,
    reservedNames =
        [ Keys.symbolDeclarationKeyword
        , Keys.lambdaKeyword
        , Keys.lambdaContKeyword
        , Keys.adtKeyword
        , Keys.aliasKeyword
        , Keys.exprBindKeyword
        , Keys.defaultKeyword
        , Keys.matchKeyword
        , Keys.matchWithKeyword
        , Keys.interfaceKeyword
        , Keys.specificsKeyword
        , Keys.instanceKeyword
        , Keys.signatureKeyword
        , Keys.reservedIdKeyword
        ],
    reservedOpNames =
        [ Keys.functionAppKeyword
        , Keys.constraintAppKeyword
        , Keys.definitionKeyword
        , Keys.caseSeparationKeyword
        , Keys.caseThenKeyword
        , Keys.stateDefinitionKeyword
        , Keys.stringLitKeyword
        , Keys.charLitKeyword
        , Keys.tupleConSugarStart
        , Keys.tupleConSugarEnd
        , Keys.tupleConSugarSep
        , Keys.tupleTySugarStart
        , Keys.tupleTySugarEnd
        , Keys.tupleTySugarSep
        , Keys.recordStartKeyword
        , Keys.recordEndKeyword
        , Keys.funAsOpKeyword
        , ";"
        , Keys.endStatementKeyword
        , "..."
        , Keys.andSeparationKeyword
        , Keys.compileTimeStartEval
        , Keys.compileTimeEndEval
        , Keys.reservedIdKeyword
        , Keys.listTySugarStart
        , Keys.listTySugarEnd
        , Keys.listConSugarStart
        , Keys.listConSugarEnd
        , Keys.listConSugarSep
        , Keys.listEmptyCon
        , Keys.listConsCon
        ],
    caseSensitive = True
}

lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser definition
