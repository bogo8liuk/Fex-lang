{- |
Module : Compiler.Syntax.Refactoring.Language
Description : Language definition
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Language definition structures: useful to parse lexemes of the language.

Steps for defining a new keyword:

  (1) Define the string lexeme in [lexer module]("Compiler.Config.Lexer")
  (2) If the lexeme is an operator, remember to add it in `reservedOps`,
      while if it is an identifier, remember to add it in `reservedIds`

Steps for updating a new keyword:

  (1) Update the string lexeme in [lexer module]("Compiler.Config.Lexer")
  (2) If the lexeme is an operator, remember to move it in `reservedOps`,
      while if it is an identifier, remember to move it in `reservedIds`

Usually, with \"identifier\" we mean everything starting with a-z, A-Z or _. An
\"operator\" is everything else.
-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.Syntax.Refactoring.Language
    (
    -- * Identifiers and operators
      upperIdentifier
    , lowerIdentifier
    , generalIdentifier
    , reservedIdentifier
    , operator
    , reservedOperator
    -- * Literals
    , charLiteral
    , stringLiteral
    , naturalLiteral
    , integerLiteral
    , floatLiteral
    -- * White spaces
    , skipSemanticless
    , lexeme
    -- * Applications
    , leftApplication
    , leftAutoApplication
    , rightApplication
    , rightAutoApplication
) where

import Compiler.Config.Lexer
import qualified Text.Parsec.Token as Token(GenLanguageDef(..)
    , GenTokenParser(..), makeTokenParser
    )
import Text.Parsec (Stream, ParsecT, (<|>), try, oneOf, between, string, (<?>))
import Data.Text (Text, pack)
import Compiler.Syntax.Refactoring.Lib (nextMustBe, application
    , applicationLast
    )
import Utils.Fancy ((<|))
import Compiler.Syntax.Refactoring.TextLiterals(validCharLiteral
    , validStringLiteral
    )
import Lib.Types(LeftApplication(..), LeftAutoApplication, leftToAutoLeft
    , RightApplication(..), RightAutoApplication, rightToAutoRight
    , buildLeftApplication, buildRightApplication
    )

reservedIds, reservedOps :: [String]
reservedIds =
    [ symbolDeclarationDefKeyword
    , typeThingDefKeyword
    , aliasDefKeyword
    , typeClassDefKeyword
    , instanceDefKeyword
    , matchKeyword
    , matchWithKeyword
    , letDefKeyword
    , postLetDefKeyword
    , letScopeKeyword
    , lambdaDefKeyword
    , signatureDefKeyword
    , mainSymbol
    , trueDataCon
    , falseDataCon
    , compileTimeOpsCategory
    ]
reservedOps =
    [ inlineCommentKeyword
    , multilineStartCommentKeyword
    , multilineEndCommentKeyword
    , lambdaScopeKeyword
    , functionTypeKeyword
    , constraintAppKeyword
    , definitionKeyword
    , defaultPatternKeyword
    , caseSeparationKeyword
    , andSeparationKeyword
    , caseScopeKeyword
    , signatureScopeKeyword
    , stringLitStartKeyword
    , stringLitEndKeyword
    , recordStartKeyword
    , recordEndKeyword
    , varAsOpKeyword
    , endStatementKeyword
    , reservedIdKeyword
    , listTypeStart
    , listTypeEnd
    , listDataConStart
    , listDataConEnd
    , listDataConSep
    , listEmptyDataCon
    , listConsDataCon
    , tupleTypeStart
    , tupleTypeEnd
    , tupleTypeSep
    , tupleDataConStart
    , tupleDataConEnd
    , tupleDataConSep
    , compileTimeEvalStart
    , compileTimeEvalEnd
    ]

identifierStart :: Stream s m Char => ParsecT s u m Char
identifierStart = try identifierStartUpper <|> identifierStartLower

identifierStartUpper :: Stream s m Char => ParsecT s u m Char
identifierStartUpper = oneOf possibleIdsHeadUpper

identifierStartLower :: Stream s m Char => ParsecT s u m Char
identifierStartLower = oneOf possibleIdsHeadLower

identifierTail :: Stream s m Char => ParsecT s u m Char
identifierTail = oneOf possibleIdsTail

operatorStart :: Stream s m Char => ParsecT s u m Char
operatorStart = oneOf possibleOpsHead

operatorTail :: Stream s m Char => ParsecT s u m Char
operatorTail = oneOf possibleOpsTail

definition :: Stream s m Char => Token.GenLanguageDef s u m
definition =
    Token.LanguageDef
        { Token.commentStart = multilineStartCommentKeyword
        , Token.commentEnd = multilineEndCommentKeyword
        , Token.commentLine = inlineCommentKeyword
        , Token.nestedComments = True
        , Token.identStart = identifierStart
        , Token.identLetter = identifierTail
        , Token.opStart = operatorStart
        , Token.opLetter = operatorTail
        , Token.reservedNames = reservedIds
        , Token.reservedOpNames = reservedOps
        , Token.caseSensitive = True
        }

tokenParser :: Stream s m Char => Token.GenTokenParser s u m
tokenParser = Token.makeTokenParser definition

{- |
`identifierStartingWith p` parses a VALID identifier according to the language
checking also that the identifier starts with `p`, then it skips white spaces
and comments.
-}
identifierStartingWith
    :: Stream s m Char
    => ParsecT s u m Char
    -> ParsecT s u m Text
identifierStartingWith p = do
    nextMustBe p
    s <- Token.identifier tokenParser
    return $ pack s

{- |
Identifiers starting with upper-case letter, then it skips white spaces and
comments.
-}
upperIdentifier :: Stream s m Char => ParsecT s u m Text
upperIdentifier = identifierStartingWith identifierStartUpper

{- |
Identifiers starting with lower-case letter then it skips white spaces and
comments.
-}
lowerIdentifier :: Stream s m Char => ParsecT s u m Text
lowerIdentifier = identifierStartingWith identifierStartLower

{- |
It parses a valid identifier then it skips white spaces and comments.
-}
generalIdentifier :: Stream s m Char => ParsecT s u m Text
generalIdentifier = pack <$> Token.identifier tokenParser

{- |
It parses a reserved identifier then it skips white spaces and comments.
-}
reservedIdentifier :: Stream s m Char => String -> ParsecT s u m ()
reservedIdentifier = Token.reserved tokenParser

{- |
It parses a valid operator then it skips white spaces and comments.
-}
operator :: Stream s m Char => ParsecT s u m Text
operator = pack <$> Token.operator tokenParser

{- |
It parses a reserved operator then it skips white spaces and comments.
-}
reservedOperator :: Stream s m Char => String -> ParsecT s u m ()
reservedOperator = Token.reservedOp tokenParser

{- |
It parses a character literal then it skips white spaces and comments.
-}
charLiteral :: Stream s m Char => ParsecT s u m Char
charLiteral =
    Token.lexeme tokenParser
        (  between
        <| string charLitStartKeyword
        <| (string charLitEndKeyword <?> "end of character")
        <| validCharLiteral
        ) <?> "literal character"

{- |
It parses a string literal then it skips white spaces and comments.
-}
stringLiteral :: Stream s m Char => ParsecT s u m Text
stringLiteral = do
    lit <- Token.lexeme tokenParser enclosedStringLiteral <?> "string literal"
    return $ pack lit
    where
        enclosedStringLiteral =
            between
                <| string stringLitStartKeyword
                <| (string stringLitEndKeyword <?> "end of string")
                <| validStringLiteral

{- |
It parses a natural literal then it skips white spaces and comments.
-}
naturalLiteral :: Stream s m Char => ParsecT s u m Integer --TODO: use a Natural data type
naturalLiteral = Token.natural tokenParser

{- |
It parses an integer literal then it skips white spaces and comments.
-}
integerLiteral :: Stream s m Char => ParsecT s u m Integer
integerLiteral = Token.integer tokenParser

{- |
It parses a floating-point number literal then it skips white spaces and
comments.
-}
floatLiteral :: Stream s m Char => ParsecT s u m Double --TODO: is Double correct?
floatLiteral = Token.float tokenParser

{- |
It parses zero or more white spaces and/or comments (both inline and multiline).
-}
skipSemanticless :: Stream s m Char => ParsecT s u m ()
skipSemanticless = Token.whiteSpace tokenParser

{- |
`lexeme p` applies parser `p` then it skips white spaces and comments.
-}
lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme = Token.lexeme tokenParser

{- |
It parses a `LeftApplication applier applied` value, then it skips white spaces
and comments. Each atom in the value is separated from other atoms by white
spaces and/or comments.
-}
leftApplication
    :: Stream s m Char
    => ParsecT s u m (applier a)
    -> ParsecT s u m (applied a)
    -> ParsecT s u m (LeftApplication applier applied a)
leftApplication applier applied = do
    (h, t) <- application <| lexeme applier <| lexeme applied
    return $ buildLeftApplication h t

{- |
It parses a `LeftAutoApplication app` value, then it skips white spaces and
comments. Each atom in the value is separated from other atoms by white spaces
and/or comments.
-}
leftAutoApplication
    :: Stream s m Char
    => ParsecT s u m (app a)
    -> ParsecT s u m (LeftAutoApplication app a)
leftAutoApplication app = do
    leftApp <- leftApplication app app
    return $ leftToAutoLeft leftApp

{- |
It parses a `RightApplication applier applied` value, then it skips white spaces
and comments. Each atom in the value is separated from other atoms by white
spaces and/or comments.
-}
rightApplication
    :: Stream s m Char
    => ParsecT s u m (applier a)
    -> ParsecT s u m (applied a)
    -> ParsecT s u m (RightApplication applier applied a)
rightApplication applier applied = do
    (h, t) <- applicationLast <| lexeme applier <| lexeme applied
    return $ buildRightApplication h t

{- |
It parses a `RightAutoApplication app` value, then it skips white spaces and
comments. Each atom in the value is separated from other atoms by white spaces
and/or comments.
-}
rightAutoApplication
    :: Stream s m Char
    => ParsecT s u m (app a)
    -> ParsecT s u m (RightAutoApplication app a)
rightAutoApplication app = do
    rightApp <- rightApplication app app
    return $ rightToAutoRight rightApp
