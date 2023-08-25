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
) where

import Compiler.Config.Lexer
import qualified Text.Parsec.Token as Token
    ( GenLanguageDef (..)
    , GenTokenParser (..)
    , makeTokenParser
    )
import Text.Parsec (Stream, ParsecT, (<|>), try, oneOf, between, string)
import Data.Text (Text, pack)
import Compiler.Syntax.Refactoring.Lib (nextMustBe)
import Utils.Fancy ((<|))

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

charLiteral :: ParsecT s u m Char
charLiteral = Token.lexeme (between <| string charLitStartKeyword <| string charLitEndKeyword <| character)
    where
        character = charLetter <|> charEscape
