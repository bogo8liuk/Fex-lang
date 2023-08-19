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

Usually, with identifier we mean everything starting with a-z, A-Z or _. An
operator is everything else.
-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.Syntax.Refactoring.Language
    (
) where

import Compiler.Config.Lexer
import Text.Parsec.Token (GenLanguageDef (..))
import Data.Text (Text)
import Control.Monad.Identity (Identity)
import Text.Parsec (Stream, ParsecT, (<|>), try, oneOf)

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

parsecDefinition :: GenLanguageDef Text u Identity
parsecDefinition =
    LanguageDef
        { commentStart = multilineStartCommentKeyword
        , commentEnd = multilineEndCommentKeyword
        , commentLine = inlineCommentKeyword
        , nestedComments = True
        , identStart = identifierStart
        , identLetter = identifierTail
        , opStart = operatorStart
        , opLetter = operatorTail
        , reservedNames = reservedIds
        , reservedOpNames = reservedOps
        }
