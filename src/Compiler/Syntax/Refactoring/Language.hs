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
import Text.Parsec.Token (GenLanguageDef (..), GenTokenParser (..), makeTokenParser)
import Text.Parsec (Stream, ParsecT, (<|>), try, oneOf, many, lookAhead)
import Data.Text (Text, pack)

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

definition :: Stream s m Char => GenLanguageDef s u m
definition =
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
        , caseSensitive = True
        }

tokenParser :: Stream s m Char => GenTokenParser s u m
tokenParser = makeTokenParser definition

{-
{- |
A more fine-grained token parser than "GenTokenParser".
-}
data LanguageParser s u m
    = LanguageParser
        {
        {- |
        Identifiers starting with upper-case letter.
        -}
          upperIdentifier :: ParsecT s u m Text
        {- |
        Identifiers starting with lower-case letter.
        -}
        , lowerIdentifier :: ParsecT s u m Text
        {- |
        This should be the general case which gathers "upperIdentifier" and
        "lowerIdentifier", so a possible implementation should be:

        > generalIdentifier = try upperIdentifier <|> lowerIdentifier
        -}
        , generalIdentifier :: ParsecT s u m Text
        {- |
        Words not usable as identifiers.
        -}
        , reservedIdentifier :: ParsecT s u m Text
        {- |
        An operator.
        -}
        , operator :: ParsecT s u m Text
        {- |
        Words not usable as operators.
        -}
        , reservedOperator :: ParsecT s u m Text
        }
        -}

{- |
`identifierStartingWith p` parses a VALID identifier according to the language
checking also that the identifier starts with `p`.
-}
identifierStartingWith
    :: Stream s m Char
    => ParsecT s u m Char
    -> ParsecT s u m Text
identifierStartingWith p = do
    try $ lookAhead p
    s <- identifier tokenParser
    return $ pack s

{- |
Identifiers starting with upper-case letter.
-}
upperIdentifier :: Stream s m Char => ParsecT s u m Text
upperIdentifier = identifierStartingWith identifierStartUpper

{- |
Identifiers starting with lower-case letter.
-}
lowerIdentifier :: Stream s m Char => ParsecT s u m Text
lowerIdentifier = identifierStartingWith identifierStartLower
