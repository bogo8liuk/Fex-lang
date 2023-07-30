{- |
Module : Compiler.Syntax.Refactoring.Lex
Description : Language definition
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Language definition structures: useful to parse lexemes of the language.

Steps for defining a new keyword:

  (1) Define the string lexeme in [lexer module]("Compiler.Config.Lexer")
  (2) If the lexeme is an operator, remember to add it in `reservedOps`,
      while if it is an identifier, remember to add it in `reservedIds`
-}

module Compiler.Syntax.Refactoring.Lex
    (
) where

import Compiler.Config.Lexer

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
