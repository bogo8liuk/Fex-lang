{- |
Module : Compiler.Config.Lexer
Description : Lexical tokens
Copyright : (c) Luca Borghi, 2022
License : GPL-3
Stability : experimental

List of lexical tokens of the language. If you change some keyword or you add
a new one, you should update the
[syntax module]("Compiler.Syntax.Refactoring.Language")
-}

module Compiler.Config.Lexer
    (
    -- * Keywords and main tokens
      inlineCommentKeyword
    , multilineStartCommentKeyword
    , multilineEndCommentKeyword
    , symbolDeclarationDefKeyword
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
    , lambdaScopeKeyword
    , signatureDefKeyword
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
    , charLitStartKeyword
    , charLitEndKeyword
    , recordStartKeyword
    , recordEndKeyword
    , varAsOpKeyword
    , reservedIdKeyword
    , mainSymbol
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
    , trueDataCon
    , falseDataCon
    , endStatementKeyword
    -- * Compile time tokens
    , compileTimeEvalStart
    , compileTimeEvalEnd
    -- ** Operators categories
    , compileTimeOpsCategory
    , opsCatIdField
    , opsCatOpsField
    , opsCatGtField
    , opsCatLtField
    , opsCatFixityField
    , opsCatFieldDef
    , opsCatAndSep
    , opsCatFieldEnd
    , opsCatInfixLeftVal
    , opsCatInfixRightVal
    , opsCatInfixNoneVal
    , opsCatPrefixVal
    , opsCatPostfixVal
    -- * Syntax
    -- ** Operators
    , possibleOpsHead
    , possibleOpsTail
    -- ** Identifiers
    , possibleIdsHeadLower
    , possibleIdsHeadUpper
    , possibleIdsTail
) where

inlineCommentKeyword :: String
inlineCommentKeyword = "//"

multilineStartCommentKeyword :: String
multilineStartCommentKeyword = "{*"

multilineEndCommentKeyword :: String
multilineEndCommentKeyword = "*}"

symbolDeclarationDefKeyword :: String
symbolDeclarationDefKeyword = "let"

typeThingDefKeyword :: String
typeThingDefKeyword = "type"

aliasDefKeyword :: String
aliasDefKeyword = "alias"

typeClassDefKeyword :: String
typeClassDefKeyword = "class"

instanceDefKeyword :: String
instanceDefKeyword = "instance"

matchKeyword :: String
matchKeyword = "match"

matchWithKeyword :: String
matchWithKeyword = "with"

letDefKeyword :: String
letDefKeyword = "let"

postLetDefKeyword :: String
postLetDefKeyword = "where"

letScopeKeyword :: String
letScopeKeyword = "in"

lambdaDefKeyword :: String
lambdaDefKeyword = "lam"

lambdaScopeKeyword :: String
lambdaScopeKeyword = "->"

signatureDefKeyword :: String
signatureDefKeyword = "val"

functionTypeKeyword :: String
functionTypeKeyword = "->"

constraintAppKeyword :: String
constraintAppKeyword = "=>"

definitionKeyword :: String
definitionKeyword = "="

defaultPatternKeyword :: String
defaultPatternKeyword = "_"

caseSeparationKeyword :: String
caseSeparationKeyword = "|"

andSeparationKeyword :: String
andSeparationKeyword = ","

caseScopeKeyword :: String
caseScopeKeyword = "->"

signatureScopeKeyword :: String
signatureScopeKeyword = ":"

stringLitStartKeyword :: String
stringLitStartKeyword = "\""

stringLitEndKeyword :: String
stringLitEndKeyword = "\""

charLitStartKeyword :: String
charLitStartKeyword = "'"

charLitEndKeyword :: String
charLitEndKeyword = "'"

recordStartKeyword :: String
recordStartKeyword = "{"

recordEndKeyword :: String
recordEndKeyword = "}"

varAsOpKeyword :: String
varAsOpKeyword = "`"

endStatementKeyword :: String
endStatementKeyword = ";;"

{- |
This token should be unparsable.
-}
reservedIdKeyword :: String
reservedIdKeyword = "_,"

mainSymbol :: String
mainSymbol = "main"

listTypeStart :: String
listTypeStart = "["

listTypeEnd :: String
listTypeEnd = "]"

listDataConStart :: String
listDataConStart = "["

listDataConEnd :: String
listDataConEnd = "]"

listDataConSep :: String
listDataConSep = ","

listEmptyDataCon :: String
listEmptyDataCon = "[]"

listConsDataCon :: String
listConsDataCon = "::"

tupleTypeStart :: String
tupleTypeStart = "("

tupleTypeEnd :: String
tupleTypeEnd = ")"

tupleTypeSep :: String
tupleTypeSep = ","

tupleDataConStart :: String
tupleDataConStart = "("

tupleDataConEnd :: String
tupleDataConEnd = ")"

tupleDataConSep :: String
tupleDataConSep = ","

trueDataCon :: String
trueDataCon = "True"

falseDataCon :: String
falseDataCon = "False"

compileTimeEvalStart :: String
compileTimeEvalStart = "{#"

compileTimeEvalEnd :: String
compileTimeEvalEnd = "#}"

compileTimeOpsCategory :: String
compileTimeOpsCategory = "OPERATORS_CATEGORY"

opsCatIdField :: String
opsCatIdField = "name"

opsCatOpsField :: String
opsCatOpsField = "operators"

opsCatGtField :: String
opsCatGtField = "greater-than"

opsCatLtField :: String
opsCatLtField = "lesser-than"

opsCatFixityField :: String
opsCatFixityField = "fixity"

opsCatFieldDef :: String
opsCatFieldDef = ":"

opsCatAndSep :: String
opsCatAndSep = ","

opsCatFieldEnd :: String
opsCatFieldEnd = ";"

opsCatInfixLeftVal :: String
opsCatInfixLeftVal = "InfixLeft"

opsCatInfixRightVal :: String
opsCatInfixRightVal = "InfixRight"

opsCatInfixNoneVal :: String
opsCatInfixNoneVal = "InfixNone"

opsCatPrefixVal :: String
opsCatPrefixVal = "Prefix"

opsCatPostfixVal :: String
opsCatPostfixVal = "Postfix"

possibleOpsHead :: String
possibleOpsHead = "|!$%&/=?^~+*@#<>.:-"

possibleOpsTail :: String
possibleOpsTail = "|!$%&/=?^~+*@#<>.:-"

possibleIdsHeadLower :: String
possibleIdsHeadLower = "_abcdefghijklmnopqrstuvwxyz"

possibleIdsHeadUpper :: String
possibleIdsHeadUpper = "_ABCDEFGHIJKLMNOPQRSTUVWXYZ"

possibleIdsTail :: String
possibleIdsTail =
  "_'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
