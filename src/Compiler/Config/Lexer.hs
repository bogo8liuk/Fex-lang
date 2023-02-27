--Keywords
module Compiler.Config.Lexer
    ( inlineCommentKeyword
    , multilineStartCommentKeyword
    , multilineEndCommentKeyword
    , symbolDeclarationKeyword
    , adtKeyword
    , aliasKeyword
    , interfaceKeyword
    , instanceKeyword
    , matchKeyword
    , matchWithKeyword
    , specificsKeyword
    , exprBindKeyword
    , lambdaKeyword
    , signatureKeyword
    , functionAppKeyword
    , constraintAppKeyword
    , definitionKeyword
    , defaultKeyword
    , caseSeparationKeyword
    , andSeparationKeyword
    , caseThenKeyword
    , stateDefinitionKeyword
    , stringLitKeyword
    , charLitKeyword
    , recordStartKeyword
    , recordEndKeyword
    , funAsOpKeyword
    , reservedIdKeyword
    , listTySugarStart
    , listTySugarEnd
    , listConSugarStart
    , listConSugarEnd
    , listConSugarSep
    , listEmptyCon
    , listConsCon
    , tupleTySugarStart
    , tupleTySugarEnd
    , tupleTySugarSep
    , tupleConSugarStart
    , tupleConSugarEnd
    , tupleConSugarSep
    , trueCon
    , falseCon
    , endStatementKeyword
    , compileTimeStartEval
    , compileTimeEndEval
    , compileTimeOpsCategory
    , OpsCategoryRecord     --must be abstract, the user does not have the right to see it or create a new one
    , fieldName
    , fieldOps
    , fieldGt
    , fieldLt
    , fieldFixity
    , def
    , listSep
    , end
    , opsCategoryKeywords
    , infixLeftKey
    , infixRightKey
    , infixNoneKey
    , prefixKey
    , postfixKey
    , possibleOpsHead
    , possibleOpsTail
) where

-- KEYWORDS

inlineCommentKeyword :: String
inlineCommentKeyword = "//"

multilineStartCommentKeyword :: String
multilineStartCommentKeyword = "{*"

multilineEndCommentKeyword :: String
multilineEndCommentKeyword = "*}"

symbolDeclarationKeyword :: String
symbolDeclarationKeyword = "let"

adtKeyword :: String
adtKeyword = "type"

aliasKeyword :: String
aliasKeyword = "alias"

interfaceKeyword :: String
interfaceKeyword = "property"

instanceKeyword :: String
instanceKeyword = "instance"

matchKeyword :: String
matchKeyword = "match"

matchWithKeyword :: String
matchWithKeyword = "with"

{- LEGACY -}
specificsKeyword :: String
specificsKeyword = "where"

exprBindKeyword :: String
exprBindKeyword = "in"

lambdaKeyword :: String
lambdaKeyword = "lam"

signatureKeyword :: String
signatureKeyword = "val"

functionAppKeyword :: String
functionAppKeyword = "->"

constraintAppKeyword :: String
constraintAppKeyword = "=>"

definitionKeyword :: String
definitionKeyword = "="

defaultKeyword :: String
defaultKeyword = "_"

caseSeparationKeyword :: String
caseSeparationKeyword = "|"

andSeparationKeyword :: String
andSeparationKeyword = ","

caseThenKeyword :: String
caseThenKeyword = "->"

stateDefinitionKeyword :: String    --TODO: name not satisfying
stateDefinitionKeyword = ":"

stringLitKeyword :: String
stringLitKeyword = "\""

charLitKeyword :: String
charLitKeyword = "'"

recordStartKeyword :: String
recordStartKeyword = "{"

recordEndKeyword :: String
recordEndKeyword = "}"

funAsOpKeyword :: String
funAsOpKeyword = "`"

endStatementKeyword :: String
endStatementKeyword = ";;"

{- To use at the beginning of an identifier, to safely create names which are unparsable. -}
reservedIdKeyword :: String
reservedIdKeyword = "_,"

listTySugarStart :: String
listTySugarStart = "["

listTySugarEnd :: String
listTySugarEnd = "]"

listConSugarStart :: String
listConSugarStart = "["

listConSugarEnd :: String
listConSugarEnd = "]"

listConSugarSep :: String
listConSugarSep = ","

listEmptyCon :: String
listEmptyCon = "[]"

listConsCon :: String
listConsCon = "::"

tupleTySugarStart :: String
tupleTySugarStart = "("

tupleTySugarEnd :: String
tupleTySugarEnd = ")"

tupleTySugarSep :: String
tupleTySugarSep = ","

tupleConSugarStart :: String
tupleConSugarStart = "("

tupleConSugarEnd :: String
tupleConSugarEnd = ")"

tupleConSugarSep :: String
tupleConSugarSep = ","

trueCon :: String
trueCon = "True"

falseCon :: String
falseCon = "False"

compileTimeStartEval :: String
compileTimeStartEval = "{#"

compileTimeEndEval :: String
compileTimeEndEval = "#}"

compileTimeOpsCategory :: String
compileTimeOpsCategory = "OPERATORS_CATEGORY"

data OpsCategoryRecord =
    Record
        { fieldName :: String
        , fieldOps :: String
        , fieldGt :: String
        , fieldLt :: String
        , fieldFixity :: String
        , def :: String
        , listSep :: String
        , end :: String
        , infixLeftKey :: String
        , infixRightKey :: String
        , infixNoneKey :: String
        , prefixKey :: String
        , postfixKey :: String
        }

opsCategoryKeywords :: OpsCategoryRecord
opsCategoryKeywords =
    Record
        { fieldName = "name"
        , fieldOps = "operators"
        , fieldGt = "greater than"
        , fieldLt = "lesser than"
        , fieldFixity = "fixity"
        , def = ":"
        , listSep = ","
        , end = ";"
        , infixLeftKey = "InfixLeft"
        , infixRightKey = "InfixRight"
        , infixNoneKey = "InfixNone"
        , prefixKey = "Prefix"
        , postfixKey = "Postfix"
        }

possibleOpsHead :: String
possibleOpsHead = "|!$%&/=?^~+*@#<>.:-"

possibleOpsTail :: String
possibleOpsTail = "|!$%&/=?^~+*@#<>.:-"
