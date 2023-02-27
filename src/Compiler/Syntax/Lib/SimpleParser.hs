module Compiler.Syntax.Lib.SimpleParser
    ( parens
    , voidText
    , integerLiteral
    , naturalLiteral
    , doubleLiteral
    , charLiteral
    , stringLiteral
    , tuplesCon
    , tuplesConApplied
    , variableIdentifier
    , typeIdentifier
    , dataConIdentifier
    , paramTypeIdentifier
    , generalTypeIdentifier
    , opIdentifier
    , interfaceIdentifier
    , operator
    , singleOperator
    , keyword
    , caseSeparated
    , caseSeparated'
    , caseStarted
    , application
    , application'
    , binApplication
    , argApplication
    , orTokens
    , thenBetween
    , thenSeparated
    , andSeparated
    , letStatement
    , partialLetStatement
    , adtStatement
    , headAdtStatement
    , aliasStatement
    , matchStatement
    , multiMatchStatement
    , constraintStatement
    , signatureStatement
    , interfaceStatement
    , instanceStatement
    , bindingStatement
    , bindingsStatement
    , matchDefault
    , lambdaStatement
    , partialLambdaStatement
    , specialIdentifier
    , binary
    , prefix
    , postfix
    , opsCategoryStatement
    , categoryIdentifier
    , opsCategoryFixity
) where

import Data.Char
import Lib.Utils
import Text.Parsec
import qualified Text.Parsec.Token as Tokens
import Text.Parsec.Expr
import qualified Compiler.Syntax.Lib.Lex as Lex
import Compiler.Syntax.Lib.Info as Info
import qualified Compiler.Config.Lexer as Keys

{- Note: when "var" or "variable" occur as prefix of the name of a function, it means the function has to parse a
symbol identifier which is not an operator. -}

lexeme :: CustomParsec a -> CustomParsec a
lexeme = Tokens.lexeme Lex.lexer

symbol :: String -> CustomParsec String
symbol = Tokens.symbol Lex.lexer

{- This version parses the first argument as a reserved operator. If this is undesirable, look at separatedBy' -}
separatedBy :: String -> CustomParsec a -> CustomParsec [a]
separatedBy sep pars = (pars |> lexeme) `sepBy` Tokens.reservedOp Lex.lexer sep 

{- This version does not pars the first argument as reserved operator -}
separatedBy' :: String -> CustomParsec a -> CustomParsec [a]
separatedBy' sep pars = (pars |> lexeme) `sepBy` (symbol sep |> lexeme)

{- This version parses the first argument as a reserved operator. If this is undesirable, look at separatedBy1' -}
separatedBy1 :: String -> CustomParsec a -> CustomParsec [a]
separatedBy1 sep pars = (pars |> lexeme) `sepBy1` Tokens.reservedOp Lex.lexer sep 

{- This version does not pars the first argument as reserved operator -}
separatedBy1' :: String -> CustomParsec a -> CustomParsec [a]
separatedBy1' sep pars = (pars |> lexeme) `sepBy1` (symbol sep |> lexeme)

separatedBy2 :: String -> CustomParsec a -> CustomParsec [a]
separatedBy2 sep pars = do
    h <- pars |> lexeme
    Tokens.reservedOp Lex.lexer sep
    t <- separatedBy1 sep pars
    return $ h : t

separatedBy2' :: String -> CustomParsec a -> CustomParsec [a]
separatedBy2' sep pars = do
    h <- pars |> lexeme
    symbol sep |> lexeme
    t <- separatedBy1' sep pars
    return $ h : t

startedBy1 :: String -> CustomParsec a -> CustomParsec [a]
startedBy1 sep pars = (pars |> lexeme) `Lex.startBy1` Tokens.reservedOp Lex.lexer sep

{- Same as separatedBy/separatedBy' -}
enclosedBy :: String -> String -> CustomParsec a -> CustomParsec a
enclosedBy start end pars =
    between (Tokens.reservedOp Lex.lexer start) (Tokens.reservedOp Lex.lexer end) (pars |> lexeme)

{- Same as separatedBy/separatedBy' -}
enclosedBy' :: String -> String -> CustomParsec a -> CustomParsec a
enclosedBy' start end pars =
    between (symbol start |> lexeme) (symbol end |> lexeme) (pars |> lexeme)

notReservedId :: CustomParsec ()
notReservedId = notFollowedBy $ string Keys.reservedIdKeyword

identifier :: (String -> a) -> CustomParsec c -> CustomParsec a
identifier from firstLetter = do
    notReservedId
    lookAhead firstLetter
    id <- Tokens.identifier Lex.lexer
    return (from id)

__parens :: CustomParsec a -> CustomParsec a
__parens pars = do
    st <- getState
    st' <- Enclosing |> setObjectToParse st |> Lex.setState'
    Tokens.parens Lex.lexer pars <?> show st'

__variableIdentifier :: (String -> a) -> CustomParsec a
__variableIdentifier from = do
    st <- getState
    st' <- Symbol |> setObjectToParse st |> Lex.setState'
    fromId <- identifier from Lex.identifierStartLower
    return fromId <?> show st'

__typeIdentifier :: (String -> a) -> CustomParsec a
__typeIdentifier from = do
    st <- getState
    st' <- Type |> setObjectToParse st |> Lex.setState'
    fromId <- identifier from Lex.asciiUpper
    return fromId <?> show st'

__dataConIdentifier :: (String -> a) -> CustomParsec a
__dataConIdentifier from = do
    st <- getState
    st' <- ADTConstructor |> setObjectToParse st |> Lex.setState'
    fromId <- identifier from Lex.asciiUpper
    return fromId <?> show st'

__paramTypeIdentifier :: (String -> a) -> CustomParsec a
__paramTypeIdentifier from = do
    st <- getState
    st' <- Type |> setObjectToParse st |> Lex.setState'
    fromId <- identifier from Lex.asciiLower
    return fromId <?> show st'

__opIdentifier :: (String -> a) -> CustomParsec a
__opIdentifier from = do
    st <- getState
    st' <- Symbol |> setObjectToParse st |> Lex.setState'
    op <- __parens (Tokens.operator Lex.lexer)
    return (from op) <?> show st'

__intfIdentifier :: (String -> a) -> CustomParsec a
__intfIdentifier from = do
    st <- getState
    st' <- Interface |> setObjectToParse st |> Lex.setState'
    fromId <- identifier from Lex.asciiUpper
    return fromId <?> show st'

__bindingStatement :: CustomParsec a -> CustomParsec a
__bindingStatement p1 = do
    st <- getState
    st' <- BoundExpression |> setObjectToParse st |> Lex.setState'
    parsed <- p1
    Tokens.reserved Lex.lexer Keys.exprBindKeyword
    return parsed <?> show st'

__application :: CustomParsec a -> CustomParsec b -> Bool -> CustomParsec (a, [b])
__application applier arg atLeastOne = do
    st <- getState
    st' <- Application |> setObjectToParse st |> Lex.setState'
    f <- applier
    voidText id
    args <- if atLeastOne then arg `sepBy1` voidText id else arg `sepBy` voidText id 
    return (f, args) <?> show st'

primHeadAdtStatement :: CustomParsec a -> CustomParsec (a, Status)
primHeadAdtStatement p = do
    st <- getState
    st' <- ADT |> setObjectToParse st |> Lex.setState'
    Tokens.reserved Lex.lexer Keys.adtKeyword
    parsed <- p
    return (parsed, st')

opsCategoryField :: (Keys.OpsCategoryRecord -> String) -> CustomParsec a -> CustomParsec a
opsCategoryField fieldFrom value = let keys = Keys.opsCategoryKeywords in
    let str s = lexeme (string s) in (do
        st <- getState
        st' <- OperatorsCategory |> setObjectToParse st |> Lex.setState'
        {- Fetching keyword from an OpsCategoryRecord. -}
        str (fieldFrom keys)
        str (Keys.def keys)
        v <- value
        str (Keys.end keys)
        return v <?> show st')

opsCategoryRecord :: CustomParsec a
                  -> CustomParsec b
                  -> CustomParsec c
                  -> CustomParsec d
                  -> CustomParsec e
                  -> CustomParsec (a, [b], [c], [d], e)
opsCategoryRecord p1 p2 p3 p4 p5 = let keys = Keys.opsCategoryKeywords in do
    pars1 <- opsCategoryField Keys.fieldName p1
    pars2 <- opsCategoryField Keys.fieldOps $ separatedBy' (Keys.listSep keys) p2
    pars3 <- opsCategoryField Keys.fieldGt $ separatedBy' (Keys.listSep keys) p3
    pars4 <- opsCategoryField Keys.fieldLt $ separatedBy' (Keys.listSep keys) p4
    pars5 <- opsCategoryField Keys.fieldFixity p5
    return (pars1, pars2, pars3, pars4, pars5)

-- Exposition from here

parens :: CustomParsec a -> CustomParsec a
parens = __parens

voidText :: (() -> a) -> CustomParsec a
voidText from = do
    st <- getState
    st' <- Void |> setObjectToParse st |> Lex.setState'
    void <- Tokens.whiteSpace Lex.lexer
    return (from void) <?> show st'

integerLiteral :: (Integer -> a) -> CustomParsec a
integerLiteral from = do
    st <- getState
    st' <- IntegerLiteral |> setObjectToParse st |> Lex.setState'
    int <- Tokens.integer Lex.lexer
    return (from int) <?> show st'

naturalLiteral :: (Integer -> a) -> CustomParsec a
naturalLiteral from = do
    st <- getState
    st' <- NaturalLiteral |> setObjectToParse st |> Lex.setState'
    nat <- Tokens.natural Lex.lexer
    return (from nat) <?> show st'

doubleLiteral :: (Double -> a) -> CustomParsec a
doubleLiteral from = do
    st <- getState
    st' <- DoubleLiteral |> setObjectToParse st |> Lex.setState'
    double <- Tokens.float Lex.lexer
    return (from double) <?> show st'

charLiteral :: (Char -> a) -> CustomParsec a
charLiteral from = do
    st <- getState
    st' <- CharacterLiteral |> setObjectToParse st |> Lex.setState'
    char <- Tokens.charLiteral Lex.lexer
    return (from char) <?> show st'

stringLiteral :: (String -> a) -> CustomParsec a
stringLiteral from = do
    st <- getState
    st' <- StringLiteral |> setObjectToParse st |> Lex.setState'
    string <- Tokens.stringLiteral Lex.lexer
    return (from string) <?> show st'

{- It parses a tuple constructor, returning the length of the tuple constructor. -}
tuplesCon :: CustomParsec Int
tuplesCon = do
    Tokens.reservedOp Lex.lexer Keys.tupleConSugarStart
    l <- separatedBy2 Keys.tupleConSugarSep $ voidText id
    Tokens.reservedOp Lex.lexer Keys.tupleConSugarEnd
    return $ length l

{- It returns the tuple constructor applied to something else, returning the applied values and the length of
the tuple constructor. -}
tuplesConApplied :: CustomParsec a -> CustomParsec (Int, [a])
tuplesConApplied p = do
    st <- getState
    st' <- TupleCon |> setObjectToParse st |> Lex.setState'
    valuesWithin <?> show st'
    where
        {- In pseudo-code:
            (w, x, y, z)
        -}
        valuesWithin = do
            Tokens.reservedOp Lex.lexer Keys.tupleConSugarStart
            res <- separatedBy2 Keys.tupleConSugarSep p
            Tokens.reservedOp Lex.lexer Keys.tupleConSugarEnd
            return (length res, res)

variableIdentifier :: (String -> a) -> CustomParsec a
variableIdentifier = __variableIdentifier

typeIdentifier :: (String -> a) -> CustomParsec a
typeIdentifier = __typeIdentifier

dataConIdentifier :: (String -> a) -> CustomParsec a
dataConIdentifier = __dataConIdentifier

paramTypeIdentifier :: (String -> a) -> CustomParsec a
paramTypeIdentifier = __paramTypeIdentifier

generalTypeIdentifier :: (String -> a) -> CustomParsec a
generalTypeIdentifier from = try (typeIdentifier from) <|> paramTypeIdentifier from

{- It parses an operator only between parenthesis. If you need a parser for operators not enclosed
in parenthesis, see `operator`. -}
opIdentifier :: (String -> a) -> CustomParsec a
opIdentifier = __opIdentifier

interfaceIdentifier :: (String -> a) -> CustomParsec a
interfaceIdentifier = __intfIdentifier

{- It parses both "normal" operators and variables used as operators (a keyword should be attached
to the variable name in order to use it as an operator, see the list of keywords for it). -}
operator :: (String -> a) -> CustomParsec a
operator from = do
    st <- getState
    st' <- Operator |> setObjectToParse st |> Lex.setState'
    op <- try (Tokens.operator Lex.lexer) <|> (do
        {- Using the special keyword that lets variables being used as operators. -}
        Tokens.reservedOp Lex.lexer Keys.funAsOpKeyword
        variableIdentifier id)
    return (from op) <?> show st'

singleOperator :: (String -> a) -> String -> CustomParsec a
singleOperator from this = do
    st <- getState
    st' <- Operator |> setObjectToParse st |> Lex.setState'
    op <- try (do { op' <- Tokens.operator Lex.lexer
                  ; if op' /= this then parserZero else return op' })
          <|> (do { Tokens.reservedOp Lex.lexer Keys.funAsOpKeyword
                  ; v <- variableIdentifier id
                  ; if v /= this then parserZero else return v })
    return (from op) <?> show st'

keyword :: String -> (() -> a) -> CustomParsec a
keyword k from = do
    st <- getState
    st' <- Keyword k |> setObjectToParse st |> Lex.setState'
    nothing <- try (Tokens.reserved Lex.lexer k) <|> Tokens.reservedOp Lex.lexer k --try parser is used not to consume the input
    return (from nothing) <?> show st'

caseSeparated :: CustomParsec a -> CustomParsec [a]
caseSeparated pars = do
    st <- getState
    st' <- CaseSequence |> setObjectToParse st |> Lex.setState'
    sep <- separatedBy Keys.caseSeparationKeyword pars
    return sep <?> show st'

{- Same of `caseSeparated`, but the parser is successful by running at least once. -}
caseSeparated' :: CustomParsec a -> CustomParsec [a]
caseSeparated' pars = do
    st <- getState
    st' <- CaseSequence |> setObjectToParse st |> Lex.setState'
    sep <- separatedBy1 Keys.caseSeparationKeyword pars
    return sep <?> show st'

caseStarted :: CustomParsec a -> CustomParsec [a]
caseStarted pars = do
    st <- getState
    st' <- CaseSequence |> setObjectToParse st |> Lex.setState'
    sep <- startedBy1 Keys.caseSeparationKeyword pars
    return sep <?> show st'

{- This version makes the second parser to be successful even by running zero times,
if this is undesirable look at application'. -}
application :: CustomParsec a -> CustomParsec b -> CustomParsec (a, [b])
application applier arg = __application applier arg False

{- This version makes the second parser to be successful only if it runs at least once -}
application' :: CustomParsec a -> CustomParsec b -> CustomParsec (a, [b])
application' applier arg = __application applier arg True

binApplication :: CustomParsec a -> CustomParsec b -> CustomParsec (a, b)
binApplication applier arg = do
    st <- getState
    st' <- Application |> setObjectToParse st |> Lex.setState'
    f <- applier
    voidText id
    x <- arg
    return (f, x)

-- To parse only what is applied in an application (not the applier)
argApplication :: CustomParsec a -> CustomParsec [a]
argApplication arg = do
    st <- getState
    st' <- Application |> setObjectToParse st |> Lex.setState'    --TODO: is it an application anyway???
    args <- endBy1 arg $ voidText id
    return args <?> show st'

orTokens :: CustomParsec a -> CustomParsec b -> CustomParsec (Either a b)
orTokens p1 p2 = do
    try leftP <|> rightP
    where
        leftP = Left <$> p1

        rightP = Right <$> p2

thenBetween :: CustomParsec a -> CustomParsec b -> CustomParsec (a, b)
thenBetween p1 p2 = do
    st <- getState
    st' <- ThenSequence |> setObjectToParse st |> Lex.setState'
    left <- p1
    Tokens.reservedOp Lex.lexer Keys.caseThenKeyword
    right <- p2
    return (left, right) <?> show st'

thenSeparated :: CustomParsec a -> CustomParsec [a]
thenSeparated pars = do
    st <- getState
    st' <- ThenSequence |> setObjectToParse st |> Lex.setState'
    separatedBy1 Keys.functionAppKeyword pars <?> show st'

andSeparated :: CustomParsec a -> CustomParsec [a]
andSeparated p = do
    st <- getState
    st' <- And |> setObjectToParse st |> Lex.setState'
    l <- separatedBy1 Keys.andSeparationKeyword p
    return l <?> show st'

letStatement :: CustomParsec a -> CustomParsec b -> CustomParsec (a, b)
letStatement p1 p2 = do
    st <- getState
    st' <- SymbolDeclaration |> setObjectToParse st |> Lex.setState'
    Tokens.reserved Lex.lexer Keys.symbolDeclarationKeyword
    left <- p1
    Tokens.reservedOp Lex.lexer Keys.definitionKeyword
    right <- p2
    return (left, right) <?> show st'

partialLetStatement :: CustomParsec a -> CustomParsec a
partialLetStatement p1 = do
    st <- getState
    st' <- SymbolDeclaration |> setObjectToParse st |> Lex.setState'
    Tokens.reserved Lex.lexer Keys.symbolDeclarationKeyword
    p1 <?> show st'

headAdtStatement :: CustomParsec a -> CustomParsec a
headAdtStatement p = do
    (parsed, st) <- primHeadAdtStatement p
    return parsed <?> show st

adtStatement :: CustomParsec a -> CustomParsec b -> CustomParsec b -> CustomParsec (a, b)
adtStatement p1 p2 otherP2 = do
    (left, st) <- primHeadAdtStatement p1
    right <- try tailStatement <|> otherP2
    return (left, right) <?> show st
    where
        tailStatement = do
            Tokens.reservedOp Lex.lexer Keys.definitionKeyword
            p2

aliasStatement :: CustomParsec a -> CustomParsec b -> CustomParsec (a, b)
aliasStatement p1 p2 = do
    st <- getState
    st' <- Alias |> setObjectToParse st |> Lex.setState'
    Tokens.reserved Lex.lexer Keys.aliasKeyword
    left <- p1
    Tokens.reservedOp Lex.lexer Keys.definitionKeyword
    right <- p2
    return (left, right) <?> show st'

matchStatement :: CustomParsec a -> CustomParsec b -> CustomParsec (a, b)
matchStatement p1 p2 = do
    st <- getState
    st' <- MatchExpression |> setObjectToParse st |> Lex.setState'
    Tokens.reserved Lex.lexer Keys.matchKeyword
    left <- p1
    Tokens.reserved Lex.lexer Keys.matchWithKeyword
    right <- p2
    return (left, right) <?> show st'

multiMatchStatement :: CustomParsec a -> CustomParsec b -> CustomParsec [(a, b)]
multiMatchStatement p1 p2 = do
    st <- getState
    st' <- MatchExpression |> setObjectToParse st |> Lex.setState'
    caseStarted comb <?> show st'
    where
        comb = do
            left <- p1
            Tokens.reservedOp Lex.lexer Keys.caseThenKeyword
            right <- p2
            return (left, right)

constraintStatement :: CustomParsec a -> CustomParsec a
constraintStatement p = do
    st <- getState
    st' <- ConstraintStatement |> setObjectToParse st |> Lex.setState'
    res <- p
    Tokens.reservedOp Lex.lexer Keys.constraintAppKeyword
    return res <?> show st'

signatureStatement :: CustomParsec a -> CustomParsec b -> CustomParsec (a, b)
signatureStatement p1 p2 = do
    st <- getState
    st' <- SignatureStatement |> setObjectToParse st |> Lex.setState'
    Tokens.reserved Lex.lexer Keys.signatureKeyword
    left <- p1
    Tokens.reservedOp Lex.lexer Keys.stateDefinitionKeyword
    right <- p2
    return (left, right) <?> show st'

interfaceStatement :: CustomParsec a -> CustomParsec b -> CustomParsec (a, b)
interfaceStatement p1 p2 = do
    st <- getState
    st' <- Interface |> setObjectToParse st |> Lex.setState'
    Tokens.reserved Lex.lexer Keys.interfaceKeyword
    left <- p1
    Tokens.reservedOp Lex.lexer Keys.definitionKeyword
    right <- p2
    Tokens.reservedOp Lex.lexer Keys.endStatementKeyword
    return (left, right) <?> show st'

instanceStatement :: CustomParsec a -> CustomParsec b -> CustomParsec (a, b)
instanceStatement p1 p2 = do
    st <- getState
    st' <- Instance |> setObjectToParse st |> Lex.setState'
    Tokens.reserved Lex.lexer Keys.instanceKeyword
    left <- p1
    Tokens.reservedOp Lex.lexer Keys.definitionKeyword
    right <- p2
    Tokens.reservedOp Lex.lexer Keys.endStatementKeyword
    return (left, right) <?> show st'

bindingStatement :: CustomParsec a -> CustomParsec b -> CustomParsec (a, b)
bindingStatement p1 p2 = do
    left <- __bindingStatement p1
    right <- p2
    return (left, right)

bindingsStatement :: CustomParsec a -> CustomParsec b -> CustomParsec ([a], b)
bindingsStatement p1 p2 = do
    left <- __bindingStatement p1 |> many1
    right <- p2
    return (left, right)

matchDefault :: (() -> a) -> CustomParsec a
matchDefault from = do
    nothing <- Tokens.reserved Lex.lexer Keys.defaultKeyword
    return (from nothing)

lambdaStatement :: CustomParsec a -> CustomParsec b -> CustomParsec (a, b)
lambdaStatement p1 p2 = do
    st <- getState
    st' <- LambdaExpression |> setObjectToParse st |> Lex.setState'
    Tokens.reserved Lex.lexer Keys.lambdaKeyword
    left <- p1
    Tokens.reservedOp Lex.lexer Keys.functionAppKeyword
    right <- p2
    return (left, right) <?> show st'

partialLambdaStatement :: CustomParsec a -> CustomParsec a
partialLambdaStatement p1 = do
    st <- getState
    st' <- LambdaExpression |> setObjectToParse st |> Lex.setState'
    Tokens.reserved Lex.lexer Keys.lambdaKeyword
    p1 <?> show st'

{- Special parser to parse identifiers which go out from language parsing rules. Anyway, it cannot parse
an identifier which begins with the reserved identifier keyword. -}
specialIdentifier :: (String -> a) -> CustomParsec a
specialIdentifier from = do
    notReservedId
    id <- many1 . satisfy $ \c -> isSymbol c || isAlphaNum c || isPunctuation c
    return $ from id

binary :: CustomParsec b -> (b -> a -> a -> a) -> Assoc -> CustomOperator a
binary pars f assoc =
    Infix <| try (do
        op <- pars
        return $ f op) <| assoc

prefix :: CustomParsec b -> (b -> a -> a) -> CustomOperator a
prefix pars f =
    Prefix <| try (do
        op <- pars
        return $ f op)

postfix :: CustomParsec b -> (b -> a -> a) -> CustomOperator a
postfix pars f =
    Postfix <| try (do
    op <- pars
    return $ f op)

opsCategoryStatement :: CustomParsec a
                     -> CustomParsec b
                     -> CustomParsec c
                     -> CustomParsec d
                     -> CustomParsec e
                     -> CustomParsec (a, [b], [c], [d], e)
opsCategoryStatement p1 p2 p3 p4 p5 = let recordPars = opsCategoryRecord p1 p2 p3 p4 p5 in do
    st <- getState
    st' <- OperatorsCategory |> setObjectToParse st |> Lex.setState'
    string Keys.compileTimeOpsCategory |> lexeme
    (pars1, pars2, pars3, pars4, pars5) <- enclosedBy Keys.compileTimeStartEval Keys.compileTimeEndEval recordPars
    return (pars1, pars2, pars3, pars4, pars5) <?> show st'

categoryIdentifier :: CustomParsec String
categoryIdentifier = do
    st <- getState
    st' <- OperatorsCategoryName |> setObjectToParse st |> Lex.setState'
    catId <- identifier id Lex.asciiUpper
    return catId <?> show st'

opsCategoryFixity :: CustomParsec String
opsCategoryFixity = let keys = Keys.opsCategoryKeywords in
    let str s = string s |> lexeme in choice
        [ str (Keys.infixLeftKey keys) |> try
        , str (Keys.infixRightKey keys) |> try
        , str (Keys.infixNoneKey keys) |> try
        , str (Keys.prefixKey keys) |> try
        , str (Keys.postfixKey keys)
        ]
