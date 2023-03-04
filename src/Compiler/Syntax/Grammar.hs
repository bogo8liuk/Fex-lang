{-# LANGUAGE TupleSections #-}

module Compiler.Syntax.Grammar
    ( declaration
    , opsCategories
    , interface
    , specialInterface
    , specialAdt
    , specialAdt'
) where

import Lib.Utils
import Data.Array(bounds, (!))
import qualified Compiler.Config.Types as BITy
import Text.Parsec
import Text.Parsec.Expr
import Compiler.Syntax.Lib.Lex as Lex
import Compiler.Syntax.Lib.Info as Info
import qualified Compiler.Syntax.Lib.SimpleParser as Pars
import qualified Compiler.Ast.Tree as Tree
import Compiler.State as With

parConstraintToken :: CustomParsec (Tree.IntfName With.ProgState) -> CustomParsec (Tree.Constraint With.ProgState)
parConstraintToken intfId = do
    tst <- fetchTokenState
    (id, ts) <- Pars.application' intfId argUnCon
    return $ Tree.Cont (id, ts, tst)

{- To parse constraints, use `constraints` instead of this, because it parses the right construct. -}
parManyConstraints :: CustomParsec (Tree.IntfName With.ProgState) -> CustomParsec [Tree.Constraint With.ProgState]
parManyConstraints intfId = Pars.andSeparated $ parConstraintToken intfId

parConstraints :: CustomParsec (Tree.IntfName With.ProgState) -> CustomParsec [Tree.Constraint With.ProgState]
{- When there are no constraints to parse, also keywords associated to constraints do not exist and this is the way
to parse a situation like that. -}
parConstraints intfId = option [] . try . Pars.constraintStatement $ parManyConstraints intfId

{- It does not really parse an IntfDeclare statement, it misses constraints. -}
parInterfaceDeclare
    :: CustomParsec (Tree.IntfName With.ProgState)
    -> CustomParsec (Tree.IntfName With.ProgState, [Tree.ParamTypeName With.ProgState], ProgState)
parInterfaceDeclare intfId = do
    tst <- fetchTokenState
    (a, as) <- Pars.application' intfId paramTypeIdentifier
    return (a, as, tst)

parAdtDeclare :: CustomParsec (Tree.ADTName With.ProgState) -> CustomParsec (Tree.ADTDeclare With.ProgState)
parAdtDeclare adtId = do
    tst <- fetchTokenState
    (a, as) <- Pars.application adtId paramTypeIdentifier
    return $ Tree.ADTDecl (a, as, tst)

parInterfaceBody
    :: CustomParsec [Tree.Constraint With.ProgState]
    -> CustomParsec ([Tree.Constraint With.ProgState], [Tree.Signature With.ProgState])
parInterfaceBody conts = do
    cs <- conts
    sigs <- many signature
    return (cs, sigs)

parInterface
    :: CustomParsec (Tree.IntfName With.ProgState, [Tree.ParamTypeName With.ProgState], ProgState)
    -> CustomParsec [Tree.Constraint With.ProgState]
    -> CustomParsec (Tree.Interface With.ProgState)
parInterface intfDecl conts = do
    tst <- fetchTokenState
    ((n, args, hst), (cs, sigs)) <- Pars.interfaceStatement intfDecl $ parInterfaceBody conts
    return $ Tree.IntfTok (Tree.IntfDecl (n, cs, args, hst), sigs, tst)

parAdt
    :: CustomParsec (Tree.ADTDeclare With.ProgState)
    -> CustomParsec [Tree.ADTConstructor With.ProgState]
    -> CustomParsec (Tree.AlgebraicDataType With.ProgState)
parAdt adtDecl cons = do
    tst <- fetchTokenState
    (b, t) <- Pars.adtStatement adtDecl cons $ pure []
    return $ Tree.ADTTok (b, t, tst)

symbolDeclare :: CustomParsec (Tree.SymbolDeclare With.ProgState)
symbolDeclare = do
    tst <- fetchTokenState
    (n, syms) <- Pars.application symbolIdentifier symbolIdentifier
    {- TODO: setting an empty hint (non-existing), because it is not supported right now. -}
    return $ Tree.SymDecl (n, Tree.Hint (Nothing, tst), syms, tst)

multiCaseMatchingExpressions :: CustomParsec (With.ProgState, [Tree.MatchingExpression With.ProgState])
multiCaseMatchingExpressions = do
    tst <- fetchTokenState
    ms <- many1 matchingExpression
    return (tst, ms)

multiPatternMatch
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Tree.MultiPatternMatch With.ProgState)
multiPatternMatch table = do
    tst <- fetchTokenState
    mssExprs <- Pars.multiMatchStatement multiCaseMatchingExpressions $ expression table
    let cs = map (\((cst, ms), e) -> Tree.MultiCase ms e cst) mssExprs
    return $ Tree.MultiPattMatch cs tst

symbolDeclaration
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Tree.SymbolDeclaration With.ProgState)
symbolDeclaration table = do
    tst <- fetchTokenState
    (d, e) <- Pars.letStatement symbolDeclare $ expression table
    return $ Tree.SymTok (d, e, tst)

multiSymbolDeclaration
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Tree.MultiSymbolDeclaration With.ProgState)
multiSymbolDeclaration table = do
    tst <- fetchTokenState
    sn <- Pars.partialLetStatement symbolIdentifier
    mpm <- multiPatternMatch table
    return $ Tree.MultiSymTok sn (Tree.Hint (Nothing, tst)) {- TODO: hint -} mpm tst

genericSymbolDeclaration
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Tree.SDUnion With.ProgState)
genericSymbolDeclaration table = try multiSymD <|> symD
    where
        symD = do
            sd <- symbolDeclaration table
            return $ Tree.SD sd

        multiSymD = do
            msd <- multiSymbolDeclaration table
            return $ Tree.MSD msd

exprEnclosed :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Expression With.ProgState)
exprEnclosed table = do
    enc <- Pars.parens $ expression table
    return enc

mExprEnclosed :: CustomParsec (Tree.MatchingExpression With.ProgState)
mExprEnclosed = do
    enc <- Pars.parens matchingExpression
    return enc

tupleApplication :: CustomParsec a -> CustomParsec (Tree.ADTConName With.ProgState, [a])
tupleApplication p = do
    tst <- fetchTokenState
    (n, as) <- Pars.tuplesConApplied p
    con <- getTupleConIdentifier n tst
    return (con, as)

adtAppMatchExpression :: CustomParsec (Tree.ADTAppMatchExpression With.ProgState)
adtAppMatchExpression = do
    tst <- fetchTokenState
    (a, as) <-
        try (Pars.application' dataConIdentifier appliedMatchExpression) <|>
        tupleApplication tupleAppliedMatchExpression
    return $ Tree.ADTAppMExpr (a, as, tst)

unAltMExprAdtApplication :: CustomParsec (Tree.UnAltMatchingExpression With.ProgState)
unAltMExprAdtApplication = do
    a <- adtAppMatchExpression
    return $ Tree.MADTApp a

mExprAdtApplication :: CustomParsec (Tree.MatchingExpression With.ProgState)
mExprAdtApplication = do
    tst <- fetchTokenState
    m <- unAltMExprAdtApplication
    return $ Tree.MatchExpr m tst

symbolIdentifier :: CustomParsec (Tree.SymbolName With.ProgState)
symbolIdentifier = do
    tst <- fetchTokenState
    parsed <- try (Pars.variableIdentifier id) <|> Pars.opIdentifier id
    return $ Tree.SymName (parsed, tst)

{- NB: it parses only user-defined data constructors. It does NOT parse special data constructors. -}
dataConIdentifier :: CustomParsec (Tree.ADTConName With.ProgState)
dataConIdentifier = do
    tst <- fetchTokenState
    parsed <- Pars.dataConIdentifier id
    return $ Tree.ADTConName (parsed, tst)

getTupleConIdentifier :: Int -> With.ProgState -> CustomParsec (Tree.ADTConName With.ProgState)
getTupleConIdentifier n tst = do
    let (lb, ub) = bounds BITy.conTuples
    if n < lb || n > ub
    then parserZero <?> "Illegal size (" ++ show n ++ ") for tuple constructor, the maximum is " ++ show ub
    else do
        return $ Tree.ADTConName (BITy.conTuples ! n, tst)

{- It parses every kind of data constructor, even built-in ones. -}
richDataConIdentifier :: CustomParsec (Tree.ADTConName With.ProgState)
richDataConIdentifier = do
    tst <- fetchTokenState
    try dataConIdentifier <|> tupleIdentifier tst
    where
        tupleIdentifier tst = do
            n <- Pars.tuplesCon
            getTupleConIdentifier n tst

typeIdentifier :: CustomParsec (Tree.ADTName With.ProgState)
typeIdentifier = do
    tst <- fetchTokenState
    parsed <- Pars.typeIdentifier id
    return $ Tree.ADTName (parsed, tst)

paramTypeIdentifier :: CustomParsec (Tree.ParamTypeName With.ProgState)
paramTypeIdentifier = do
    tst <- fetchTokenState
    parsed <- Pars.paramTypeIdentifier id
    return $ Tree.PtyName (parsed, tst)

interfaceIdentifier :: CustomParsec (Tree.IntfName With.ProgState)
interfaceIdentifier = do
    tst <- fetchTokenState
    i <- Pars.interfaceIdentifier id
    return $ Tree.IntfName (i, tst)

matchCase :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.MatchCase With.ProgState)
matchCase table = do
    tst <- fetchTokenState
    st <- getState
    st' <- MatchCase |> setObjectToParse st |> Lex.setState'
    (m, e) <- Pars.thenBetween matchingExpression $ expression table
    return (Tree.Case (m, e, tst)) <?> show st'

patternMatch :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.PatternMatch With.ProgState)
patternMatch table = do
    tst <- fetchTokenState
    (e, mc) <- Pars.matchStatement <| expression table <| Pars.caseSeparated (matchCase table)
    return $ Tree.PattMatch (e, mc, tst)

literal :: CustomParsec (Tree.Literal With.ProgState)
literal = do
    tst <- fetchTokenState
    st <- getState
    st' <- Literal |> setObjectToParse st |> Lex.setState'
    lit <- choice
        [ try . Pars.doubleLiteral $ \f -> Tree.DoubleLit (f, tst)
        , try . Pars.naturalLiteral $ \n -> Tree.IntLit (n, tst)
        , try . Pars.charLiteral $ \c -> Tree.CharLit (c, tst)
        , Pars.stringLiteral $ \s -> Tree.StringLit (s, tst)
        ]
    return lit <?> show st'

unAltExprLiteral :: CustomParsec (Tree.UnAltExpression With.ProgState)
unAltExprLiteral = do
    l <- literal
    return $ Tree.Lit l

exprLiteral :: CustomParsec (Tree.Expression With.ProgState)
exprLiteral = do
    tst <- fetchTokenState
    e <- unAltExprLiteral
    return $ Tree.Expr (e, Tree.Hint (Nothing, tst {- TODO: fetch real state of hint -}), tst)

unAltMExprLiteral :: CustomParsec (Tree.UnAltMatchingExpression With.ProgState)
unAltMExprLiteral = do
    l <- literal
    return $ Tree.MLit l

mExprLiteral :: CustomParsec (Tree.MatchingExpression With.ProgState)
mExprLiteral = do
    tst <- fetchTokenState
    m <- unAltMExprLiteral
    return $ Tree.MatchExpr m tst

unAltExprBase :: CustomParsec (Tree.UnAltExpression With.ProgState)
unAltExprBase = do
    s <- symbolIdentifier
    return $ Tree.Base s

exprBase :: CustomParsec (Tree.Expression With.ProgState)
exprBase = do
    tst <- fetchTokenState
    e <- unAltExprBase
    return $ Tree.Expr (e, Tree.Hint (Nothing, tst {- TODO: fetch real state of hint -}), tst)

unAltMExprBase :: CustomParsec (Tree.UnAltMatchingExpression With.ProgState)
unAltMExprBase = do
    s <- symbolIdentifier
    return $ Tree.MBase s

mExprBase :: CustomParsec (Tree.MatchingExpression With.ProgState)
mExprBase = do
    tst <- fetchTokenState
    m <- unAltMExprBase
    return $ Tree.MatchExpr m tst

exprAdtBase :: CustomParsec (Tree.Expression With.ProgState)
exprAdtBase = do
    tst <- fetchTokenState
    e <- unAltExprAdtBase
    return $ Tree.Expr (e, Tree.Hint (Nothing, tst {- TODO: fetch real state of hint -}), tst)

unAltExprAdtBase :: CustomParsec (Tree.UnAltExpression With.ProgState)
unAltExprAdtBase = do
    c <- richDataConIdentifier
    return $ Tree.ADTBase c

unAltMExprAdtBase :: CustomParsec (Tree.UnAltMatchingExpression With.ProgState)
unAltMExprAdtBase = do
    c <- richDataConIdentifier
    return $ Tree.MADTBase c

mExprAdtBase :: CustomParsec (Tree.MatchingExpression With.ProgState)
mExprAdtBase = do
    tst <- fetchTokenState
    m <- unAltMExprAdtBase
    return $ Tree.MatchExpr m tst

unAltMExprDefault :: CustomParsec (Tree.UnAltMatchingExpression With.ProgState)
unAltMExprDefault = do
    tst <- fetchTokenState
    Pars.matchDefault $ \_ -> Tree.MDefault tst

mExprDefault :: CustomParsec (Tree.MatchingExpression With.ProgState)
mExprDefault = do
    tst <- fetchTokenState
    m <- unAltMExprDefault
    return $ Tree.MatchExpr m tst

unAltExprPatternMatch :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.UnAltExpression With.ProgState)
unAltExprPatternMatch table = do
    p <- patternMatch table
    return $ Tree.Match p

exprPatternMatch :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Expression With.ProgState)
exprPatternMatch table = do
    tst <- fetchTokenState
    e <- unAltExprPatternMatch table
    return $ Tree.Expr (e, Tree.Hint (Nothing, tst {- TODO: fetch real state of hint -}), tst)

tupleAppliedExpression
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Tree.Expression With.ProgState)
tupleAppliedExpression = expression

exprTupleApplication
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Tree.Expression With.ProgState, [Tree.Expression With.ProgState])
exprTupleApplication table = do
    tst <- fetchTokenState
    (con, as) <- tupleApplication $ tupleAppliedExpression table
    return (Tree.Expr (Tree.ADTBase con, Tree.Hint (Nothing, tst{- TODO: fetch real state of hint -}), tst), as)

appliedExpression :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Expression With.ProgState)
appliedExpression table =
    choice
        [ try exprLiteral
        , try exprBase
        , try exprAdtBase
        , try appTupleExpression
        , exprEnclosed table
        ]
    where
        appTupleExpression = do
            tst <- fetchTokenState
            (a, as) <- exprTupleApplication table
            let e = Tree.App $ Tree.AppExpr (a, as, tst)
            return $ Tree.Expr (e, Tree.Hint (Nothing, tst {- TODO: fetch real state of hint -}), tst)

appExpression
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Tree.AppExpression With.ProgState) 
appExpression table = do
    tst <- fetchTokenState
    (a, as) <-
        try (exprTupleApplication table) <|>
        (Pars.application' <| noAppExpression table <| appliedExpression table)
    return $ Tree.AppExpr (a, as, tst)

unAltExprApplication
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Tree.UnAltExpression With.ProgState)
unAltExprApplication table = do
    a <- appExpression table
    return $ Tree.App a

exprApplication
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Tree.Expression With.ProgState)
exprApplication table = do
    tst <- fetchTokenState
    e <- unAltExprApplication table
    return $ Tree.Expr (e, Tree.Hint (Nothing, tst {- TODO: fetch real state of hint -}), tst)

lambda :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Lambda With.ProgState)
lambda table = do
    tst <- fetchTokenState
    (syms, e) <- Pars.lambdaStatement <| Pars.argApplication symbolIdentifier <| expression table
    return $ Tree.Lambda (syms, e, tst)

multiLambda :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.MultiLambda With.ProgState)
multiLambda table = do
    tst <- fetchTokenState
    mpm <- Pars.partialLambdaStatement $ multiPatternMatch table
    return $ Tree.MultiLambda mpm tst

genLambda
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Either (Tree.Lambda With.ProgState) (Tree.MultiLambda With.ProgState))
genLambda table = Pars.orTokens (lambda table) (multiLambda table)

unAltExprLambda :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.UnAltExpression With.ProgState)
unAltExprLambda table = do
    f <- genLambda table
    case f of
        Left l -> return $ Tree.Lam l
        Right ml -> return $ Tree.MultiLam ml

exprLambda :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Expression With.ProgState)
exprLambda table = do
    tst <- fetchTokenState
    e <- unAltExprLambda table
    return $ Tree.Expr (e, Tree.Hint (Nothing, tst {- TODO: fetch real state of hint -}), tst)

{-exprLambdaApplication :: CustomOpTable Tree.Expression -> CustomParsec Tree.Expression
exprLambdaApplication table = do
    f <- Pars.parens (lambda table)
    args <- Pars.argApplication (appliedExpression table)
    return (Tree.LamApp (f, args))
-}

boundExpression
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Tree.BoundExpression With.ProgState)
boundExpression table = do
    tst <- fetchTokenState
    (s, e) <- Pars.bindingStatement <| symbolDeclaration table <| expression table
    return $ Tree.BoundExpr (s, e, tst)

multiBoundExpression
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Tree.MultiBoundExpression With.ProgState)
multiBoundExpression table = do
    tst <- fetchTokenState
    (s, e) <- Pars.bindingStatement <| multiSymbolDeclaration table <| expression table
    return $ Tree.MultiBoundExpr s e tst

genBoundExpression
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Either (Tree.BoundExpression With.ProgState) (Tree.MultiBoundExpression With.ProgState))
genBoundExpression table =
    Pars.orTokens <| boundExpression table <| multiBoundExpression table

unAltGenExprBound :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.UnAltExpression With.ProgState)
unAltGenExprBound table = do
    genBound <- genBoundExpression table
    case genBound of
        Left b -> return $ Tree.Bound b
        Right mb -> return $ Tree.MultiBound mb

genExprBound :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Expression With.ProgState)
genExprBound table = do
    tst <- fetchTokenState
    e <- unAltGenExprBound table
    return $ Tree.Expr (e, Tree.Hint (Nothing, tst {- TODO: fetch real state of hint -}), tst)

noAppExpression :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Expression With.ProgState)
noAppExpression table = choice
    [ try exprBase
    , try exprAdtBase
    , try exprLiteral
    , exprEnclosed table
    ]

__expression :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Expression With.ProgState)
__expression table = choice
    [ try $ exprApplication table
    , try exprBase
    , try exprAdtBase
    , try $ exprPatternMatch table
    , try $ exprLambda table
    , try $ genExprBound table
    , try exprLiteral
    , exprEnclosed table
    ]

expression :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Expression With.ProgState)
expression table = do
    st <- getState
    st' <- Expression |> setObjectToParse st |> Lex.setState'
    buildExpressionParser table (__expression table) <?> show st'

appliedMatchExpression :: CustomParsec (Tree.MatchingExpression With.ProgState)
appliedMatchExpression = choice
    [ try mExprLiteral
    , try mExprAdtBase
    , try mExprBase
    , try mExprDefault
    , try appTupleExpression
    , mExprEnclosed
    ]
    where
        appTupleExpression = do
            tst <- fetchTokenState
            (con, as) <- tupleApplication tupleAppliedMatchExpression
            let me = Tree.MADTApp (Tree.ADTAppMExpr (con, as, tst))
            return $ Tree.MatchExpr me tst

tupleAppliedMatchExpression :: CustomParsec (Tree.MatchingExpression With.ProgState)
tupleAppliedMatchExpression = matchingExpression

matchingExpression :: CustomParsec (Tree.MatchingExpression With.ProgState)
matchingExpression = choice
    [ try mExprAdtApplication
    , try mExprAdtBase
    , try mExprLiteral
    , try mExprBase
    , try mExprDefault
    , mExprEnclosed
    ]

paramTypeInNonRec :: CustomParsec (Tree.NonRecType With.ProgState)
paramTypeInNonRec = do
    id <- paramTypeIdentifier
    return $ Tree.Param id

realTypeInNonRec :: CustomParsec (Tree.NonRecType With.ProgState)
realTypeInNonRec = do
    id <- typeIdentifier
    return $ Tree.Real id

nonRecTypeToken :: CustomParsec (Tree.NonRecType With.ProgState)
nonRecTypeToken = try realTypeInNonRec <|> paramTypeInNonRec

{- Use this to parse unconstrained types which must be arguments of another token. -}
argUnCon :: CustomParsec (Tree.UnConType With.ProgState)
argUnCon = try singletonInUnCon <|> Pars.parens compositeInUnCon

typeComposite :: CustomParsec (Tree.TypeComposite With.ProgState)
typeComposite = do
    tst <- fetchTokenState
    --The applied must be at least one, so using `application'`
    (a, as) <- Pars.application' nonRecTypeToken argUnCon
    return $ Tree.TyComp (a, as, tst)

{- compositeInUnCon and singletonInUnCon are just wrapper for UnConType type. The entire
UnConType parser is functionUnConTypeToken. -}
compositeInUnCon :: CustomParsec (Tree.UnConType With.ProgState)
compositeInUnCon = do
    con <- typeComposite
    return $ Tree.Composite con

singletonInUnCon :: CustomParsec (Tree.UnConType With.ProgState)
singletonInUnCon = do
    sin <- nonRecTypeToken
    return $ Tree.Singleton sin

{- NB: if you have to parse unconstrained type tokens as arguments of another token, do NOT use
this, look for `argUnCon`.
NB: this is not the top-level parser of unconstrained types, look at functionUnConTypeToken instead.
-}
unConTypeToken :: CustomParsec (Tree.UnConType With.ProgState)
unConTypeToken = try compositeInUnCon <|> singletonInUnCon

constraints :: CustomParsec [Tree.Constraint With.ProgState]
constraints = parConstraints interfaceIdentifier

functionUnConTypeToken :: CustomParsec (Tree.UnConType With.ProgState)
functionUnConTypeToken = do
    {- Trying firstly the non-function type without parenthesys, then trying the function type with parenthesys,
    else trying the non-function type with parenthesys. -}
    unCons <- Pars.mappingSeparated
        (choice
            [ try $ withState unConTypeToken
            , try . Pars.parens $ withState functionUnConTypeToken
            , Pars.parens $ withState unConTypeToken
            ]
        )
    createFunType unCons
    where
        withState unConParser = do
            tst <- fetchTokenState
            unCon <- unConParser
            return (unCon, tst)

        createFunType [] =
            parserZero <?> "Zero types available to build a type token"
        createFunType [(unCon, _)] =
            return unCon
        createFunType ((unCon, tst) : t) = do
            resUnCon <- createFunType t
            {- Creating the function type -}
            return . Tree.Composite $
                Tree.TyComp (Tree.Real (Tree.ADTName (BITy.nameFunctionApp, tst)), [unCon, resUnCon], tst)

typeToken :: CustomParsec (Tree.Type With.ProgState)
typeToken = do
    tst <- fetchTokenState
    cs <- constraints
    ty <- functionUnConTypeToken
    return $ Tree.Type (cs, ty, tst)

parAdtConstructor :: CustomParsec (Tree.ADTConName With.ProgState) -> CustomParsec (Tree.ADTConstructor With.ProgState)
parAdtConstructor con = do
    tst <- fetchTokenState
    (a, as) <- Pars.application con argUnCon
    return $ Tree.ADTCon (a, as, tst)

adtConstructor :: CustomParsec (Tree.ADTConstructor With.ProgState)
{- Using dataConIdentifier instead of richDataConidentifier because the parsing of an ADTConstructor token
should be performed only in a context of adt definition where constructors with a special syntax are not
allowed. -}
adtConstructor = parAdtConstructor dataConIdentifier

richAdtConstructor :: CustomParsec (Tree.ADTConstructor With.ProgState)
richAdtConstructor = parAdtConstructor richDataConIdentifier

adtConstructors :: CustomParsec [Tree.ADTConstructor With.ProgState]
adtConstructors = Pars.caseSeparated adtConstructor 

richAdtConstructors :: CustomParsec [Tree.ADTConstructor With.ProgState]
richAdtConstructors = Pars.caseSeparated richAdtConstructor

adtDeclare :: CustomParsec (Tree.ADTDeclare With.ProgState)
adtDeclare = parAdtDeclare typeIdentifier

adt :: CustomParsec (Tree.AlgebraicDataType With.ProgState)
adt = parAdt adtDeclare adtConstructors

declarationAdt :: CustomParsec (Tree.Declaration With.ProgState)
declarationAdt = do
    a <- adt
    return $ Tree.ADT a

alias :: CustomParsec (Tree.AliasAlgebraicDataType With.ProgState)
alias = do
    tst <- fetchTokenState
    (b, t) <- Pars.aliasStatement adtDeclare functionUnConTypeToken
    return $ Tree.AliasTok (b, t, tst)

declarationAlias :: CustomParsec (Tree.Declaration With.ProgState)
declarationAlias = do
    a <- alias
    return $ Tree.AliasADT a

signature :: CustomParsec (Tree.Signature With.ProgState)
signature = do
    tst <- fetchTokenState
    (s, t) <- Pars.signatureStatement symbolIdentifier typeToken
    return $ Tree.SigTok (s, t, tst)

declarationSignature :: CustomParsec (Tree.Declaration With.ProgState)
declarationSignature = do
    s <- signature
    return $ Tree.Sig s

interface :: CustomParsec (Tree.Interface With.ProgState)
interface = parInterface (parInterfaceDeclare interfaceIdentifier) constraints

declarationInterface :: CustomParsec (Tree.Declaration With.ProgState)
declarationInterface = do
    i <- interface
    return $ Tree.Intf i

genSymbolDeclaration
    :: CustomOpTable (Tree.Expression With.ProgState)
    -> CustomParsec (Either (Tree.SymbolDeclaration With.ProgState) (Tree.MultiSymbolDeclaration With.ProgState))
genSymbolDeclaration table =
    Pars.orTokens <| symbolDeclaration table <| multiSymbolDeclaration table

declarationSymbol :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Declaration With.ProgState)
declarationSymbol table = do
    genSd <- genSymbolDeclaration table
    case genSd of
        Left sd -> return $ Tree.Let sd
        Right msd -> return $ Tree.LetMulti msd

instanceHeader
    :: CustomParsec
        ( [Tree.Constraint With.ProgState]
        , Tree.IntfName With.ProgState
        , [Tree.UnConType With.ProgState]
        )
instanceHeader = do
    cs <- constraints
    (i, ts) <- Pars.application' interfaceIdentifier argUnCon
    return (cs, i, ts)

intfInstance :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Instance With.ProgState)
intfInstance table = do
    tst <- fetchTokenState
    ((cs, intf, inst), symbols) <- Pars.instanceStatement instanceHeader . many $ genericSymbolDeclaration table
    return $ Tree.InstTok (intf, cs, inst, symbols, tst)

declarationInstance :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Declaration With.ProgState)
declarationInstance table = do
    inst <- intfInstance table
    return $ Tree.Ins inst

__declaration :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Declaration With.ProgState)
__declaration table = choice
    [ try $ declarationSymbol table
    , try declarationSignature
    , try declarationAdt
    , try declarationAlias
    , try declarationInterface
    , declarationInstance table
    ]

declaration :: CustomOpTable (Tree.Expression With.ProgState) -> CustomParsec (Tree.Declaration With.ProgState)
declaration table = do
    st <- getState
    st' <- Declaration |> setObjectToParse st |> Lex.setState'
    __declaration table <?> show st'

categoryIdentifier :: CustomParsec (Tree.CategoryName With.ProgState)
categoryIdentifier = do
    tst <- fetchTokenState
    parsed <- Pars.categoryIdentifier
    return $ Tree.CatgName (parsed, tst)

categoryFixity :: CustomParsec (Tree.Fixity With.ProgState)
categoryFixity = do
    tst <- fetchTokenState
    parsed <- Pars.opsCategoryFixity
    return $ Tree.Fxty (parsed, tst)

operator :: CustomParsec (Tree.SymbolName With.ProgState)
operator = do
    tst <- fetchTokenState
    Pars.operator $ \name -> Tree.SymName (name, tst)

opsCategory :: CustomParsec (Tree.OperatorsCategory With.ProgState)
opsCategory = do
    let catSt = Pars.opsCategoryStatement
         categoryIdentifier
         operator
         categoryIdentifier
         categoryIdentifier
         categoryFixity
    tst <- fetchTokenState
    (name, ops, gts, lts, fixity) <- catSt
    return $ Tree.Catg (name, ops, gts, lts, fixity, tst)

opsCategories :: CustomParsec (Tree.OperatorsCategories With.ProgState)
opsCategories = do
    cs <- many opsCategory
    return $ Tree.OpsCatgs cs

specialIdentifier :: With.ProgState -> CustomParsec (String, With.ProgState)
specialIdentifier tst = Pars.specialIdentifier (, tst)

specialInterfaceIdentifier :: CustomParsec (Tree.IntfName With.ProgState)
specialInterfaceIdentifier = do
    tst <- fetchTokenState
    idSt <- specialIdentifier tst
    return $ Tree.IntfName idSt

specialTypeIdentifier :: CustomParsec (Tree.ADTName With.ProgState)
specialTypeIdentifier = do
    tst <- fetchTokenState
    idSt <- specialIdentifier tst
    return $ Tree.ADTName idSt

specialConstraints :: CustomParsec [Tree.Constraint With.ProgState]
specialConstraints = parConstraints specialInterfaceIdentifier

{- It parses an interface declaration, but with a special interface identifier which goes out of the
language parsing rules. -}
specialInterface :: CustomParsec (Tree.Interface With.ProgState)
specialInterface = parInterface (parInterfaceDeclare specialInterfaceIdentifier) specialConstraints

specialAdtDeclare :: CustomParsec (Tree.ADTDeclare With.ProgState)
specialAdtDeclare = parAdtDeclare specialTypeIdentifier

{- It parses an adt declaration, but also with constructors which are normally out of language parsing rules
and with a type identifier which goes out of the language parsing rules. -}
specialAdt :: CustomParsec (Tree.AlgebraicDataType With.ProgState)
specialAdt = parAdt specialAdtDeclare richAdtConstructors

{- It parses an adt declaration, but also with constructors which are normally out of language parsing rules. -}
specialAdt' :: CustomParsec (Tree.AlgebraicDataType With.ProgState)
specialAdt' = parAdt adtDeclare richAdtConstructors
