module Compiler.Syntax.Operators
    ( Fixity(..)
    , Category(..)  --TODO: not AbDT?
    , defaultOperators
    , addOperators
    , makeTable
) where

import Data.List
import Data.Eq
import Data.Ord
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Compiler.Syntax.Lib.Info
import qualified Compiler.Syntax.Lib.SimpleParser as Pars
import qualified Compiler.Syntax.NonTranzitInsertion as NTInsert
import qualified Compiler.State as With
import Compiler.Ast.Common
import qualified Compiler.Ast.Tree as Tree

data Fixity = In Assoc | Pre | Post

instance Show Fixity where
    show (In AssocLeft) = "Infix Left"
    show (In AssocRight) = "Infix Right"
    show (In AssocNone) = "Infix None"
    show Pre = "Prefix"
    show Post = "Postfix"

data Category = Category
    { name :: String
    , operators :: [String]
    , gt :: [String] --declaring the categories this category is greater than
    , lt :: [String] --declaring the categories this category is lesser than
    , fixity :: Fixity
} deriving Show

instance Eq Category where
    (==) (Category { name = name1, operators = _, gt = gtl1, lt = ltl1, fixity = _ })
         (Category { name = name2, operators = _, gt = gtl2, lt = ltl2, fixity = _ }) =
        (all (/= name2) gtl1) && (all (/= name1) gtl2) &&   --One category does not occur as greater than the other one
        (all (/= name2) ltl1) && (all (/= name1) ltl2)      --One category does not occur as lesser than the other one

instance Ord Category where
    compare (Category { name = name1, operators = _, gt = gtl1, lt = ltl1, fixity = _ })
            (Category { name = name2, operators = _, gt = gtl2, lt = ltl2, fixity = _ }) =
        if (any (== name2) gtl1) || (any (== name1) ltl2)
        then GT
        else if (any (== name1) gtl2) || (any (== name2) ltl1)
        then LT
        else EQ

binaryApp :: Tree.SymbolName With.ProgState
          -> Tree.Expression With.ProgState
          -> Tree.Expression With.ProgState
          -> Tree.Expression With.ProgState
binaryApp s e1 e2 =
    {- Setting the state of the entire expression and the applier expression with the one of the first
    expression. The state of the entire application hint is the one of the second expression, because
    a hint comes after an expression (TODO: verify it). -}
    let e1st = stateOf e1 in
    let nst = stateOf s in
    let e2st = stateOf e2 in
        Tree.Expr (Tree.App $ Tree.AppExpr (Tree.Expr (Tree.Base s, Tree.Hint (Nothing, nst), nst), [e1, e2], e1st)
                  , Tree.Hint (Nothing, e2st)
                  , e1st
                  )

{- Unary application works the same both for prefix operators and for postfix operators. The difference
is between which state to fetch: the taken state is the one of the token which occurs first. -}
preUnaryApp :: Tree.SymbolName With.ProgState -> Tree.Expression With.ProgState -> Tree.Expression With.ProgState
preUnaryApp s e =
    let est = stateOf e in
    let nst = stateOf s in
        {- Setting the state of the entire expression as the one of the symbol name (which stands in the
        applier expression), because it comes before the applied expression. Same for application expression.
        The state of application expression hint is the one of the applied expression, because a hint
        comes after an expression (TODO: verify it). -}
        Tree.Expr (Tree.App $ Tree.AppExpr (Tree.Expr (Tree.Base s, Tree.Hint (Nothing, nst), nst), [e], nst)
                  , Tree.Hint (Nothing, est)
                  , nst
                  )

postUnaryApp :: Tree.SymbolName With.ProgState -> Tree.Expression With.ProgState -> Tree.Expression With.ProgState
postUnaryApp s e =
    let est = stateOf e in
    let nst = stateOf s in
        {- Setting the state of the entire expression as the one of the applied expression, because
        the applied expression comes before the operator. Same for the application expression.
        The state of hints is quite obvious, see also `preUnaryApp`. -}
        Tree.Expr (Tree.App $ Tree.AppExpr (Tree.Expr (Tree.Base s, Tree.Hint (Nothing, nst), nst), [e], est)
                  , Tree.Hint (Nothing, nst)
                  , est
                  )

defaultOperators :: CustomOperator (Tree.Expression With.ProgState)
defaultOperators =
    let opParser = do { tst <- With.fetchTokenState     --fetching state "dynamically"
                      ; Pars.operator (\strOp -> (strOp, tst)) --the callback builds a SymbolName
                      }
    in Pars.binary opParser (\(sn, st) -> binaryApp $ Tree.SymName (sn, st)) AssocLeft

addOperators :: [[Category]] -> Category -> Maybe [[Category]]
addOperators categories c = NTInsert.algorithm categories c

parsOpFrom :: [String] -> With.ProgState -> CustomParsec (Tree.SymbolName With.ProgState)
parsOpFrom ops st = choice (map (try . Pars.singleOperator (\strOp -> Tree.SymName (strOp, st))) ops) --building a SymbolName

makeOp :: Category -> CustomOperator (Tree.Expression With.ProgState)
makeOp (Category { name = _, operators = ops, gt = _, lt = _, fixity = fixity }) = case fixity of
    In assoc -> Infix (do { tst <- With.fetchTokenState
                          ; op <- parsOpFrom ops tst
                          ; return (binaryApp op)
                          }) assoc
    Pre -> Prefix (do { tst <- With.fetchTokenState
                      ; op <- parsOpFrom ops tst
                      ; return (preUnaryApp op)
                      })
    Post -> Postfix (do { tst <- With.fetchTokenState
                        ; op <- parsOpFrom ops tst
                        ; return (postUnaryApp op)
                        })

makeParsers :: [Category] -> [CustomOperator (Tree.Expression With.ProgState)]
makeParsers [] = []
makeParsers (c : t) = makeOp c : (makeParsers t)

makeTable :: [[Category]] -> CustomOpTable (Tree.Expression With.ProgState)
makeTable [] = []
makeTable (l : lt) = makeParsers l : (makeTable lt)
