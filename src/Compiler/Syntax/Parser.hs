-- This file should be the entry point of the parser
module Compiler.Syntax.Parser
    ( program
) where

import Lib.Utils
import Lib.Result
import Text.Parsec
import Text.Parsec.Expr
import qualified Compiler.Config.Lexer as Keys
import Compiler.Ast.Common
import qualified Compiler.Syntax.Lib.Lex as Lex
import Compiler.Syntax.Lib.Info as Info
import qualified Compiler.Syntax.Lib.SimpleParser as Pars
import qualified Compiler.Syntax.Grammar as Gram
import qualified Compiler.Syntax.Operators as Op
import qualified Compiler.Ast.Tree as Tree
import qualified Compiler.State as With

instance InfoShow ParseError where
    infoShow = show

instance DebugShow ParseError where
    dbgShow = show

instance UnreachableState ParseError where
    isUnreachable _ = Nothing

makeCategory :: Tree.OperatorsCategory With.ProgState -> Maybe Op.Category
makeCategory (Tree.Catg (Tree.CatgName (name, _), ops, gt, lt, Tree.Fxty (fixity, _), _)) =
    let keys = Keys.opsCategoryKeywords in
    let inL = Keys.infixLeftKey keys in
    let inR = Keys.infixRightKey keys in
    let inN = Keys.infixNoneKey keys in
    let pre = Keys.prefixKey keys in
    let post = Keys.postfixKey keys in
    let ops' = map repOf ops in
    let gt' = map repOf gt in
    let lt' = map repOf lt in
    if fixity == inL
    then Just $
        Op.Category
            { Op.name = name
            , Op.operators = ops'
            , Op.gt = gt'
            , Op.lt = lt'
            , Op.fixity = Op.In AssocLeft
            }
    else if fixity == inR
    then Just $
        Op.Category
            { Op.name = name
            , Op.operators = ops'
            , Op.gt = gt'
            , Op.lt = lt'
            , Op.fixity = Op.In AssocRight
            }
    else if fixity == inN
    then Just $
        Op.Category
            { Op.name = name
            , Op.operators = ops'
            , Op.gt = gt'
            , Op.lt = lt'
            , Op.fixity = Op.In AssocNone
            }
    else if fixity == pre
    then Just $
        Op.Category
            { Op.name = name
            , Op.operators = ops'
            , Op.gt = gt'
            , Op.lt = lt'
            , Op.fixity = Op.Pre
            }
    else if fixity == post
    then Just $
        Op.Category
            { Op.name = name
            , Op.operators = ops'
            , Op.gt = gt'
            , Op.lt = lt'
            , Op.fixity = Op.Post
            }
    else Nothing

{- This parser fails if it cannot build a CustomOpTable for any reason, otherwise it does not consume any input
and returns a CustomOpTable. ++ [[Op.defaultOperators]] is the add of the default operator parser (with the
lowest precedency). -}
buildOpTable :: [Tree.OperatorsCategory With.ProgState] -> CustomParsec (CustomOpTable (Tree.Expression With.ProgState))
buildOpTable cs =
    case maybemap makeCategory cs of
        Nothing -> parserZero
        Just opcs ->
            case makeOpTable opcs [] of
                Nothing -> parserZero
                Just table -> table |> Op.makeTable |> (++ [[Op.defaultOperators]]) |> return 
    where
        makeOpTable [] ll = Just ll
        makeOpTable (c : t) ll =
            let newTable = Op.addOperators ll c in
                case newTable of
                    Nothing -> Nothing
                    Just catgs -> makeOpTable t catgs

programParse :: CustomParsec (Tree.Program With.ProgState)
programParse = do
    st <- makeInitialStatus |> Lex.setState'
    {- Parsing eventual starting useless text (comments, white spaces, etc.). -}
    Pars.voidText id
    (Tree.OpsCatgs cs) <- Gram.opsCategories
    table <- buildOpTable cs
    decls <- many1 (Gram.declaration table)
    eof
    return (Tree.Program decls) <?> show st

program :: SourceName -> String -> Either ParseError (Tree.Program With.ProgState)
program = runParser programParse Info.makeInitialStatus
