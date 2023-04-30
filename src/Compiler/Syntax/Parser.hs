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
makeCategory (Tree.Catg (Tree.CatgName (name, _), ops, gt, lt, Tree.Fxty (fixity, _), _))
    | fixity == inL =
        Just $ baseCategory
            { Op.fixity = Op.In AssocLeft
            }
    | fixity == inR =
        Just $ baseCategory
            { Op.fixity = Op.In AssocRight
            }
    | fixity == inN =
        Just $ baseCategory
            { Op.fixity = Op.In AssocNone
            }
    | fixity == pre =
        Just $ baseCategory
            { Op.fixity = Op.Pre
            }
    | fixity == post =
        Just $ baseCategory
            { Op.fixity = Op.Post
            }
    | otherwise = Nothing
    where
        {- Even though this makes the compiler emit a warning (not initialized field), this is done in order not to
        boiler-plate code. -}
        baseCategory =
            Op.Category
                { Op.name = name
                , Op.operators = ops'
                , Op.gt = gt'
                , Op.lt = lt'
                }

        keys = Keys.opsCategoryKeywords

        inL = Keys.infixLeftKey keys

        inR = Keys.infixRightKey keys

        inN = Keys.infixNoneKey keys

        pre = Keys.prefixKey keys

        post = Keys.postfixKey keys

        ops' = map repOf ops

        gt' = map repOf gt

        lt' = map repOf lt

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
