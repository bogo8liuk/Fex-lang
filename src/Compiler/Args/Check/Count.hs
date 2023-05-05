{- This module just counts the arguments of adts and aliases. -}
module Compiler.Args.Check.Count
    ( UnionTok(..)
    , ArgsMap
    , countOp
    -- re-exporting some of map library functions to do operations on ArgsMap
    , Map.lookup
) where

import Data.Map.Strict as Map hiding (foldl')
import Compiler.Ast.Common
import Compiler.Ast.Tree as Raw
import Compiler.State as With

{- Type to represent different tokens. -}
data UnionTok =
      ItemAdt
    | ItemAlias
    | ItemProp
    deriving (Eq, Ord)

{- Int value represents the number of arguments of a token. Using `UnionRepTok` in a tuple as a key is
necessary, because types and properties can have the same names. -}
type ArgsMap = Map (TokenRep, UnionTok) (Int, With.ProgState)

count
    :: (AtomRep (name With.ProgState), HasArgs (tok With.ProgState) args, HasState tok)
    => UnionTok
    -> ArgsMap
    -> tok With.ProgState
    -> name With.ProgState
    -> ArgsMap
count item argsMap token tokName =
    insert
        (repOf tokName, item)
        (length $ argsOf token, stateOf token)
        argsMap

adtCountOp :: ArgsMap -> Raw.AstOp With.ProgState ArgsMap
adtCountOp m = Raw.safeLookupAdt m adtCount
    where
        adtCount argsMap adt =
            count ItemAdt argsMap adt $ Raw.adtNameFrom adt

aliasCountOp :: ArgsMap -> Raw.AstOp With.ProgState ArgsMap
aliasCountOp m = Raw.safeLookupAlias m aliasCount
    where
        aliasCount argsMap alias =
            count ItemAlias argsMap alias $ Raw.aliasNameFrom alias

propCountOp :: ArgsMap -> Raw.AstOp With.ProgState ArgsMap
propCountOp m = Raw.safeLookupProp m propCount
    where
        propCount argsMap intf =
            count ItemProp argsMap intf $ Raw.intfNameFrom intf

countOp :: Raw.AstOp With.ProgState ArgsMap
countOp = do
    m <- adtCountOp empty
    m' <- aliasCountOp m
    propCountOp m'
{-
count :: Raw.Program With.ProgState -> ArgsMap
count p = case runAstOp p () countOp of
    Left _ -> empty     --Unreachable state
    Right (argsMap, _) -> argsMap-}
