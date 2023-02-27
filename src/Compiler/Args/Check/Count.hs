{- This module just counts the arguments of adts and aliases. -}
module Compiler.Args.Check.Count
    ( UnionTok(..)
    , ArgsMap
    , countOp
    -- re-exporting some of map library functions to do operations on ArgsMap
    , Map.lookup
) where

import Lib.Utils((<|))
import Data.List(foldl')
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
type ArgsMap = Map (String, UnionTok) (Int, With.ProgState)

adtCountOp :: ArgsMap -> Raw.AstOp With.ProgState e ArgsMap
adtCountOp m = Raw.safeLookupAdt m (\argsMap adt ->
    insert (strOf $ Raw.adtNameFrom adt, ItemAdt)
           (length $ argsOf adt, stateOf adt)
           argsMap
    )

aliasCountOp :: ArgsMap -> Raw.AstOp With.ProgState e ArgsMap
aliasCountOp m = Raw.safeLookupAlias m (\argsMap alias ->
    insert (strOf $ Raw.aliasNameFrom alias, ItemAlias)
           (length $ argsOf alias, stateOf alias)
           argsMap
    )

propCountOp :: ArgsMap -> Raw.AstOp With.ProgState e ArgsMap
propCountOp m = Raw.safeLookupProp m (\argsMap intf ->
    insert (strOf $ Raw.intfNameFrom intf, ItemProp)
           (length $ argsOf intf, stateOf intf)
           argsMap
    )

countOp :: Raw.AstOp With.ProgState e ArgsMap
countOp = do
    m <- adtCountOp empty
    m' <- aliasCountOp m
    propCountOp m'
{-
count :: Raw.Program With.ProgState -> ArgsMap
count p = case runAstOp p () countOp of
    Left _ -> empty     --Unreachable state
    Right (argsMap, _) -> argsMap-}
