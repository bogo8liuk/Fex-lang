{- This module is responsible to make unique names of whatever symbol (here, the term "symbol" is a name used to
represent something which can have a name in a program). It relies on Lib.Counter. -}

module Compiler.Desugar.Names
    ( mkLamUniqueName
    , mkProgUniqueName
    , mkNestedUniqueName
    --, dispatchSuffix
    , mkDispatchName
    , mkDispatchSuffix
    --, mkDispatchName'
    --, mkDispatchName''
    --, mkDispatchNVar
    , mkUniqueObj
) where

import Lib.Utils
import Data.List(foldl')
import Data.Map.Strict as M hiding (foldl')
import Compiler.Config.Lexer(reservedIdKeyword)
import qualified Lib.Counter as C
import Unique
import Compiler.Ast.Common
import qualified Compiler.Ast.Typed as Ty

data NameEntity =
      LambdaSymbol
    | ProgSymbol
    deriving Show

reservedIdSymRep, reservedIdTyConRep, reservedIdTyVarRep, reservedIdKindVarRep, reservedIdKindConRep, reservedIdPropConRep,
 reservedIdDataConRep, reservedIdCompTokRep :: TokenRep
reservedIdSymRep = tokenRepFromStr reservedIdKeyword
reservedIdTyConRep = tokenRepFromStr reservedIdKeyword
reservedIdTyVarRep = tokenRepFromStr reservedIdKeyword
reservedIdKindVarRep = tokenRepFromStr reservedIdKeyword
reservedIdKindConRep = tokenRepFromStr reservedIdKeyword
reservedIdPropConRep = tokenRepFromStr reservedIdKeyword
reservedIdDataConRep = tokenRepFromStr reservedIdKeyword
reservedIdCompTokRep = tokenRepFromStr reservedIdKeyword

{- NB: this is very UNSAFE if two or more concatenated values of the counter can give a possible future value of
the counter. -}
mkUniqueName :: NameEntity -> C.AlphabeticCounterObj -> (TokenRep, C.AlphabeticCounterObj)
mkUniqueName ent counter =
    let (str, newCounter) = C.next counter in
        case ent of
            LambdaSymbol ->
                (tokenRepFromStr (reservedIdKeyword ++ str), newCounter)
            ProgSymbol ->
                (tokenRepFromStr (reservedIdKeyword ++ str), newCounter)

mkLamUniqueName :: C.AlphabeticCounterObj -> (SymbolRep, C.AlphabeticCounterObj)
mkLamUniqueName = mkUniqueName LambdaSymbol

mkProgUniqueName :: C.AlphabeticCounterObj -> (SymbolRep, C.AlphabeticCounterObj)
mkProgUniqueName = mkUniqueName ProgSymbol

mkNestedUniqueName :: SymbolRep -> C.CounterObj -> (SymbolRep, C.CounterObj)
mkNestedUniqueName symRep counter =
    let (str, newCounter) = C.next counter in
        ( tokenRepFromStr (reservedIdKeyword ++ tokenRepToStr symRep ++ str ++ reservedIdKeyword)
        , newCounter
        )

mkDispatchName :: C.CounterObj -> (SymbolRep, C.CounterObj)
mkDispatchName c =
    let (str, c') = C.next c in
        (tokenRepFromStr (reservedIdKeyword ++ str), c')

mkDispatchSuffix' :: SymbolRep -> Ty.LangSpecConstraint a -> SymbolRep
mkDispatchSuffix' h c =
    let lhts = argsOf c in
    let tyVars = Ty.occFirstTyVarsOfMany lhts in
    let m = mapTyVarIndex tyVars in
    let headTok = h <> reservedIdCompTokRep <> reservedIdCompTokRep <> repOf c in
        forAll <| argsOf c <| buildSuffix m `startingFrom` headTok
    where
        buildSuffix m headTok lhty =
            {- NB: passing from the string of a LangHigherType. -}
            headTok <> reservedIdCompTokRep <> tokenRepFromStr (Ty.showHeadsLHTyWith lhty $ showTyVar m)

        showTyVar m tyVar =
            let tyVarRep = repOf tyVar in
                case M.lookup tyVarRep m of
                    Nothing -> tokenRepToStr tyVarRep    --This is unreachable, filling with an arbitrary value
                    Just ix -> ix

        mapTyVarIndex tyVars =
            fst $ foldl' addTyVar (empty, C.new :: C.CounterObj) tyVars

        addTyVar (m, counter) tyVar =
            let tyVarRep = repOf tyVar in
                case M.lookup tyVarRep m of
                    Nothing ->
                        let (ix, counter') = C.next counter in
                        let m' = M.insert tyVarRep ix m in
                            (m', counter')
                    Just _ -> (m, counter)

mkDispatchSuffix :: SymbolRep -> [Ty.LangSpecConstraint a] -> SymbolRep
mkDispatchSuffix = foldl' mkDispatchSuffix'

{-
dispatchSuffix :: Ty.LangHigherType a -> String
dispatchSuffix ty = reservedIdKeyword ++ Ty.showHeadsLHTy ty

mkDispatchName :: Ty.LangSpecConstraint a -> String
mkDispatchName c = mkDispatchName'' <| repOf c <| argsOf c

mkDispatchName' :: String -> Ty.LangHigherType a -> String
mkDispatchName' symRep ty = symRep ++ dispatchSuffix ty

mkDispatchName'' :: String -> [Ty.LangHigherType a] -> String
mkDispatchName'' symRep ts = symRep ++ concatMap dispatchSuffix ts

{- NB: the suffix has to be built with dispatchSuffix. This is unsafe if the suffix does not come from that function. -}
mkDispatchNVar :: Ty.NotedVar a -> String -> Ty.NotedVar a
mkDispatchNVar nVar suffix = Ty.attachSuffix suffix nVar
-}

{- Utility to make a Unique object from a counter. -}
mkUniqueObj :: C.AlphabeticCounterObj -> (Unique, C.AlphabeticCounterObj)
mkUniqueObj c =
    let (char, n, newC) = C.nextCharAndNumber c in
        (mkUnique char (fromInteger n :: Int), newC)