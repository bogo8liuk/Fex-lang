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

--import Lib.Utils
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

{- NB: this is very UNSAFE if two or more concatenated values of the counter can give a possible future value of
the counter. -}
mkUniqueName :: NameEntity -> C.AlphabeticCounterObj -> (String, C.AlphabeticCounterObj)
mkUniqueName ent counter =
    let (str, newCounter) = C.next counter in
        case ent of
            LambdaSymbol ->
                (reservedIdKeyword ++ str, newCounter)
            ProgSymbol ->
                (reservedIdKeyword ++ str, newCounter)

mkLamUniqueName :: C.AlphabeticCounterObj -> (String, C.AlphabeticCounterObj)
mkLamUniqueName = mkUniqueName LambdaSymbol

mkProgUniqueName :: C.AlphabeticCounterObj -> (String, C.AlphabeticCounterObj)
mkProgUniqueName = mkUniqueName ProgSymbol

mkNestedUniqueName :: String -> C.CounterObj -> (String, C.CounterObj)
mkNestedUniqueName symRep counter =
    let (str, newCounter) = C.next counter in
        (reservedIdKeyword ++ symRep ++ str ++ reservedIdKeyword, newCounter)

mkDispatchName :: C.CounterObj -> (String, C.CounterObj)
mkDispatchName c =
    let (str, c') = C.next c in
        (reservedIdKeyword ++ str, c')

mkDispatchSuffix' :: String -> Ty.LangSpecConstraint a -> String
mkDispatchSuffix' str c =
    let lhts = argsOf c in
    let tyVars = Ty.occFirstTyVarsOfMany lhts in
    let m = mapTyVarIndex tyVars in
    let headStr = str ++ reservedIdKeyword ++ reservedIdKeyword ++ strOf c in
        foldl' (buildSuffix m) headStr (argsOf c)
    where
        buildSuffix m headStr lhty =
            headStr ++ reservedIdKeyword ++ Ty.showHeadsLHTyWith lhty (showTyVar m)

        showTyVar m tyVar =
            let tyVarRep = strOf tyVar in
                case M.lookup tyVarRep m of
                    Nothing -> tyVarRep    --This is unreachable, filling with an arbitrary value
                    Just ix -> ix

        mapTyVarIndex tyVars =
            fst $ foldl' addTyVar (empty, C.new :: C.CounterObj) tyVars

        addTyVar (m, counter) tyVar =
            let tyVarRep = strOf tyVar in
                case M.lookup tyVarRep m of
                    Nothing ->
                        let (ix, counter') = C.next counter in
                        let m' = M.insert tyVarRep ix m in
                            (m', counter')
                    Just _ -> (m, counter)

mkDispatchSuffix :: String -> [Ty.LangSpecConstraint a] -> String
mkDispatchSuffix = foldl' mkDispatchSuffix'

{-
dispatchSuffix :: Ty.LangHigherType a -> String
dispatchSuffix ty = reservedIdKeyword ++ Ty.showHeadsLHTy ty

mkDispatchName :: Ty.LangSpecConstraint a -> String
mkDispatchName c = mkDispatchName'' <| strOf c <| argsOf c

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