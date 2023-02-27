{-# LANGUAGE LambdaCase #-}

module Tests.TypeSystem.TestState
    ( TyProgTest
    , testFail
    , couldNotFindSym
    , wrongTyping
    , wrongTypingInType
    , wrongKind
    , shouldBe
    , ifExisting
    , getInfrdType
    , checkArgsNo
    , checkStrRep
    , checkType
    , checkIsFunType
    , ifFunType
    , ifVarType
    , ifNonFunType
    , ifXType
    , ifXType'
    , ifIntType
    , ifCharType
    , ifByteStringType
    , ifNTuple
    , checkConts
    , noConts
    , contInConts
    , getNVarFromExpr
    , checkIsNotedPM
    , performingTest
    , module Control.Monad.State
) where

import Lib.Utils
import Data.Array
import Control.Monad.State
import Lib.Monad.Utils
import System.Exit(exitFailure)
import Compiler.Ast.Common
import Compiler.State as With
import qualified Compiler.Ast.Typed as Ty
import Compiler.Types.Tables
import Compiler.Config.Types
import Compiler(typeInference)
import Tests.TestUtils

type TyProgTest = StateT (TypedProgram With.ProgState) IO

testFail :: String -> TyProgTest a
testFail reason = liftIO $ do
    putStrLn $ "Test failure: " ++ reason
    exitFailure

couldNotFindSym :: String -> TyProgTest a
couldNotFindSym symRep = testFail $ "symbol " ++ symRep ++ " should exist in the typed program"

wrongTyping :: String -> String -> String -> TyProgTest a
wrongTyping pos sym whatShouldBe =
    testFail ("the " ++ pos ++ " type of symbol " ++ sym ++ " should be " ++ whatShouldBe)

wrongTypingInType :: String -> String -> String -> String -> TyProgTest a
wrongTypingInType posArg posTy sym whatShouldBe =
    testFail ("the " ++ posArg ++ " argument in " ++ posTy ++ " type of symbol " ++ sym ++ " should be " ++ whatShouldBe)

wrongKind :: String -> String -> TyProgTest a
wrongKind pos sym = testFail ("bad kind of " ++ pos ++ " type of symbol " ++ sym)

shouldBe :: (a -> b) -> a -> b
shouldBe = ($)

ifExisting
    :: String
    -> TypedProgram With.ProgState
    -> (  Ty.NotedVar With.ProgState
       -> [Ty.NotedVar With.ProgState]
       -> Ty.NotedExpr With.ProgState
       -> TyProgTest a
       )
    -> (TyProgTest a -> TyProgTest a)
ifExisting symRep tp withRes cont =
    case kFind symRep tp
        :: Maybe (Ty.NotedVar With.ProgState, [Ty.NotedVar With.ProgState], Ty.NotedExpr With.ProgState) of
        Nothing -> cont
        Just (nVar, nVars, nExpr) -> withRes nVar nVars nExpr

getInfrdType
    :: (Show (tok With.ProgState), Ty.ActualType tok)
    => tok With.ProgState
    -> TyProgTest (Ty.LangHigherType With.ProgState)
getInfrdType token =
    case Ty.infrdTypeOf token of
        Nothing -> testFail $ "cannot infer the type of token " ++ show token
        Just ty -> return ty

checkArgsNo :: [a] -> Int -> String -> TyProgTest ()
checkArgsNo elems expNo err =
    if length elems /= expNo
    then testFail err
    else doNothing'

checkStrRep :: AtomStr a => a -> String -> String -> TyProgTest ()
checkStrRep token rep err =
    if strOf token /= rep
    then testFail err
    else doNothing'

checkType
    :: Ty.LangHigherType With.ProgState
    -> (Ty.LangHigherType With.ProgState -> Bool)
    -> String
    -> TyProgTest ()
checkType ty ok err =
    if ok ty
    then doNothing'
    else testFail err

checkIsFunType :: Ty.LangHigherType With.ProgState -> String -> TyProgTest ()
checkIsFunType ty =
    checkType ty $ \ty' ->
        Ty.doOnType ty'
            (const False)
            (const True)
            (const False)

ifFunType
    :: Ty.LangHigherType With.ProgState
    -> ([Ty.LangHigherType With.ProgState] -> TyProgTest a)
    -> (TyProgTest a -> TyProgTest a)
ifFunType ty withTs cont =
    Ty.doOnType ty
        (const cont)
        withTs
        (const cont)

ifVarType
    :: Ty.LangHigherType With.ProgState
    -> (Ty.LangVarCompType With.ProgState -> TyProgTest a)
    -> (TyProgTest a -> TyProgTest a)
ifVarType ty withVar cont =
    Ty.doOnType ty
        (const cont)
        (const cont)
        withVar

ifNonFunType
    :: Ty.LangHigherType With.ProgState
    -> (Ty.LangSpecType With.ProgState -> TyProgTest a)
    -> (TyProgTest a -> TyProgTest a)
ifNonFunType ty withSpec cont =
    Ty.doOnType ty
        withSpec
        (const cont)
        (const cont)

ifXType
    :: String
    -> Ty.LangSpecType With.ProgState
    -> TyProgTest a
    -> (TyProgTest a -> TyProgTest a)
ifXType tyRep lspty withTy cont =
    let tyRep' = strOf $ headOf lspty in
        if tyRep' == tyRep
        then withTy
        else cont

ifXType'
    :: String
    -> Ty.LangHigherType With.ProgState
    -> TyProgTest a
    -> (TyProgTest a -> TyProgTest a)
ifXType' tyRep ty withTy cont =
    Ty.doOnType ty
        (\lspty -> ifXType tyRep lspty withTy cont)
        (const cont)
        (const cont)

ifIntType
    :: Ty.LangHigherType With.ProgState
    -> TyProgTest a
    -> (TyProgTest a -> TyProgTest a)
ifIntType = ifXType' nameInt64

ifCharType
    :: Ty.LangHigherType With.ProgState
    -> TyProgTest a
    -> (TyProgTest a -> TyProgTest a)
ifCharType = ifXType' nameChar8

ifByteStringType
    :: Ty.LangHigherType With.ProgState
    -> TyProgTest a
    -> (TyProgTest a -> TyProgTest a)
ifByteStringType = ifXType' nameByteString

ifNTuple
    :: Ty.LangHigherType With.ProgState
    -> Int
    -> ([Ty.LangHigherType With.ProgState] -> TyProgTest a)
    -> (TyProgTest a -> TyProgTest a)
ifNTuple ty n withTy cont =
    ifNonFunType ty
        (\lspty -> do
            let tyRep = lspty |> headOf |> strOf
            let (min, max) = bounds namesTuple
            if n < min || n > max
            then testFail ("illegal tuple size indexing: " ++ show n)
            else if namesTuple ! n == tyRep
            then withTy $ argsOf lspty
            else cont
        )
        cont

checkConts
    :: Ty.LangVarCompType With.ProgState
    -> ([Ty.LangSpecConstraint With.ProgState] -> TyProgTest ())
    -> TyProgTest ()
checkConts cTyVar withConts = withConts $ Ty.contsOf cTyVar

noConts :: Ty.LangVarCompType With.ProgState -> String -> TyProgTest ()
noConts cTyVar err =
    checkConts cTyVar $ \case
        [] -> doNothing'
        _ -> testFail err

contInConts :: Ty.LangVarCompType With.ProgState -> String -> String -> TyProgTest ()
contInConts cTyVar contRep err =
    checkConts cTyVar $ \cs ->
        if any (\c -> strOf c == contRep) cs
        then doNothing'
        else testFail err

getNVarFromExpr :: Ty.NotedExpr With.ProgState -> String -> TyProgTest (Ty.NotedVar With.ProgState)
getNVarFromExpr (Ty.ExprApp (Ty.NotedApp (Ty.ExprVar nVar _) _ _) _) _ = return nVar
getNVarFromExpr (Ty.ExprApp (Ty.NotedApp expr _ _) _) symRep = getNVarFromExpr expr symRep
getNVarFromExpr _ symRep =
    testFail $ "could not fetch noted variable token from expression in symbol def of " ++ symRep

checkIsNotedPM :: Ty.NotedExpr With.ProgState -> String -> TyProgTest ()
checkIsNotedPM (Ty.ExprPM _ _) _ = doNothing'
checkIsNotedPM _ what = testFail $ "the expression of " ++ what ++ " is not the pattern matching construct"

performingTest :: String -> TyProgTest () -> IO ()
performingTest testName check = do
    printStartTest testName
    (_, _, _, _, _, _, _, tp, _) <- typeInference $ typeSystemTestTop ++ testName ++ srcTestExtension
    evalStateT check tp
    printEndTest testName
