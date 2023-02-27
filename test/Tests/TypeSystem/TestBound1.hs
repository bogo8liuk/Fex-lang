module Tests.TypeSystem.TestBound1
    ( perform
) where

import Lib.Utils
import Lib.Monad.Utils
import qualified Compiler.Config.Props as BIProps
import Compiler.State as With
import Compiler.Ast.Common
import Compiler.Types.Tables
import qualified Compiler.Ast.Typed as Ty
import Tests.TypeSystem.TestState

testName :: String
testName = "Bound1"

checkf :: TypedProgram With.ProgState -> TyProgTest ()
checkf tp = do
    ifExisting "f" tp thenDoCheck
        `else'` couldNotFindSym "f"
    ifExisting "g" tp (\_ _ _ -> testFail "symbol g should not exist as a top level symbol")
        `else'` doNothing'
    where
        thenDoCheck
            :: Ty.NotedVar With.ProgState
            -> [Ty.NotedVar With.ProgState]
            -> Ty.NotedExpr With.ProgState
            -> TyProgTest ()
        thenDoCheck nVar nVars (Ty.ExprBound (Ty.NotedBound bVar bVars _ _) _ _) = do
            checkArgsNo nVars 2 "number of f parameters is different from 2"
            checkArgsNo bVars 1 "number of g parameters is different from 1"
            checkTypef nVar
            checkTypeg bVar
        thenDoCheck _ _ _ = testFail "the top level expression of f is not a bound expression"

        {- The check on f is more massive than this. -}
        checkTypeg nVarg = do
            ty <- getInfrdType nVarg
            checkIsFunType ty "the type of g function (in f) should be the function type"

        checkTypef nVarf = do
            ty <- getInfrdType nVarf
            ifFunType ty checkTypesf
                `else'` testFail "type of f should be the function type"

        checkTypesf :: [Ty.LangHigherType With.ProgState] -> TyProgTest ()
        checkTypesf [ty, ty'] = do
            ifVarType ty (`noConts` "first type (type var.) of f should not have type constraints")
                `else'` testFail "first type of should be a type variable"
            ifFunType ty' thenCheckSndTypef
                `else'` testFail "second type of f should be the function type"
        checkTypesf _ = testFail "type of f built in a bad way"

        thenCheckSndTypef :: [Ty.LangHigherType With.ProgState] -> TyProgTest ()
        thenCheckSndTypef [ty, ty'] = do
            varRep <- ifVarType ty
                (\lvcty -> do
                    contInConts lvcty BIProps.nameNum "second type variable of f type has not Num constraint"
                    return $ strOf lvcty
                )
                `else'` testFail "second type of f should be a type variable"
            ifVarType ty'
                (\lvcty -> do
                    contInConts lvcty BIProps.nameNum "return type of f has not Num constraint"
                    checkStrRep lvcty varRep
                        "second type variable and return type of f should have the same string representation"
                )
                `else'` testFail "return type of f should be a type variable"
        thenCheckSndTypef _ = testFail "second type of f built in a bad way"

checkf' :: TypedProgram With.ProgState -> TyProgTest ()
checkf' tp =
    ifExisting "f'" tp thenDoCheck
        `else'` couldNotFindSym "f'"
    where
        thenDoCheck
            :: Ty.NotedVar With.ProgState
            -> [Ty.NotedVar With.ProgState]
            -> Ty.NotedExpr With.ProgState
            -> TyProgTest ()
        thenDoCheck _ _
            (Ty.ExprBound
                (Ty.NotedBound nVark _ _ _)
                (Ty.ExprBound
                    (Ty.NotedBound nVarc _ _ _)
                    (Ty.ExprBound
                        (Ty.NotedBound nVarq _ _ _)
                        _ _
                    )
                    _
                )
                _
            ) =
            let nVarRepk = strOf nVark in
            let nVarRepc = strOf nVarc in
            let nVarRepq = strOf nVarq in
                if strOf nVark == "k" &&
                   strOf nVarc == "c" &&
                   strOf nVarq == "q"
                then doNothing'
                else testFail $ "the nested bound expressions of f' should have the following variables (in order): \
                    \k, c, q; but we have: " ++ nVarRepk ++ ", " ++ nVarRepc ++ ", " ++ nVarRepq
        thenDoCheck _ _ _ = testFail "expression of f' should have the structure of three nested bound expressions"

checkf''' :: TypedProgram With.ProgState -> TyProgTest ()
checkf''' tp = do
    ifExisting "f'''" tp thenDoCheck
        `else'` couldNotFindSym "f'''"
    where
        thenDoCheck
            :: Ty.NotedVar With.ProgState
            -> [Ty.NotedVar With.ProgState]
            -> Ty.NotedExpr With.ProgState
            -> TyProgTest ()
        thenDoCheck nVar _ _ = do
            ty <- getInfrdType nVar
            ifFunType ty thenCheckTypef'''
                `else'` testFail "type of f''' should be the function type"

        thenCheckTypef''' [ty, ty'] =
            ifFunType ty' (const $ testFail "second type of f''' should not be the function type")
                `else'` (ifVarType ty (`noConts` "the first type of f''' should not have constraints")
                    `else'` doNothing')
        thenCheckTypef''' _ = testFail "type of f''' built in a bad way"

check :: TyProgTest ()
check = do
    tp <- get
    checkf tp
    checkf' tp
    --For now, the test on f'' does not exist, it just has to pass the type-inference algorithm.
    checkf''' tp

perform :: IO ()
perform = performingTest testName check
