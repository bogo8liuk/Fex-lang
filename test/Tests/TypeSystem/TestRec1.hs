module Tests.TypeSystem.TestRec1
    ( perform
) where

import Lib.Utils
import Lib.Monad.Utils
import Compiler.State as With
import Compiler.Ast.Common
import qualified Compiler.Ast.Typed as Ty
import Compiler.Types.Tables
import Compiler.Config.Props(nameNum)
import Tests.TypeSystem.TestState

testName :: String
testName = "Rec1"

firstMutRecSym :: String
firstMutRecSym = "x"

secondMutRecSym :: String
secondMutRecSym = "y"

firstRecSym :: String
firstRecSym = "f"

theSameOf :: String -> String -> String -> String -> Ty.LangVarCompType With.ProgState -> TyProgTest ()
theSameOf pos whichSym whichSymShouldBe tyVarRep lvcty =
    if tyVarRep == strOf lvcty
    then doNothing'
    else wrongTyping pos whichSym `shouldBe` ("the same type variable of " ++ whichSymShouldBe)

checkFirstMutRecSyms :: TypedProgram With.ProgState -> TyProgTest ()
checkFirstMutRecSyms tp = do
    tyVarRep <-
        ifExisting firstMutRecSym tp thenDoCheckFst
            `else'` couldNotFindSym firstMutRecSym
    ifExisting secondMutRecSym tp (thenDoCheckSnd tyVarRep)
        `else'` couldNotFindSym secondMutRecSym
    where
        thenDoCheckFst nVar _ _ = do
            ty <- getInfrdType nVar
            ifVarType ty (return . strOf)
                `else'` do wrongTyping "first" firstMutRecSym `shouldBe` "a type variable"

        thenDoCheckSnd tyVarRep nVar _ _ = do
            ty <- getInfrdType nVar
            ifVarType ty (theSameOf "first" secondMutRecSym firstMutRecSym tyVarRep)
                `else'` do wrongTyping "first" secondMutRecSym `shouldBe` "a type variable"

checkFirstRecSym :: TypedProgram With.ProgState -> TyProgTest ()
checkFirstRecSym tp =
    ifExisting firstRecSym tp thenDoCheck
        `else'` couldNotFindSym firstRecSym
    where
        thenDoCheck nVar _ nExpr = do
            checkNVarType nVar
            checkIsNotedPM nExpr firstRecSym

        checkNVarType nVar = do
            ty <- getInfrdType nVar
            ifFunType ty checkFirstType
                `else'` do wrongTyping "first" firstRecSym `shouldBe` "the function type"

        checkFirstType [ty1, ty2] = do
            ifVarType ty1
                (\lvcty -> do
                    checkContsNum "first sub" firstRecSym lvcty
                    let tyVarRep = strOf lvcty
                    ifFunType ty2 (checkSecondType tyVarRep)
                        `else'` do wrongTyping "second" firstRecSym `shouldBe` "the function type"
                )
                `else'` do wrongTyping "first sub" firstRecSym `shouldBe` "a type variable"
        checkFirstType _ = wrongKind "first" firstRecSym

        checkSecondType tyVarRep [ty3, ty4] = do
            ifVarType ty3
                (\lvcty -> do
                    checkContsNum "second sub" firstRecSym lvcty
                    theSameOf "second sub" firstRecSym ("the first type of " ++ firstRecSym) tyVarRep lvcty
                    checkReturnType tyVarRep ty4
                )
                `else'` do wrongTyping "second sub" firstRecSym `shouldBe` "a type variable"
        checkSecondType _ _ = wrongKind "second" firstRecSym

        checkReturnType tyVarRep ty4 =
            ifVarType ty4
                (\lvcty -> do
                    checkContsNum "return" firstRecSym lvcty
                    theSameOf "return" firstRecSym ("the first type of " ++ firstRecSym) tyVarRep lvcty
                )
                `else'` do wrongTyping "return" firstRecSym `shouldBe` "a type variable"

        checkContsNum pos sym lvcty = contInConts lvcty nameNum $ "the " ++ pos ++ " type of " ++ sym ++ " should \
            \have the Num constraint"

check :: TyProgTest ()
check = do
    tp <- get
    checkFirstMutRecSyms tp
    checkFirstRecSym tp

perform :: IO ()
perform = performingTest testName check