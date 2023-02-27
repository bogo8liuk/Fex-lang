module Tests.TypeSystem.TestMultiBound1
    ( perform
) where

import Lib.Utils
import Lib.Monad.Utils
import Compiler.Ast.Common
import Compiler.Types.Tables
import Compiler.State as With
import Tests.TypeSystem.TestState

testName :: String
testName = "MultiBound1"

firstSym :: String
firstSym = "f"

secondSym :: String
secondSym = "g"

checkFirst :: TypedProgram With.ProgState -> TyProgTest ()
checkFirst tp =
    ifExisting firstSym tp thenDoCheck
        `else'` couldNotFindSym firstSym
    where
        thenDoCheck nVar _ _ = do
            checkType nVar

        checkType nVar = do
            ty <- getInfrdType nVar
            ifFunType ty checkFirstType
                `else'` do wrongTyping "first" firstSym `shouldBe` "function type"

        checkFirstType [ty1, ty2] = do
            ifXType' "T" ty1
                (do
                    let args = argsOf ty1
                    checkArgsTy1 args
                    ifFunType ty2 checkSecondType
                        `else'` do wrongTyping "second" firstSym `shouldBe` "function type"
                )
                `else'` do wrongTyping "first sub" firstSym `shouldBe` "concrete type T"
        checkFirstType _ = wrongKind "first" firstSym

        checkArgsTy1 [ty] =
            ifByteStringType ty doNothing'
                `else'` do wrongTypingInType "first" "first" firstSym `shouldBe` "string type"
        checkArgsTy1 _ = wrongKind "first sub" firstSym

        checkSecondType [ty2, ty3] =
            ifVarType ty2
                (const $ ifFunType ty3 checkThirdType
                    `else'` do wrongTyping "third" firstSym `shouldBe` "function type"
                )
                `else'` do wrongTyping "second type" firstSym `shouldBe` "int type"
        checkSecondType _ = wrongKind "second" firstSym

        checkThirdType [ty3, resTy] = do
            ifByteStringType ty3 doNothing'
                `else'` do wrongTyping "third sub" firstSym `shouldBe` "string type"
            ifNTuple resTy 2 checkArgsResTy
                `else'` do wrongTyping "return" firstSym `shouldBe` "2-tuple type"
        checkThirdType _ = wrongKind "third" firstSym

        checkArgsResTy [argTy1, argTy2] = do
            ifByteStringType argTy1 doNothing'
                `else'` do wrongTyping "return first sub" firstSym `shouldBe` "string type"
            ifVarType argTy2 (const doNothing')
                `else'` do wrongTyping "return second sub" firstSym `shouldBe` "int type"
        checkArgsResTy _ = wrongKind "return" firstSym

check :: TyProgTest ()
check = do
    tp <- get
    checkFirst tp

perform :: IO ()
perform = performingTest testName check