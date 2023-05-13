{- Primitive types data. This module has to be used to build, manipulate, handle and
fetch information from primitive types. -}
module Compiler.Config.Types
    ( SizeType(..)
    , Type(..)
    , nameInt
    , int
    , nameNat
    , nat
    , nameDouble
    , double
    , nameBool
    , trueConBool
    , falseConBool
    , boolCons
    , bool
    , nameChar
    , char
    , nameByteString
    , byteString
    , nameFunctionApp
    , functionApp
    , nameList
    , emptyConList
    , consConList
    , listCons
    , list
    , maxTupleSize
    , nameTuple
    , namesTuple
    , conTuples
    , conTuplesList
    , getTupleCon
    , tuples
    , types
) where

import Data.Array
import GHC.Exts(maxTupleSize)
import Compiler.Config.Rep
import qualified Compiler.Config.Lexer as Lexer

data SizeType = Fixed Int | Variable

data Type =
    BT
        { source :: String
        , name :: TokenRep
        }

nameInt :: TokenRep
nameInt = tokenRepFromStr "Int"

sourceInt :: String
sourceInt = Lexer.adtKeyword ++ " " ++ tokenRepToStr nameInt ++ "\n"

int :: Type
int =
    BT
        { source = sourceInt
        , name = nameInt
        }

nameNat :: TokenRep
nameNat = tokenRepFromStr "Nat"

sourceNat :: String
sourceNat = Lexer.adtKeyword ++ " " ++ tokenRepToStr nameNat ++ "\n"

nat :: Type
nat =
    BT
        { source = sourceNat
        , name = nameNat
        }

nameDouble :: TokenRep
nameDouble = tokenRepFromStr "Double"

sourceDouble :: String
sourceDouble = Lexer.adtKeyword ++ " " ++ tokenRepToStr nameDouble ++ "\n"

double :: Type
double =
    BT
        { source = sourceDouble
        , name = nameDouble
        }

nameBool :: TokenRep
nameBool = tokenRepFromStr "Bool"

trueConBool :: TokenRep
trueConBool = tokenRepFromStr Lexer.trueCon

falseConBool :: TokenRep
falseConBool = tokenRepFromStr Lexer.falseCon

boolCons :: [TokenRep]
boolCons = [trueConBool, falseConBool]

sourceBool :: String
sourceBool =
    Lexer.adtKeyword ++ " " ++ tokenRepToStr nameBool ++ " " ++ Lexer.definitionKeyword ++ " " ++ Lexer.trueCon ++
        " " ++ Lexer.caseSeparationKeyword ++ " " ++ Lexer.falseCon ++ "\n"

bool :: Type
bool =
    BT
        { source = sourceBool
        , name = nameBool
        }

nameChar :: TokenRep
nameChar = tokenRepFromStr "Char"

sourceChar :: String
sourceChar = Lexer.adtKeyword ++ " " ++ tokenRepToStr nameChar ++ "\n"

char :: Type
char =
    BT
        { source = sourceChar
        , name = nameChar
        }

nameByteString :: TokenRep
nameByteString = tokenRepFromStr "String"

sourceByteString :: String
sourceByteString = Lexer.adtKeyword ++ " " ++ tokenRepToStr nameByteString ++ "\n"

byteString :: Type
byteString =
    BT
        { source = sourceByteString
        , name = nameByteString
        }

nameFunctionApp :: TokenRep
nameFunctionApp = tokenRepFromStr "App"

sourceFunctionApp :: String
sourceFunctionApp = Lexer.adtKeyword ++ " " ++ tokenRepToStr nameFunctionApp ++ " a b\n"

functionApp :: Type
functionApp =
    BT
        { source = sourceFunctionApp
        , name = nameFunctionApp
        }

nameList :: TokenRep
nameList = tokenRepFromStr "List"

emptyConList :: TokenRep
emptyConList = tokenRepFromStr Lexer.listEmptyCon

consConList :: TokenRep
consConList = tokenRepFromStr Lexer.listConsCon

listCons :: [TokenRep]
listCons = [emptyConList, consConList]

--TODO: add cons to the source
sourceList :: String
sourceList = Lexer.adtKeyword ++ " " ++ tokenRepToStr nameList ++ " a\n"

list :: Type
list =
    BT
        { source = sourceList
        , name = nameList
        }

nameTuple :: TokenRep
nameTuple = tokenRepFromStr "Tuple"

namesTuple :: Array Int TokenRep
namesTuple =
    array (2, maxTupleSize) [ (i, tokenRepFromStr (tokenRepToStr nameTuple ++ show i)) | i <- [2..maxTupleSize] ]

{- A 2-maxTupleSize array of tuple constructors. -}
conTuples :: Array Int TokenRep
conTuples = array (2, maxTupleSize) [ (i, mkTupleCon i) | i <- [2..maxTupleSize] ]

conTuplesList :: [(Int, TokenRep)]
conTuplesList = assocs conTuples

mkTupleCon :: Int -> TokenRep
mkTupleCon i =
    tokenRepFromStr $
        Lexer.tupleConSugarStart ++
        concat [ Lexer.tupleConSugarSep | _ <- [1..(i - 1)] ] ++
        Lexer.tupleConSugarEnd

getTupleCon :: Int -> Maybe TokenRep
getTupleCon i =
    let (minTup, maxTup) = bounds conTuples in
        if i < minTup || i > maxTup
        then Nothing
        else Just $ namesTuple ! i

mkTupleParams :: Int -> [TokenRep]
mkTupleParams i = [ tokenRepFromStr ("e" ++ show j) | j <- [1..i] ]

mkTupleSource :: Int -> String
mkTupleSource i =
    let params = concatMap (\p -> " " ++ tokenRepToStr p) $ mkTupleParams i in
    let con = tokenRepToStr $ conTuples ! i in
    let tyCon = tokenRepToStr $ namesTuple ! i in
        Lexer.adtKeyword ++ " " ++ tyCon ++ params ++ " " ++ Lexer.definitionKeyword ++ " " ++ con ++ params ++ "\n"

tuples :: [Type]
tuples =
    [ BT
        { source = mkTupleSource i
        , name = namesTuple ! i
        }
        | i <- [2..maxTupleSize]
    ]

types :: [Type]
types =
    [ int
    , nat
    , double
    , bool
    , char
    , byteString
    , functionApp
    , list
    ] ++ tuples
