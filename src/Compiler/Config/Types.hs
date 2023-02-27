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
import qualified Compiler.Config.Lexer as Lexer

data SizeType = Fixed Int | Variable

data Type =
    BT
        { source :: String
        , name :: String
        }

nameInt :: String
nameInt = "Int"

sourceInt :: String
sourceInt = Lexer.adtKeyword ++ " " ++ nameInt ++ "\n"

int :: Type
int =
    BT
        { source = sourceInt
        , name = nameInt
        }

nameNat :: String
nameNat = "Nat"

sourceNat :: String
sourceNat = Lexer.adtKeyword ++ " " ++ nameNat ++ "\n"

nat :: Type
nat =
    BT
        { source = sourceNat
        , name = nameNat
        }

nameDouble :: String
nameDouble = "Double"

sourceDouble :: String
sourceDouble = Lexer.adtKeyword ++ " " ++ nameDouble ++ "\n"

double :: Type
double =
    BT
        { source = sourceDouble
        , name = nameDouble
        }

nameBool :: String
nameBool = "Bool"

trueConBool :: String
trueConBool = Lexer.trueCon

falseConBool :: String
falseConBool = Lexer.falseCon

boolCons :: [String]
boolCons = [trueConBool, falseConBool]

sourceBool :: String
sourceBool =
    Lexer.adtKeyword ++ " " ++ nameBool ++ " " ++ Lexer.definitionKeyword ++ " " ++ trueConBool ++ " " ++
    Lexer.caseSeparationKeyword ++ " " ++ falseConBool ++ "\n"

bool :: Type
bool =
    BT
        { source = sourceBool
        , name = nameBool
        }

nameChar :: String
nameChar = "Char"

sourceChar :: String
sourceChar = Lexer.adtKeyword ++ " " ++ nameChar ++ "\n"

char :: Type
char =
    BT
        { source = sourceChar
        , name = nameChar
        }

nameByteString :: String
nameByteString = "String"

sourceByteString :: String
sourceByteString = Lexer.adtKeyword ++ " " ++ nameByteString ++ "\n"

byteString :: Type
byteString =
    BT
        { source = sourceByteString
        , name = nameByteString
        }

nameFunctionApp :: String
nameFunctionApp = "App"

sourceFunctionApp :: String
sourceFunctionApp = Lexer.adtKeyword ++ " " ++ nameFunctionApp ++ " a b\n"

functionApp :: Type
functionApp =
    BT
        { source = sourceFunctionApp
        , name = nameFunctionApp
        }

nameList :: String
nameList = "List"

emptyConList :: String
emptyConList = Lexer.listEmptyCon

consConList :: String
consConList = Lexer.listConsCon

listCons :: [String]
listCons = [emptyConList, consConList]

--TODO: add cons to the source
sourceList :: String
sourceList = Lexer.adtKeyword ++ " " ++ nameList ++ " a\n"

list :: Type
list =
    BT
        { source = sourceList
        , name = nameList
        }

nameTuple :: String
nameTuple = "Tuple"

namesTuple :: Array Int String
namesTuple = array (2, maxTupleSize) [ (i, nameTuple ++ show i) | i <- [2..maxTupleSize] ]

{- A 2-maxTupleSize array of tuple constructors. -}
conTuples :: Array Int String
conTuples = array (2, maxTupleSize) [ (i, mkTupleCon i) | i <- [2..maxTupleSize] ]

conTuplesList :: [(Int, String)]
conTuplesList = assocs conTuples

mkTupleCon :: Int -> String
mkTupleCon i =
    Lexer.tupleConSugarStart ++
    concat [ Lexer.tupleConSugarSep | _ <- [1..(i - 1)] ] ++
    Lexer.tupleConSugarEnd

getTupleCon :: Int -> Maybe String
getTupleCon i =
    let (minTup, maxTup) = bounds conTuples in
        if i < minTup || i > maxTup
        then Nothing
        else Just $ namesTuple ! i

mkTupleParams :: Int -> String
mkTupleParams i = concat [ " e" ++ show j | j <- [1..i] ]

mkTupleSource :: Int -> String
mkTupleSource i =
    let params = mkTupleParams i in
    let con = conTuples ! i in
    let name = namesTuple ! i in
        Lexer.adtKeyword ++ " " ++ name ++ params ++ " " ++ Lexer.definitionKeyword ++ " " ++ con ++ params ++ "\n"

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
