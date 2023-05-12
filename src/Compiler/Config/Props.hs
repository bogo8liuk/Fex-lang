module Compiler.Config.Props
    ( Prop(..)
    , source
    , nameNum
    , num
    , nameFract
    , fract
    , props
) where

import qualified Compiler.Config.Lexer as Lexer
import Compiler.Config.Rep
import Compiler.Config.Types hiding (source)

nameFunStr, nameIntStr, nameNumStr, nameFractStr :: String
nameFunStr = tokenRepToStr nameFunctionApp
nameIntStr = tokenRepToStr nameInt
nameNumStr = tokenRepToStr nameNum
nameFractStr = tokenRepToStr nameFract

newtype Prop = BP String

source :: Prop -> String
source (BP s) = s

nameNum :: TokenRep
nameNum = propConRepFromStr' "Num"

sourceNum :: String
sourceNum =
    Lexer.interfaceKeyword ++ " " ++ nameNumStr ++ " a " ++ Lexer.definitionKeyword ++ "\n\
    \    " ++ Lexer.signatureKeyword ++ " (+) " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunStr ++
    " a (" ++ nameFunStr ++ " a a)\n\
    \    " ++ Lexer.signatureKeyword ++ " (-) " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunStr ++
    " a (" ++ nameFunStr ++ " a a)\n\
    \    " ++ Lexer.signatureKeyword ++ " (*) " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunStr ++
    " a (" ++ nameFunStr ++ " a a)\n\
    \    " ++ Lexer.signatureKeyword ++ " negate " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunStr ++
    " a a\n\
    \    " ++ Lexer.signatureKeyword ++ " abs " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunStr ++
    " a a\n\
    \    " ++ Lexer.signatureKeyword ++ " sign " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunStr ++
    " a a\n\
    \    " ++ Lexer.signatureKeyword ++ " fromInt " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunStr ++
    " " ++ nameIntStr ++ " a\n" ++
    Lexer.endStatementKeyword ++ "\n"

num :: Prop
num = BP sourceNum

nameFract :: TokenRep
nameFract = propConRepFromStr' "Fractional"

{- TODO: right now, Fractional misses `fromRational` function, look at GHC.Real of haskell std library. -}
sourceFract :: String
sourceFract =
    Lexer.signatureKeyword ++ " " ++ nameNumStr ++ " a " ++ Lexer.constraintAppKeyword ++ " " ++ nameFractStr ++ " a " ++
    Lexer.definitionKeyword ++ "\n\
    \    " ++ Lexer.signatureKeyword ++ " (/) " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunStr ++ " a (" ++
    nameFunStr ++ " a a)\
    \    " ++ Lexer.signatureKeyword ++ " recip " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunStr ++ " a a " ++
    Lexer.endStatementKeyword ++ "\n"

fract :: Prop
fract = BP sourceFract

props :: [Prop]
props =
    [ num
    , fract
    ]