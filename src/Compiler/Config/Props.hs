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
import Compiler.Config.Types hiding (source)

newtype Prop = BP String

source :: Prop -> String
source (BP s) = s

nameNum :: String
nameNum = "Num"

sourceNum :: String
sourceNum =
    Lexer.interfaceKeyword ++ " " ++ nameNum ++ " a " ++ Lexer.definitionKeyword ++ "\n\
    \    " ++ Lexer.signatureKeyword ++ " (+) " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunctionApp ++
    " a (" ++ nameFunctionApp ++ " a a)\n\
    \    " ++ Lexer.signatureKeyword ++ " (-) " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunctionApp ++
    " a (" ++ nameFunctionApp ++ " a a)\n\
    \    " ++ Lexer.signatureKeyword ++ " (*) " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunctionApp ++
    " a (" ++ nameFunctionApp ++ " a a)\n\
    \    " ++ Lexer.signatureKeyword ++ " negate " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunctionApp ++
    " a a\n\
    \    " ++ Lexer.signatureKeyword ++ " abs " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunctionApp ++
    " a a\n\
    \    " ++ Lexer.signatureKeyword ++ " sign " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunctionApp ++
    " a a\n\
    \    " ++ Lexer.signatureKeyword ++ " fromInt " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunctionApp ++
    " " ++ nameInt ++ " a\n" ++
    Lexer.endStatementKeyword ++ "\n"

num :: Prop
num = BP sourceNum

nameFract :: String
nameFract = "Fractional"

{- TODO: right now, Fractional misses `fromRational` function, look at GHC.Real of haskell std library. -}
sourceFract :: String
sourceFract =
    Lexer.signatureKeyword ++ " " ++ nameNum ++ " a " ++ Lexer.constraintAppKeyword ++ " " ++ nameFract ++ " a " ++
    Lexer.definitionKeyword ++ "\n\
    \    " ++ Lexer.signatureKeyword ++ " (/) " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunctionApp ++ " a (" ++
    nameFunctionApp ++ " a a)\
    \    " ++ Lexer.signatureKeyword ++ " recip " ++ Lexer.stateDefinitionKeyword ++ " " ++ nameFunctionApp ++ " a a " ++
    Lexer.endStatementKeyword ++ "\n"

fract :: Prop
fract = BP sourceFract

props :: [Prop]
props =
    [ num
    , fract
    ]