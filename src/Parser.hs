{-# LANGUAGE FlexibleContexts #-}

module Parser
  ( parse
  , ParseError
) where

import Ast
import Data.Text
import Text.Parsec hiding (parse, letter)
import Text.Parsec.Token
import Text.Parsec.Char
import Data.Functor.Identity (Identity)
import Text.Parsec.Expr

parse :: Text -> Either ParseError Ast
parse = runParser astParser () ""

astParser :: ParsecT Text u Identity Ast
astParser = do
  expr <- buildExpressionParser operatorsTable parseNumberLiteral
  return $ NumberExpression expr

parseNumberLiteral :: ParsecT Text u Identity NumberExpression
parseNumberLiteral = do
  n <- natural genTokenParser
  return . Literal $ NumberLiteral n

languageDef :: GenLanguageDef Text u Identity
languageDef =
  LanguageDef
    { commentStart = "(*"
    , commentEnd = "*)"
    , commentLine = "//"
    , nestedComments = True
    , identStart = identifierStart
    , identLetter = alphaNum <|> char '_' <|> char '\''
    , opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedNames = ["let", "type"]
    , reservedOpNames = ["+", "-", "*", "/"]
    , caseSensitive = True
    }

genTokenParser :: GenTokenParser Text u Identity
genTokenParser = makeTokenParser languageDef

identifierStart :: Stream s m Char => ParsecT s u m Char
identifierStart = try letter <|> char '_'

operatorsTable =
  [ [prefix "-" Negate]
  , [infix' "*" Times AssocLeft, infix' "/" Divide AssocLeft]
  , [infix' "+" Plus AssocLeft, infix' "-" Minus AssocLeft]
  ]

infix' :: String -> (a -> a -> a) -> Assoc -> Operator Text u Identity a
infix' op con =
  Infix (do
    reservedOp genTokenParser op
    return con
  )

prefix :: String -> (a -> a) -> Operator Text u Identity a
prefix op con =
  Prefix (do
    reservedOp genTokenParser op
    return con
  )

postfix :: String -> (a -> a) -> Operator Text u Identity a
postfix op con =
  Postfix (do
    reservedOp genTokenParser op
    return con
  )
