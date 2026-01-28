module Ast
  ( Ast(..)
  , StringExpression(..)
  , StringLiteral(..)
  , NumberExpression(..)
  , NumberLiteral(..)
) where
import Data.Text.Lazy (Text)

data Ast
  = NumberExpression NumberExpression
  | StringExpression StringExpression
  deriving Show

data StringExpression
  = StrLiteral StringLiteral deriving Show

data StringLiteral
  = StringLiteral Text deriving Show

data NumberExpression
  = Plus NumberExpression NumberExpression
  | Minus NumberExpression NumberExpression
  | Times NumberExpression NumberExpression
  | Divide NumberExpression NumberExpression
  | Modulo NumberExpression NumberExpression
  | Negate NumberExpression
  | NumLiteral NumberLiteral
  deriving Show

data NumberLiteral
  = NumberLiteral Integer
  deriving Show
