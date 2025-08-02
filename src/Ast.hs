module Ast
  ( Ast(..)
  , NumberExpression(..)
  , NumberLiteral(..)
) where

data Ast
  = NumberExpression NumberExpression
  deriving Show

data NumberExpression
  = Plus NumberExpression NumberExpression
  | Minus NumberExpression NumberExpression
  | Times NumberExpression NumberExpression
  | Divide NumberExpression NumberExpression
  | Negate NumberExpression
  | Literal NumberLiteral
  deriving Show

data NumberLiteral
  = NumberLiteral Integer
  deriving Show
