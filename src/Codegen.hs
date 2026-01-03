{-# LANGUAGE OverloadedStrings #-}

module Codegen
  ( rustGen
) where
import Ast (Ast (..), NumberExpression (..), NumberLiteral (..))
import Data.Text.Lazy (Text, concat, pack)

rustGen :: Ast -> Text
rustGen (NumberExpression expr) =
  rustNumberExprMain exprGen
  where
    exprGen = genFromNumberExpr expr

    genFromNumberExpr :: NumberExpression -> Text
    genFromNumberExpr (Literal (NumberLiteral n)) = pack $ show n 
    genFromNumberExpr (Negate e) = Data.Text.Lazy.concat
      [ "-(" :: Text
      , genFromNumberExpr e
      , ")" :: Text
      ]
    genFromNumberExpr (Plus e1 e2) = genFromBinaryNumberExpr " + " e1 e2
    genFromNumberExpr (Minus e1 e2) = genFromBinaryNumberExpr " - " e1 e2
    genFromNumberExpr (Times e1 e2) = genFromBinaryNumberExpr " * " e1 e2
    genFromNumberExpr (Divide e1 e2) = genFromBinaryNumberExpr " / " e1 e2
    genFromNumberExpr (Modulo e1 e2) = genFromBinaryNumberExpr " % " e1 e2

    genFromBinaryNumberExpr op e1 e2 = Data.Text.Lazy.concat
      [ "(" :: Text
      , genFromNumberExpr e1
      , op
      , genFromNumberExpr e2
      , ")" :: Text
      ]

rustNumberExprMain :: Text -> Text
rustNumberExprMain expr = Data.Text.Lazy.concat ["fn main() {\n\
\    let res = " :: Text, expr, ";\n\
\\n\
\    println!(\"{}\", res); \n\
\}\n" :: Text]
