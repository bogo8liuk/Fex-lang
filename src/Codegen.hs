{-# LANGUAGE OverloadedStrings #-}

module Codegen
  ( rustGen
) where
import Ast (Ast (..), NumberExpression (..), NumberLiteral (..))
import Data.Text.Lazy (Text, concat, pack)

rustGen :: Ast -> Text
rustGen (NumberExpression expr) =
  let exprGen = genFromNumberExpr expr in
  rustNumberExprMain exprGen
  where
    genFromNumberExpr :: NumberExpression -> Text
    genFromNumberExpr (Literal (NumberLiteral n)) = pack $ show n 
    genFromNumberExpr (Negate e) = Data.Text.Lazy.concat
      [ "-(" :: Text
      , genFromNumberExpr e
      , ")" :: Text
      ]
    genFromNumberExpr (Plus e1 e2) = Data.Text.Lazy.concat
      [ "(" :: Text
      , genFromNumberExpr e1
      , " + " :: Text
      , genFromNumberExpr e2
      , ")" :: Text
      ]
    genFromNumberExpr (Minus e1 e2) = Data.Text.Lazy.concat
      [ "(" :: Text
      , genFromNumberExpr e1
      , " - " :: Text
      , genFromNumberExpr e2
      , ")" :: Text
      ]
    genFromNumberExpr (Times e1 e2) = Data.Text.Lazy.concat
      [ "(" :: Text
      , genFromNumberExpr e1
      , " * " :: Text
      , genFromNumberExpr e2
      , ")" :: Text
      ]
    genFromNumberExpr (Divide e1 e2) = Data.Text.Lazy.concat
      [ "(" :: Text
      , genFromNumberExpr e1
      , " / " :: Text
      , genFromNumberExpr e2
      , ")" :: Text
      ]
    genFromNumberExpr (Modulo e1 e2) = Data.Text.Lazy.concat
      [ "(" :: Text
      , genFromNumberExpr e1
      , " % " :: Text
      , genFromNumberExpr e2
      , ")" :: Text
      ]

rustNumberExprMain :: Text -> Text
rustNumberExprMain expr = Data.Text.Lazy.concat ["fn main() {\n\
\    let res = " :: Text, expr, ";\n\
\\n\
\    println!(\"{}\", res); \n\
\}\n" :: Text]
