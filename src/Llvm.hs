{-# LANGUAGE OverloadedStrings #-}

module Llvm
  ( llvmGen
) where
import Ast (Ast (..), NumberExpression (..), NumberLiteral (..))
import Data.Text.Lazy (Text, concat, pack)

llvmGen :: Ast -> Text
llvmGen (NumberExpression expr) =
  uncurry llvmNumberExprMain exprGen
  where
    exprGen :: (Text, Text)
    exprGen =
      let (e, i) = genFromNumberExpr ("" :: Text) 0 expr in
        (e, Data.Text.Lazy.concat ["%res" :: Text, pack $ show (i - 1)])

    genFromNumberExpr :: Text -> Int -> NumberExpression -> (Text, Int)
    genFromNumberExpr accExpr index (NumLiteral (NumberLiteral n)) =
      (Data.Text.Lazy.concat [accExpr, "\n%res" :: Text, pack $ show index,
      " = add i32 0, " :: Text, pack $ show n], index + 1)
    genFromNumberExpr accExpr index (Negate e) =
      let (subExpr, newIndex) = genFromNumberExpr accExpr index e in
        (Data.Text.Lazy.concat [subExpr, "\n%res" :: Text, pack $ show newIndex,
        " = sub i32 0, %res" :: Text, pack $ show (newIndex - 1)], newIndex + 1)
    genFromNumberExpr accExpr index (Plus e1 e2) =
      genFromBinaryNumberExpr ("add" :: Text) accExpr index e1 e2
    genFromNumberExpr accExpr index (Minus e1 e2) =
      genFromBinaryNumberExpr ("sub" :: Text) accExpr index e1 e2
    genFromNumberExpr accExpr index (Times e1 e2) =
      genFromBinaryNumberExpr ("mul" :: Text) accExpr index e1 e2
    genFromNumberExpr accExpr index (Divide e1 e2) =
      genFromBinaryNumberExpr ("udiv" :: Text) accExpr index e1 e2
    genFromNumberExpr accExpr index (Modulo e1 e2) =
      genFromBinaryNumberExpr ("urem" :: Text) accExpr index e1 e2

    genFromBinaryNumberExpr :: Text -> Text -> Int -> NumberExpression ->
      NumberExpression -> (Text, Int)
    genFromBinaryNumberExpr op accExpr index e1 e2 =
      let (tmpExpr, tmpIndex) = genFromNumberExpr accExpr index e1 in
      let (subExpr, newIndex) = genFromNumberExpr tmpExpr tmpIndex e2 in
        (Data.Text.Lazy.concat [subExpr, "\n%res" :: Text, pack $ show newIndex,
        " = " :: Text, op, " i32 %res" :: Text, pack $ show (tmpIndex - 1), ", %res" :: Text,
        pack $ show (newIndex - 1)], newIndex + 1)

llvmNumberExprMain :: Text -> Text -> Text
llvmNumberExprMain expr var = Data.Text.Lazy.concat ["\
  \declare i32 @printf(i8*, ...)\n\
  \\n\
  \@.result = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n\
  \\n\
  \define i32 @main(i32 %argc, i8** %argv) {\n\
  \entry:\n" :: Text,
  expr,
  "\n%fmt = getelementptr [4 x i8], [4 x i8]* @.result, i32 0, i32 0\n\
  \call i32 (i8*, ...) @printf(i8* %fmt, i32 " :: Text, var, ")\n\
  \ret i32 0\n\
  \}\n\
  \" :: Text]
