{-# LANGUAGE OverloadedStrings #-}

module Llvm
  ( llvmGen
) where
import Ast (Ast (..), NumberExpression (..), NumberLiteral (..))
import Data.Text.Lazy (Text, concat, pack)

llvmGen :: Ast -> Text
llvmGen (NumberExpression expr) =
  let (e, var) = exprGen in
    llvmNumberExprMain e var
  where
    exprGen :: (Text, Text)
    exprGen =
      let (e, i) = genFromNumberExpr ("" :: Text) 0 expr in
        (e, Data.Text.Lazy.concat ["%res" :: Text, pack $ show (i - 1)])

    genFromNumberExpr :: Text -> Int -> NumberExpression -> (Text, Int)
    genFromNumberExpr accExpr index (Literal (NumberLiteral n)) =
      (Data.Text.Lazy.concat [accExpr, "\n%res" :: Text, pack $ show index,
      " = add i32 0, " :: Text, pack $ show n], index + 1)
    genFromNumberExpr accExpr index (Negate e) =
      let (subExpr, newIndex) = genFromNumberExpr accExpr index e in
        (Data.Text.Lazy.concat [subExpr, "\n%res" :: Text, pack $ show newIndex,
        " = sub i32 0, %res" :: Text, pack $ show (newIndex - 1)], newIndex + 1)
    genFromNumberExpr accExpr index (Plus e1 e2) =
      let (tmpExpr, tmpIndex) = genFromNumberExpr accExpr index e1 in
      let (subExpr, newIndex) = genFromNumberExpr tmpExpr tmpIndex e2 in
        (Data.Text.Lazy.concat [subExpr, "\n%res" :: Text, pack $ show newIndex,
        " = add i32 %res" :: Text, pack $ show (tmpIndex - 1), ", %res" :: Text,
        pack $ show (newIndex - 1)], newIndex + 1)
    genFromNumberExpr accExpr index (Minus e1 e2) =
      let (tmpExpr, tmpIndex) = genFromNumberExpr accExpr index e1 in
      let (subExpr, newIndex) = genFromNumberExpr tmpExpr tmpIndex e2 in
        (Data.Text.Lazy.concat [subExpr, "\n%res" :: Text, pack $ show newIndex,
        " = sub i32 %res" :: Text, pack $ show (tmpIndex - 1), ", %res" :: Text,
        pack $ show (newIndex - 1)], newIndex + 1)
    genFromNumberExpr accExpr index (Times e1 e2) =
      let (tmpExpr, tmpIndex) = genFromNumberExpr accExpr index e1 in
      let (subExpr, newIndex) = genFromNumberExpr tmpExpr tmpIndex e2 in
        (Data.Text.Lazy.concat [subExpr, "\n%res" :: Text, pack $ show newIndex,
        " = mul i32 %res" :: Text, pack $ show (tmpIndex - 1), ", %res" :: Text,
        pack $ show (newIndex - 1)], newIndex + 1)
    genFromNumberExpr accExpr index (Divide e1 e2) =
      let (tmpExpr, tmpIndex) = genFromNumberExpr accExpr index e1 in
      let (subExpr, newIndex) = genFromNumberExpr tmpExpr tmpIndex e2 in
        (Data.Text.Lazy.concat [subExpr, "\n%res" :: Text, pack $ show newIndex,
        " = udiv i32 %res" :: Text, pack $ show (tmpIndex - 1), ", %res" :: Text,
        pack $ show (newIndex - 1)], newIndex + 1)
    genFromNumberExpr accExpr index (Modulo e1 e2) =
      let (tmpExpr, tmpIndex) = genFromNumberExpr accExpr index e1 in
      let (subExpr, newIndex) = genFromNumberExpr tmpExpr tmpIndex e2 in
        (Data.Text.Lazy.concat [subExpr, "\n%res" :: Text, pack $ show newIndex,
        " = urem i32 %res" :: Text, pack $ show (tmpIndex - 1), ", %res" :: Text,
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
  "\n  %fmt = getelementptr [4 x i8], [4 x i8]* @.result, i32 0, i32 0\n\
  \  call i32 (i8*, ...) @printf(i8* %fmt, i32 " :: Text, var, ")\n\
  \  ret i32 0\n\
  \}\n\
  \" :: Text]
