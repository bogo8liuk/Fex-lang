module Compiler
  ( compile
) where

import Parser
import Prelude hiding (readFile)
import Ast (Ast)
import Codegen (rustGen)
import Llvm (llvmGen)
import Data.Text.Lazy (Text, pack)
import Data.Text.IO (readFile)

compile :: FilePath -> IO Text--(Either ParseError Ast)
compile path = do
  src <- readFile path
  let parseRes = parse src
  case parseRes of
    Left err -> return . pack $ show err
    Right ast -> return $ rustGen ast
