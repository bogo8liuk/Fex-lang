module Compiler
  ( Target(..)
  , compile
) where

import Parser
import Prelude hiding (readFile)
import Ast (Ast)
import Codegen (rustGen)
import Llvm (llvmGen)
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.IO (readFile)

data Target = Llvm | Rust

compile :: FilePath -> Target -> IO Text--(Either ParseError Ast)
compile path target = do
  src <- readFile path
  let parseRes = parse src
  case parseRes of
    Left err -> return . pack $ show err
    Right ast -> return $ gen target ast
  where
    gen :: Target -> (Ast -> Text)
    gen Llvm = llvmGen
    gen Rust = rustGen
