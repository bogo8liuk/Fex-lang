module Compiler
  ( compile
) where

import Parser
import Data.Text.IO
import Prelude hiding (readFile)
import Ast (Ast)

compile :: FilePath -> IO (Either ParseError Ast)
compile path = do
  src <- readFile path
  return $ parse src
