module Cli
    ( partialCompile
    , compile
) where

import System.Exit(exitFailure)
import System.Environment
import System.Console.GetOpt
import qualified Compiler(compile, compilerExecName)
import GHC(HscTarget(..), GhcLink(..))

defaultCompTarget :: HscTarget
defaultCompTarget = HscAsm

defaultCompLink :: GhcLink
defaultCompLink = NoLink

printUsage :: IO ()
printUsage = print $ "USAGE: " ++ Compiler.compilerExecName ++ " <file-path>"

getPath :: ([a], [String], [String]) -> IO String
getPath (_, [path], _) = return path
getPath _ = do
    printUsage
    exitFailure

{- It handles cmd-line arguments for the compiler. Why partial compilation? Because it takes a parametric IO
action, instead of calling directly the compiler, so this implies even a non-full compilation can executed. -}
partialCompile :: (FilePath -> IO ()) -> IO ()
partialCompile partialComp = do
    argv <- getArgs
    path <- getPath $ getOpt RequireOrder [] argv
    partialComp path

compile :: IO ()
compile = do
    argv <- getArgs
    path <- getPath $ getOpt RequireOrder [] argv
    --TODO: parse cmd-line arguments to get target and link options
    Compiler.compile path defaultCompTarget defaultCompLink