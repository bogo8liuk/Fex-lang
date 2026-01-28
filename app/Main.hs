import System.Exit(exitFailure)
import System.Environment
import System.Console.GetOpt
import Data.Text.Lazy.IO (putStrLn)
import Compiler ( compile, Target(..) )

main :: IO ()
main = do
  args <- getArgs
  let call = getOpt RequireOrder options args
  path <- getPath call
  target <- getTarget call
  res <- compile path target
  Data.Text.Lazy.IO.putStrLn res

printUsage :: IO ()
printUsage = do
    compilerName <- getProgName
    Prelude.putStrLn $ usageInfo compilerName options

getPath :: ([Maybe Target], [String], [String]) -> IO String
getPath (_, [path], _) = return path
getPath _ = do
    printUsage
    exitFailure

getTarget :: ([Maybe Target], [String], [String]) -> IO Target
getTarget ([], _, _) = return Rust
getTarget ([Just Rust], _, _) = return Rust
getTarget ([Just Llvm], _, _) = return Llvm
getTarget _ = do
    printUsage
    exitFailure

options =
    [ targetOption
    ]

targetOption = Option ['t'] ["target"] (ReqArg target "TARGET")
    "The compilation target code"
    where
        target "rust" = Just Rust
        target "llvm" = Just Llvm
        target _ = Nothing
