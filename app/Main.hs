import System.Exit(exitFailure)
import System.Environment
import System.Console.GetOpt
import Data.Text.Lazy.IO (putStrLn)
import Compiler(compile)

main :: IO ()
main = do
  args <- getArgs
  path <- getPath $ getOpt RequireOrder [] args
  res <- compile path
  Data.Text.Lazy.IO.putStrLn res

printUsage :: IO ()
printUsage = do
    compilerName <- getProgName
    print $ "USAGE: " ++ compilerName ++ " <file-path>"

getPath :: ([a], [String], [String]) -> IO String
getPath (_, [path], _) = return path
getPath _ = do
    printUsage
    exitFailure
