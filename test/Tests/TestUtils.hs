module Tests.TestUtils
    ( srcTestExtension
    , typeSystemTestTop
    , printStartTest
    , printEndTest
) where

srcTestExtension :: String
srcTestExtension = ".prog.txt"

typeSystemTestTop :: String
typeSystemTestTop = "test/Tests/TypeSystem/test"

printStartTest, printEndTest :: String -> IO ()
printStartTest test = putStrLn $ "=== Start of test " ++ test ++ " ==="
printEndTest test = putStrLn $ "=== End of test " ++ test ++ " ==="