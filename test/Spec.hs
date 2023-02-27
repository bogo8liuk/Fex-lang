{- This is the entry point for tests. -}

import qualified Tests.TypeSystem

main :: IO ()
main = do
    Tests.TypeSystem.perform
