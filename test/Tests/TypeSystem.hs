module Tests.TypeSystem
    ( perform
) where

import qualified Tests.TypeSystem.TestBound1 as TestBound1
import qualified Tests.TypeSystem.TestMultiBound1 as TestMultiBound1
import qualified Tests.TypeSystem.TestRec1 as TestRec1

perform :: IO ()
perform = do
    TestBound1.perform
    TestMultiBound1.perform
    TestRec1.perform
