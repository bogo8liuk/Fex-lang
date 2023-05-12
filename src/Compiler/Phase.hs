module Compiler.Phase
    ( CompPhase(..)
) where

--TODO: finish it
data CompPhase =
      Parsing
    | Dispatch
    | Backend
    deriving (Eq, Ord)