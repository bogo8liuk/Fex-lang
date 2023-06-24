{- |
Module : Compiler.Phase
Description : Phases of the compiler
Copyright : (c) Luca Borghi, 2022
License : GPL-3
Stability : experimental

Phases of the compiler
-}

module Compiler.Phase
    ( CompPhase(..)
) where

--TODO: missing any?
{- |
A specific phase or task of the compiler.
-}
data CompPhase
    -- | A phase where source code is read
    = Read
    | Parsing
    | BuiltinTokensAdd !BuiltinTokensPhase
    | Desugar !DesugaringPhase
    -- | Everything concerning static checks
    | Check !CheckPhase
    | PrepareTyInf
    | Inference !InferencePhase
    -- | Static dispatch
    | Dispatch
    -- | IR code generation
    | Codegen !CodegenPhase
    -- | Stuff happening in the compiler backend, so after code generation
    | Backend
    deriving (Eq, Ord)

data BuiltinTokensPhase
    = Types
    | TypeClasses
    deriving (Eq, Ord)

data CheckPhase
    = Names
    | Args --TODO: is this necessary?
    | Constraints
    deriving (Eq, Ord)

data InferencePhase
    = Kind
    | Type
    | Constraint
    | DataCon  --TODO: is this an inference phase?
    | Instance --TODO: is this an inference phase?
    deriving (Eq, Ord)

data DesugaringPhase
    = Aliases
    | Signatures --TODO: is this necessary?
    | Lambdas
    | DeepPatternMatching
    | PatternMatchScrutinee
    | WidePatternMatching
    deriving (Eq, Ord)

data CodegenPhase
    = GHCCore
    deriving (Eq, Ord)
