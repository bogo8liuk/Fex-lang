{- |
Module : Compiler.Lib.Types
Description : General compiler types
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

General types and data structures.
-}

module Compiler.Lib.Types
    (
    -- * Applications
      LeftApplication(..)
    , LeftAutoApplication(..)
    , RightApplication(..)
    , RightAutoApplication(..)
    , AutoApplication(..)
    -- * Functions on applications
    , leftToAutoLeft
    , rightToAutoRight
) where

{- | Application of items with left associativity. You can think:

> LeftApplication A B

as

> (...((((A) B) B) B) ... B)
-}
data LeftApplication applier applied a
    = LeftHead (applier a)
    | LeftApp (LeftApplication applier applied a) (applied a)

{- |
Same of `LeftApplication`, but with just one item (the head item is the same of
the applied items). You can think:

> LeftAutoApplication B

as

> (...((((B) B) B) B) ... B)
-}
data LeftAutoApplication app a
    = LeftAutoHead (app a)
    | LeftAutoApp (LeftAutoApplication app a) (app a)

{- |
Application of items with right associativity. You can think:

> RightApplication A B

as

> (A ... (A (A (A (B))))...)
-}
data RightApplication applier applied a
    = RightTail (applied a)
    | RightApp (applier a) (RightApplication applier applied a)

{- |
Same of `RightApplication`, but with just one item (the head item is the same of
the applied items). You can think:

> RightAutoApplication B

as

> (B ... (B (B (B (B))))...)
-}
data RightAutoApplication app a
    = RightAutoTail (app a)
    | RightAutoApp (app a) (RightAutoApplication app a)

{- |
Auto-application of items. You can think:

> AutoApplication X

as

> X X
-}
data AutoApplication app a = AutoApp (app a) (app a)

{- |
Given a `LeftApplication applier applied` value where `applier` and `applied`
are the same type, it builds up `LeftAutoApplication applier` semantically
identical to the previous `LeftApplication` value.
-}
leftToAutoLeft :: LeftApplication app app a -> LeftAutoApplication app a
leftToAutoLeft (LeftHead x) = LeftAutoHead x
leftToAutoLeft (LeftApp leftApp y) = LeftAutoApp (leftToAutoLeft leftApp) y

{- |
Given a `RightApplication applier applied` value where `applier` and `applied`
are the same type, it builds up `RightAutoApplication applier` semantically
identical to the previous `RightApplication` value.
-}
rightToAutoRight :: RightApplication app app a -> RightAutoApplication app a
rightToAutoRight (RightTail x) = RightAutoTail x
rightToAutoRight (RightApp y rightApp) =
    RightAutoApp y (rightToAutoRight rightApp)
