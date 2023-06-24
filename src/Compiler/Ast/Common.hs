{- |
Module : Compiler.Ast.Common
Description : Common interfaces for ast
Copyright : (c) Luca Borghi, 2022
License : GPL-3
Stability : experimental

Common interfaces for the abstract syntax tree, regardless the nature of the tree (typed, untyped or something else).
Some notation: we will refer to nodes of the ast as "tokens", for example, a type definition (with presumably a name
and list of constructors) is a token; we will refer to opaque objects denoting some tokens as "items". An item should
keep no information about a token, including its structure and it shouldn't be used as an identifier for the token.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Compiler.Ast.Common
    (
    --( module Compiler.Config.Rep
    --, HasArgs(..)
    --, HasHead(..)
    --, HasState(..)
    --, AtomRep(..)
    --, strOf
    --, Binder(..)
    --, Diff(..)
    --, CxtDiff(..)
      Item(..)
    , TokenRep
    , HasRepresentation(..)
    , textOf
    , Id(..)
    , HasId(..)
) where

import Utils.TypeAlias (Typing (unTyping))
import Data.Text (Text)

{- |
The class for "itemization" of tokens.
-}
class Item token item where
    {- |
    It returns the item of a token.
    -}
    itemOf :: token -> item

data RepresentationType
type TokenRep = RepresentationType `Typing` Text

{- |
The class for tokens which can be represented as `TokenRep`.
-}
class HasRepresentation token where
    representationOf :: token -> TokenRep

    rawRepresentationOf :: token -> Text
    rawRepresentationOf = unTyping . representationOf

textOf :: TokenRep -> Text
textOf = unTyping

data Id
    -- | A program variable
    = VarId !TokenRep
    | DataConId !TokenRep
    | TypeConId !TokenRep
    | TypeVarId !TokenRep
    | KindConId !TokenRep
    | KindVarId !TokenRep
    -- | A constructor coming from a type class
    | ConstrConId !TokenRep
    -- | Identifier for tokens solved at compile-time
    | CompTimeTokenId !TokenRep

{- |
Tokens which can be identified by an `Id`.
-}
class HasId token where
    idOf :: token -> Id

{-
-- Type-classes: HasArgs

{- Type-class for those tokens which have the notion of arguments. -}
class HasArgs t as | t -> as where
    argsOf :: t -> [as]

{- The dual of HasArgs. A type which implements both HasArgs and HasHead should be seen like this:
    (<head>, [<arg1>, <arg2>, <arg3>, etc.])
        |                   |
        V                   V
    result of           result of
     headOf               argsOf
-}
class HasHead t h | t -> h where
    headOf :: t -> h

{- Type-class for those tokens which have the notion of state. -}
class HasState t where
    stateOf :: t st -> st

{- It returns the atomic representation of a token. -}
class AtomRep t where
    repOf :: t -> TokenRep

strOf :: AtomRep t => t -> String
strOf = tokenRepToStr . repOf

{- `Binder` defines operations which have to be executed on the "bindings" of a token (`tok`), namely the tokens
(`bindtok`) which should give a scope to other tokens (the "scoped" ones, always `bindtok`) in a token (`tok`).
The order of evaluation of bindings is implementation-defined. -}
class Binder tok bindtok where
    onBindingsOf :: tok -> x -> (bindtok -> x -> (bindtok, x)) -> (tok, x)
    onScopedOf :: tok -> x -> (bindtok -> x -> (bindtok, x)) -> (tok, x)

{- Operations of difference between tokens. -}
class Diff tok m where
    (-\) :: tok -> tok -> m tok
    (-\) t1 t2 = t1 *\ [t2]
    (*\) :: tok -> [tok] -> m tok

{- Operations of difference between tokens that need a context where to do computations. -}
class CxtDiff tok cxt m where
    cxtMinus :: tok -> tok -> cxt -> m tok
    cxtMinus t1 t2 = cxtDiff t1 [t2]
    cxtDiff :: tok -> [tok] -> cxt -> m tok
    -}
