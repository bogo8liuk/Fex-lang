{-# LANGUAGE FunctionalDependencies #-}

{- Common interfaces and functions among different representations of ast. -}
module Compiler.Ast.Common
    ( TokenRep
    , SymbolRep
    , ConcTyRep
    , TyVarRep
    , TypeRep
    , KindVarRep
    , ConcKindRep
    , KindRep
    , PropRep
    , DataConRep
    , HasArgs(..)
    , HasHead(..)
    , HasState(..)
    , AtomStr(..)
    , Binder(..)
    , Diff(..)
    , CxtDiff(..)
) where

-- String representation of tokens

{- String of whatever token -}
type TokenRep = String
{- String of a symbol (classic program variable) -}
type SymbolRep = String
{- String of a concrete (not a variable) type, namely a type-constructor -}
type ConcTyRep = String
{- String of a type variable (whatever kind it is) -}
type TyVarRep = String
{- String of whatever type -}
type TypeRep = String
{- String of a kind variable -}
type KindVarRep = String
{- String of a concrete (not a variable) kind -}
type ConcKindRep = String
{- String of whatever kind -}
type KindRep = String
{- String of a concrete property -}
type PropRep = String
{- String of a data-constructor -}
type DataConRep = String

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

{- An alternative to Show type-class. It should also have a different semantics from `Show`: a type implements
`AtomStr` if it has a way to provide the "most minimal" string representation of its values. -}
class AtomStr t where
    strOf :: t -> String

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
