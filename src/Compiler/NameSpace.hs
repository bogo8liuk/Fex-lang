{- Just a little library to create the abstraction of namespaces inside a piece of code. -}

module Compiler.NameSpace
    ( Scope
    , globalScope
    , firstLocalScope
    , secondLocalScope
    , thirdLocalScope
    , incrScope
    , decrScope
    , cmpScopes
    , ltScopes
    , leScopes
    , eqScopes
    , gtScopes
    , geScopes
    , neqScopes
    , cmpTuple2
    , cmpTuple2Shift1
    , cmpTuple3
    , cmpTuple3Shift1
    , cmpTuple3Shift2
    , cmpTuple4
    , cmpTuple4Shift1
    , cmpTuple4Shift2
    , cmpTuple4Shift3
) where

{- NB: this is the type of scope, it's not an abstract data type, but it's recommended to use scope api. For example,
if you want to define a new scope, use one of globalScope, firstLocalScope, etc.; normally, they should be sufficient,
but if you (the client) have to go deeper with the scopes, there are also incScope and decScope which help you to
increment and decrement the scope arbitrarily. -}
type Scope = Integer

globalScope, firstLocalScope, secondLocalScope, thirdLocalScope :: Scope
globalScope = 0
firstLocalScope = 1
secondLocalScope = 2
thirdLocalScope = 3

incrScope, decrScope :: Scope -> Scope
incrScope = (+ 1)
decrScope sc = sc - 1

cmpScopes :: Scope -> Scope -> Ordering
cmpScopes = compare

ltScopes, leScopes, eqScopes, gtScopes, geScopes, neqScopes :: Scope -> Scope -> Bool
ltScopes = (<)
leScopes = (<=)
eqScopes = (==)
gtScopes = (>)
geScopes = (>=)
neqScopes = (/=)

cmpTuple2 :: (Scope, a) -> (Scope, a) -> Ordering
cmpTuple2 (sc, _) (sc', _) = compare sc sc'

cmpTuple2Shift1 :: (a, Scope) -> (a, Scope) -> Ordering
cmpTuple2Shift1 (_, sc) (_, sc') = compare sc sc'

cmpTuple3 :: (Scope, a, b) -> (Scope, a, b) -> Ordering
cmpTuple3 (sc, _, _) (sc', _, _) = compare sc sc'

cmpTuple3Shift1 :: (a, Scope, b) -> (a, Scope, b) -> Ordering
cmpTuple3Shift1 (_, sc, _) (_, sc', _) = compare sc sc'

cmpTuple3Shift2 :: (a, b, Scope) -> (a, b, Scope) -> Ordering
cmpTuple3Shift2 (_, _, sc) (_, _, sc') = compare sc sc'

cmpTuple4 :: (Scope, a, b, c) -> (Scope, a, b, c) -> Ordering
cmpTuple4 (sc, _, _, _) (sc', _, _, _) = compare sc sc'

cmpTuple4Shift1 :: (a, Scope, b, c) -> (a, Scope, b, c) -> Ordering
cmpTuple4Shift1 (_, sc, _, _) (_, sc', _, _) = compare sc sc'

cmpTuple4Shift2 :: (a, b, Scope, c) -> (a, b, Scope, c) -> Ordering
cmpTuple4Shift2 (_, _, sc, _) (_, _, sc', _) = compare sc sc'

cmpTuple4Shift3 :: (a, b, c, Scope) -> (a, b, c, Scope) -> Ordering
cmpTuple4Shift3 (_, _, _, sc) (_, _, _, sc') = compare sc sc' 
