{- This module tries to gather a generic API on tables. -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib.Table
    ( Emptiness(..)
    , ResAdder(..)
    , Adder(..)
    , ResKeyValueAdder(..)
    , KeyValueAdder(..)
    , ResKeyRemover(..)
    , KeyRemover(..)
    , Existence(..)
    , KeyFinding(..)
    , PredExistence(..)
    , PredFinding(..)
    , PredKeyFinding(..)
    , SafeKeyFinding(..)
    , AllGetter(..)
    , ValUpdate(..)
    , KeyValUpdate(..)
    , KeyValUpdate'(..)
    , KeyKeyUpdate(..)
    , ListSource(..)
    , GenericKeyTable
) where

class Emptiness a where
    noElems :: a

class ResAdder a e where
    tryAddElem :: e -> a -> Maybe a

class Adder a e where
    addElem :: e -> a -> a

class ResKeyValueAdder a k e where
    kTryAddElem :: k -> e -> a -> Maybe a

class KeyValueAdder a k e where
    kAddElem :: k -> e -> a -> a

class ResKeyRemover a k e where
    kTryRemoveElem :: k -> a -> (Maybe e, a)

{- A thing to note is that it's not necessary to specify the elements-type, while in `ResKeyRemover` it does. -}
class KeyRemover a k where
    kRemoveElem :: k -> a -> a

{- The existence test. It does not imply the satisfaction of any finding test. -}
class Existence a e where
    existIn :: e -> a -> Bool

{- The finding test through a key; if an object can satisfy both the key-finding test and the existence test,
and the element-type in Existence instance is the same of the key-type in KeyFinding instance, then the
following property should be always true:

  kFind key m /= Nothing = existIn key m

`KeyFinding` has also parameter `e` of elements in order to fix the type of `e` when instantiating a
`KeyFinding`, otherwise the signature of `getElem` would be: "forall e. k -> a -> Maybe e", but an instance
of `KeyFinding` should have an already fixed elements type. -}
class KeyFinding a k e where
    kFind :: k -> a -> Maybe e

{- The existence test through a predicate. -}
class PredExistence a e where
    isSatisfiedIn :: (e -> Bool) -> a -> Bool

{- The finding test through a predicate. -}
class PredFinding a e where
    pFind :: (e -> Bool) -> a -> Maybe e

{- The finding test through a predicate on a key. -}
class PredKeyFinding a k e where
    pkFind :: (k -> Bool) -> a -> Maybe e

{- The safe finding test through a key. The use of this should be very rare. -}
class SafeKeyFinding a k e where
    kSafeFind :: k -> a -> e

{- A way to "one-shot-fetch" all elements of an object. -}
class AllGetter a e where
    getAllElems :: a -> [e]

{- Value update. -}
class ValUpdate a e where
    valUpdate :: (e -> e) -> a -> a

{- Value update. The value is searched through a key. -}
class KeyValUpdate a k e where
    kValUpdate :: k -> (e -> e) -> a -> a

class KeyValUpdate' a k e where
    kValUpdate' :: k -> e -> a -> a

{- A way to update a key; The use of this should be very rare. -}
class KeyKeyUpdate a k where
    kKeyUpdate :: k -> (k -> k) -> a -> a

class ListSource a e where
    toList' :: a -> [e]
    fromList' :: [e] -> a

{- Constraint shorthand. -}
class
    ( Emptiness t
    , ResKeyValueAdder t key elem
    , ResKeyRemover t key elem
    , Existence t key
    , KeyFinding t key elem
    ) => GenericKeyTable t key elem
