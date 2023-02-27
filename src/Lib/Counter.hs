module Lib.Counter
    ( Counter(..)
    , CounterObj
    , AlphabeticCounterObj
    , newNumeric
    , newAlphabeticNumber
    , nextNumeric
    , nextCharAndNumber
) where

import Data.Char

class Counter c where
    new :: c
    cur :: c -> String
    next :: c -> (String, c)

{- Just a numeric infinite counter of natural numbers:
    0 -> 1 -> 2 -> 3 -> 4 -> ...
-}
newtype CounterObj = CounterObj Integer

{- The counter should work like this:
     a -> b -> c -> ... -> y -> z -> a1 -> b1 -> ... -> z1 -> a2 -> ...
-}
data AlphabeticCounterObj =
    ABCounterObj
        { abCurrent :: String
        , abCurrentChar :: Char
        , abCurrentNum :: Integer
        }

data CountRes a =
    {- Just a value. -}
      The a
    {- A value plus the information of "plus one". -}
    | Carry a

beforeLowerA :: Char
beforeLowerA = chr (ord 'a' - 1)

newAlphabeticNumber :: AlphabeticCounterObj
newAlphabeticNumber =
    ABCounterObj
        { abCurrent = ""
        , abCurrentChar = beforeLowerA
        , abCurrentNum = 0
        }

newNumeric :: CounterObj
newNumeric = CounterObj 0

{- It calculates the next char of a given one.
NB: it does not check the character is in the range 'a'-'z'. -}
nextAlphabeticChar :: Char -> CountRes Char
nextAlphabeticChar 'z' = Carry 'a'
nextAlphabeticChar c = The . chr $ ord c + 1

nextABWithChar :: c -> Char -> (c -> Integer) -> (String, Char, Integer)
nextABWithChar c char curNum =
    case nextAlphabeticChar char of
        The char' ->
            let n = curNum c in
                ( char' : show n
                , char'
                , n
                )
        Carry char' ->
            let nextOne = curNum c + 1 in
                ( char' : show nextOne
                , char'
                , nextOne
                )

nextAB :: c -> (c -> Char) -> (c -> Integer) -> (String, Char, Integer)
nextAB c curChar = nextABWithChar c $ curChar c

nextAlphabeticNumber :: AlphabeticCounterObj -> (String, AlphabeticCounterObj)
nextAlphabeticNumber c =
    let (res, char, n) = nextAB c abCurrentChar abCurrentNum in
        ( res
        , c
            { abCurrent = res
            , abCurrentChar = char
            , abCurrentNum = n
            }
        )

{- It calculates the next integer of the one contained in the CounterObj value. -}
nextNumeric :: CounterObj -> (Integer, CounterObj)
nextNumeric (CounterObj n) =
    let nextOne = n + 1 in
        (nextOne, CounterObj nextOne)

instance Counter CounterObj where
    {- NB: the default implementation is a new numeric counter. -}
    new = newNumeric
    cur (CounterObj n) = show n
    next c =
        case nextNumeric c of
            (n, c') -> (show n, c')

instance Counter AlphabeticCounterObj where
    new = newAlphabeticNumber
    cur = abCurrent
    next = nextAlphabeticNumber

{- `next` utility returns directly a String value, but having both the Char and the Integer values which builds up the
String value can be useful. -}
nextCharAndNumber :: AlphabeticCounterObj -> (Char, Integer, AlphabeticCounterObj)
nextCharAndNumber c =
    let (res, char, n) = nextAB c abCurrentChar abCurrentNum in
        ( char
        , n
        , c
            { abCurrent = res
            , abCurrentChar = char
            , abCurrentNum = n
            }
        )
