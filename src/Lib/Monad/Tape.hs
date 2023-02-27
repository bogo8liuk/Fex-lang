module Lib.Monad.Tape
    ( Tape
    , runTape
    , runTape'
    , testTape
    , testTape'
    , mvon
    , mvback
    , stay
    , reset
) where

import Lib.Utils
import Data.Array
import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Lib.Monad.Repeat

data TapeState a = TapeSt { tape :: Array Int a
                          , headOn :: Int
                          }

build :: [a] -> Int -> TapeState a
build elems start = let max = length elems - 1 in
    TapeSt { tape = listArray (0, max) elems
           , headOn = start
           }

data TapeResult a =
      OutOfBoundTape String     --TODO: changing name to the con??? It can be whatever kind of error, not just the tape out of bounds...
    | Current a
    | Mvback a
    | Mvon a
    | Reset a

cmnErrMsg :: String
cmnErrMsg = "Tape error: tape is gone out of its bounds"

{- A Tape represents an action on a tape (namely an ordered sequence of elements with the notion
of "current element") with type `e` of elements and return type `a`. -}
newtype Tape e a = Tape { runOnTape :: TapeState e -> TapeResult a }

extract :: a -> TapeState e -> Tape e (a -> b) -> TapeResult b
extract m t (Tape ff) = case ff t of
    Current g -> Current (g m)
    Mvback g -> Mvback (g m)
    Mvon g -> Mvon (g m)
    Reset g -> Reset (g m)
    OutOfBoundTape msg -> OutOfBoundTape msg

instance Functor (Tape e) where
    {- fmap does not take care of the result because it is just a "transformations" of types, there's no
    notion of dynamic computation (as in Monad bind) -}
    fmap f (Tape g) = Tape $ \t -> case g t of
        Current m -> Current . f $ m
        Mvback m -> Mvback . f $ m
        Mvon m -> Mvon . f $ m
        Reset m -> Reset . f $ m
        OutOfBoundTape msg -> OutOfBoundTape msg

instance Applicative (Tape e) where
    pure elem = Tape $ \_ -> Current elem    --An action that always succeeds

    {- As in Functor fmap, there's no notion of dynamic computation -}
    (<*>) ft (Tape g) = Tape $ \t -> case g t of
        Current m -> extract m t ft
        Mvback m -> extract m t ft
        Mvon m -> extract m t ft
        Reset m -> extract m t ft
        OutOfBoundTape msg -> OutOfBoundTape msg

instance Monad (Tape e) where
    (>>=) action f = Tape $ \(TapeSt { tape = t, headOn = h }) ->
        let res = runOnTape action (TapeSt { tape = t, headOn = h }) in
            case res of
                Current m -> runOnTape (f m) (TapeSt { tape = t, headOn = h })
                Mvback m -> runOnTape (f m) (TapeSt { tape = t, headOn = h - 1 })
                Mvon m -> runOnTape (f m) (TapeSt { tape = t, headOn = h + 1 })
                Reset m -> runOnTape (f m) (TapeSt { tape = t, headOn = 0 })
                OutOfBoundTape msg -> OutOfBoundTape msg

instance Alternative (Tape e) where
    empty = Tape $ \_ -> OutOfBoundTape cmnErrMsg    --An action that always fails

    (<|>) (Tape f) (Tape g) = Tape $ \t -> case f t of
        Current _ -> g t     --TODO: discarding result???
        Mvback _ -> g t
        Mvon _ -> g t
        Reset _ -> g t
        OutOfBoundTape msg -> OutOfBoundTape msg

instance MonadPlus (Tape e)

instance MonadFail (Tape e) where
    fail msg = Tape $ \_ -> OutOfBoundTape msg      --This is useful for a custom error message

instance MonadRepeat (Tape e)

isIn :: Int -> (Int, Int) -> Bool
isIn i (min, max) = i >= min && i <= max

actionOn :: Array Int e -> Int -> a -> (b -> TapeResult b) -> (a -> e -> b) -> TapeResult b
actionOn t h x res f =
    if h `isIn` bounds t
    then (t ! h) |> f x |> res
    else OutOfBoundTape cmnErrMsg

__runTape :: [e] -> Tape e a -> Int -> Maybe a
__runTape elems (Tape f) start = case build elems start |> f of
    OutOfBoundTape _ -> Nothing
    Current m -> Just m
    Mvback m -> Just m
    Mvon m -> Just m
    Reset m -> Just m

__testTape :: Show a => [e] -> Tape e a -> Int -> IO ()
__testTape elems (Tape f) start = case build elems start |> f of
    OutOfBoundTape msg -> print msg
    Current m -> print m
    Mvback m -> print m
    Mvon m -> print m
    Reset m -> print m

{- `runTape es a` runs the action `a` building a tape from list `es`, returning a value if the action
succeeds, Nothing otherwise. It is guaranteed that the tape is exactly "long" (has the same number of
elements) as the list. It initially sets the current element one position before the first element,
if this is undesirable, look at runTape'. -}
runTape :: [e] -> Tape e a -> Maybe a
runTape elems t = __runTape elems t (-1)

{- Same as runTape, with the exception that runTape' initially sets the current element onto the first
element. -}
runTape' :: [e] -> Tape e a -> Maybe a
runTape' elems t = __runTape elems t 0

{- testTape is the same as runTape, except for the return type: it prints out the result of the action.
It initially sets the current element one position before the first element, if this is undesirable,
look at testTape'. -}
testTape :: Show a => [e] -> Tape e a -> IO ()
testTape elems t = __testTape elems t (-1)

{- Same as testTape, with the exception that testTape' initially sets the current element onto the first
element. -}
testTape' :: Show a => [e] -> Tape e a -> IO ()
testTape' elems t = __testTape elems t 0

{- It performs an action, moving the current element one position forward. -}
mvon :: a -> (a -> e -> b) -> Tape e b
mvon x f =
    Tape $ \(TapeSt { tape = t, headOn = h }) -> actionOn t (h + 1) x Mvon f

{- It performs an action, moving the current element one position back. -}
mvback :: a -> (a -> e -> b) -> Tape e b
mvback x f =
    Tape $ \(TapeSt { tape = t, headOn = h }) -> actionOn t (h - 1) x Mvback f

{- It performs an action, without moving the current element. -}
stay :: a -> (a -> e -> b) -> Tape e b
stay x f =
    Tape $ \(TapeSt { tape = t, headOn = h }) -> actionOn t h x Current f

{- It performs an action, resetting the the tape, so making the current element the first element. -}
reset :: a -> (a -> e -> b) -> Tape e b
reset x f =
    Tape $ \(TapeSt { tape = t, headOn = h }) -> actionOn t 0 x Reset f
