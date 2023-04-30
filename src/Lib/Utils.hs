module Lib.Utils
    ( (|>)
    , (<|)
    , if'
    , then'
    , else'
    , intOfStr
    , KnowledgeOf(..)
    , isYes
    , isNo
    , isDK
    , KnowledgeOneOf(..)
    , isThis
    , isThat
    , isNone
    , onFst
    , onSnd
    , insertAt
    , diffList
    , diffListTail
    , getIfAll
    , allEq
    , occursAtLeastNTimes
    , head'
    , tail'
    , tail''
    , last'
    , heads
    , headsAndLast
    , elemAt
    , imap
    , lastmap
    , fltmap
    , firstThat
    , lastThat
    , foldne
    , isSublist
    , maybemap
    , leftmapFst
    , rightmapFst
    , thatmapFst
    , thismapFst
    , newNEFst
    , newNELast
    , maximumBy'
    , minimumBy'
    , theMost
    , greatest
    , lowest
    , theMost'
    , greatest'
    , lowest'
    , forAll
    , takeWhile'
    , indexing
) where

import Data.List(maximumBy, minimumBy, foldl')
import Data.List.NonEmpty(NonEmpty(..))
import Data.Maybe(isNothing)
import Data.Either(isRight, isLeft)
import Data.Foldable(toList)
import Text.Read

infixl 1 |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

{- This the same of ($) operator, but it is left-associative instead of being right-associative like ($).
Let's make an example to understand its usefullness, we have:
    f x y z = x + y + z
    ...
    f <| 4 + 5 <| 2 + 3 - 5 <| 1
This is useful to nest intermediate operations to pass to a function without parenthesis. -}
infixl 1 <|
(<|) :: (a -> b) -> a -> b
(<|) f = f

{- The if-then-else construct without the if-then-else construct! Usage example:

if' (x > y)
`then'` "Hello World!"
`else'` "World! Hello"

The only limitation is that the if-condition has to be inside parenthesis.
-}
if' :: Bool -> Bool
if' = id

then' :: Bool -> a -> (a -> a)
then' True x = const x
then' False _ = id

else' :: (a -> a) -> a -> a
else' = ($)

intOfStr :: String -> Maybe Integer
intOfStr = readMaybe :: String -> Maybe Integer

----------------------- New data-types -----------------------

{- Like Maybe, but it offers one more case with the same type. -}
data KnowledgeOf a = Yes a | No a | Don'tKnow deriving (Show, Eq, Ord)

instance Functor KnowledgeOf where
    fmap f (Yes x) = Yes $ f x
    fmap f (No x) = No $ f x
    fmap _ Don'tKnow = Don'tKnow

isYes :: KnowledgeOf a -> Bool
isYes (Yes _) = True
isYes _ = False

isNo :: KnowledgeOf a -> Bool
isNo (No _) = True
isNo _ = False

isDK :: KnowledgeOf a -> Bool
isDK Don'tKnow = True
isDK _ = False

{- Like Maybe Either, but using a single type. -}
data KnowledgeOneOf a b = This a | That b | None deriving (Show, Eq, Ord)

isThis :: KnowledgeOneOf a b -> Bool
isThis (This _) = True
isThis _ = False

isThat :: KnowledgeOneOf a b -> Bool
isThat (That _) = True
isThat _ = False

isNone :: KnowledgeOneOf a b -> Bool
isNone None = True
isNone _ = False

----------------------- Operations on tuples -----------------------

onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (x, y) = (f x, y)

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)

----------------------- Operations on lists and foldables -----------------------

insertAt :: a -> Int -> [a] -> [a]
insertAt x _ [] = [x]
insertAt x n l @ (h : t)
    | n <= 0 = x : l
    | otherwise = h : insertAt x (n - 1) t

{- From the second list, it removes (from the head) the number of elements of the first list. -}
diffList :: [a] -> [b] -> [b]
diffList l = drop $ length l

{- Same as diffList but it removes starting from the tail. -}
diffListTail :: [a] -> [b] -> [b]
diffListTail l = reverse . drop (length l) . reverse

{- Given a Foldable of Maybe, it returns a Maybe of Foldable of elements which were contained in the previous
Foldable: Nothing if at least one was Nothing, Just a Foldable otherwise (namely all elements were Just something). -}
getIfAll :: (Foldable t, Functor t) => t (Maybe a) -> Maybe (t a)
getIfAll = maybemap id

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (h : t) = all (== h) t

occursAtLeastNTimes :: Eq a => a -> [a] -> Int -> Bool
occursAtLeastNTimes _ [] n
    | n <= 0 = True
    | otherwise = False
occursAtLeastNTimes x (h : t) n
    | n <= 0 = True
    | x == h = occursAtLeastNTimes x t $ n - 1
    | otherwise = occursAtLeastNTimes x t n

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

tail' :: [a] -> Maybe [a]
tail' [] = Nothing
tail' (_ : t) = Just t

{- Like tail, but if the list is empty, it returns the empty list. -}
tail'' :: [a] -> [a]
tail'' [] = []
tail'' (_ : t) = t

last' :: [a] -> Maybe a
last' l = head' $ reverse l

heads :: [a] -> Maybe [a]
heads [] = Nothing
heads l = Just $ heads' l
    where
        {- Using this function in order not to unwrap and wrap results. -}
        heads' [] = []    --This case should never be evaluated
        heads' [_] = []
        heads' (e : t) = e : heads' t

headsAndLast :: [a] -> Maybe ([a], a)
headsAndLast l = headsAndLast' l []
    where
        headsAndLast' [] _ = Nothing
        headsAndLast' [e] accum = Just (reverse accum, e)
        headsAndLast' (e : t) accum = headsAndLast' t $ e : accum

elemAt :: Int -> [a] -> Maybe a
elemAt _ [] = Nothing
elemAt 0 (h : _) = Just h
elemAt n (_ : t) = elemAt (n - 1) t

{- Indexed version of map. -}
imap :: (Int -> a -> b) -> [a] -> [b]
imap f l = map <| uncurry f <| zip [0..] l

{- The same of map, but with a dedicated callback for the last element of the list. -}
lastmap :: (a -> b) -> (a -> b) -> [a] -> [b]
lastmap _ _ [] = []
lastmap _ flast [e] = [flast e]
lastmap f flast (e : t) = f e : lastmap f flast t

{- A filtered version of map: only just some elements get mapped. -}
fltmap :: (a -> Maybe b) -> [a] -> [b]
fltmap _ [] = []
fltmap f (e : t) =
    case f e of
        Nothing -> fltmap f t
        Just e' -> e' : fltmap f t

{- Same of `any`, but it returns the first element which satisfies the predicate. -}
firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat f l = head' $ dropWhile (not . f) l

lastThat :: (a -> Bool) -> [a] -> Maybe a
lastThat f l = firstThat f $ reverse l

{- Total version of foldl1'. -}
foldne :: (a -> a -> a) -> [a] -> Maybe a
foldne _ [] = Nothing
foldne f l = Just $ foldl1 f l

{- It tests that all elements of the first list have at least one of the second list with which the equality
test returns true. -}
isSublist :: Eq a => [a] -> [a] -> Bool
isSublist l l' = all (`elem` l') l

maybemap :: (Foldable t, Functor t) => (a -> Maybe b) -> t a -> Maybe (t b)
maybemap f x =
    let y = fmap f x in
        if any isNothing y
        then Nothing
        else Just $ fmap (\(Just e) -> e) y

leftmapFst :: (Foldable t, Functor t) => (a -> Either b err) -> t a -> Either (t b) err
leftmapFst f x =
    let y = fmap f x in
        case firstThat isRight $ toList y of
            Just (Right err) -> Right err
            _ -> Left $ fmap (\(Left e) -> e) y

rightmapFst :: (Foldable t, Functor t) => (a -> Either err b) -> t a -> Either err (t b)
rightmapFst f x =
    let y = fmap f x in
        case firstThat isLeft $ toList y of
            Just (Left err) -> Left err
            _ -> Right $ fmap (\(Right e) -> e) y

thatmapFst :: (Foldable t, Functor t) => (a -> KnowledgeOneOf err b) -> t a -> KnowledgeOneOf err (t b)
thatmapFst f x =
    let y = fmap f x in
        case firstThat (not . isThat) $ toList y of
            Just None -> None
            Just (This err) -> This err
            _ -> That $ fmap (\(That e) -> e) y

thismapFst :: (Foldable t, Functor t) => (a -> KnowledgeOneOf b err) -> t a -> KnowledgeOneOf (t b) err
thismapFst f x =
    let y = fmap f x in
        case firstThat (not . isThis) $ toList y of
            Just None -> None
            Just (That err) -> That err
            _ -> This $ fmap (\(This e) -> e) y

newNEFst :: a -> [a] -> NonEmpty a
newNEFst = (:|)

newNELast :: [a] -> a -> NonEmpty a
newNELast [] e' = e' :| []
newNELast (e : t) e' = e :| t ++ [e']

{- Total version of maximumBy. -}
maximumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
maximumBy' ord x
    | null x = Nothing
    | otherwise = Just $ maximumBy ord x

{- Total version of minimumBy. -}
minimumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
minimumBy' ord x
    | null x = Nothing
    | otherwise = Just $ minimumBy ord x

theMost :: (a -> a -> Bool) -> [a] -> Maybe a
theMost _ [] = Nothing
theMost p (h : t) = Just $ theMost' p (h :| t)

greatest :: Ord a => [a] -> Maybe a
greatest = theMost (>)

lowest :: Ord a => [a] -> Maybe a
lowest = theMost (<)

theMost' :: (a -> a -> Bool) -> NonEmpty a -> a
theMost' p (h :| l) = foldl' keep h l
    where
        keep x y
            | y `p` x = y
            | otherwise = x

greatest' :: Ord a => NonEmpty a -> a
greatest' = theMost' (>)

lowest' :: Ord a => NonEmpty a -> a
lowest' = theMost' (<)

{- A more fancy version of foldl':

    forAll ["hello", "world", "42"] deleteKeyFrom strTable
-}
forAll :: Foldable t => t a -> (b -> a -> b) -> b -> b
forAll obj f start = foldl' f start obj

{- Same of takeWhile, but it includes also the element that satisfy the predicate. -}
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x : t) =
    if f x
    then x : takeWhile' f t
    else [x]

indexing :: [a] -> [(Int, a)]
indexing = zip [0..]
