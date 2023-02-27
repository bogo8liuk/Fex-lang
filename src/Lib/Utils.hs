module Lib.Utils
    ( ErrDescription
    , KnowledgeOf(..)
    , isYes
    , isNo
    , isDK
    , KnowledgeOneOf(..)
    , isThis
    , isThat
    , isNone
    , (|>)
    , (<|)
    , insertAtAfter
    , insertAtList
    , insertAt
    , diffList
    , cDiffList
    , allConcrete
    , doOnFst
    , allEq
    , allMapEq
    , occursAtLeastNTimes
    , occursAtLeastTwice
    , removeDup
    , removeDup'
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
    , allMatchEq
    , maybemap
    , leftmapFst
    , rightmapFst
    , thatmapFst
    , thismapFst
    , newNEFst
    , newNELast
    , maximumBy'
    , minimumBy'
    , if'
    , then'
    , else'
    , theMost
    , greatest
    , lowest
    , intOfStr
    , forIterNo
    , forIterNo'
    , do'
    , while
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

type ErrDescription = String

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
(<|) f x = f x

insertAtAfter :: a -> Integer -> (a -> b) -> [b] -> Maybe [b]
insertAtAfter elem i turn l = if i < 0 then Nothing else Just (__insertAtAfter elem i turn l)

__insertAtAfter :: a -> Integer -> (a -> b) -> [b] -> [b]
__insertAtAfter elem _ turn [] = [turn elem]
__insertAtAfter elem 0 turn l = (turn elem) : l
__insertAtAfter elem i turn (h : t) = h : (__insertAtAfter elem (i - 1) turn t)

insertAtList :: a -> Integer -> [[a]] -> Maybe [[a]]
insertAtList elem i ll = if i < 0 then Nothing else Just (__insertAtList elem i ll)

__insertAtList :: a -> Integer -> [[a]] -> [[a]]
__insertAtList elem _ [] = [[elem]]
__insertAtList elem 0 ((h : t) : tl) = (elem : h : t) : tl
__insertAtList elem i (hl : tl) = hl : (__insertAtList elem (i - 1) tl)

insertAt :: a -> Integer -> [a] -> Maybe [a]
insertAt elem i l = insertAtAfter elem i id l

{- From the second list, it removes (from the head) the number of elements of the first list. -}
diffList :: [a] -> [b] -> [b]
diffList l l' = drop (length l) l'

{- Same as diffList but it removes starting from the tail. -}
cDiffList :: [a] -> [b] -> [b]
cDiffList l l' = reverse . drop (length l) . reverse $ l'

{- Given a list of Maybe, it returns a Maybe of list of elements which were contained in the previous
list: Nothing if at least one was Nothing, Just a list otherwise (namely all elements were Just something). -}
allConcrete :: [Maybe a] -> Maybe [a]
allConcrete [] = Just []
allConcrete (Nothing : _) = Nothing
allConcrete (Just x : t) = case allConcrete t of
    Nothing -> Nothing
    Just xs -> Just $ x : xs

doOnFst :: (a -> b) -> (a, c) -> (b, c)
doOnFst f (x, y) = (f x, y)

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (elem : t) = all (== elem) t

{- Same of allEq, but applicating first a mapping of elements. -}
allMapEq :: Eq b => (a -> b) -> [a] -> Bool
allMapEq _ [] = True
allMapEq f (elem : t) = all ((== f elem) . f) t

occursAtLeastNTimes :: Eq a => a -> [a] -> Int -> Bool
occursAtLeastNTimes x l n = occurs l n
    where
        occurs [] _ = False
        occurs (x' : t) n =
            if x /= x'
            then occurs t n
            else let n' = n - 1 in
                n' == 0 || occurs t n'

occursAtLeastTwice :: Eq a => a -> [a] -> Bool
occursAtLeastTwice x l = occursAtLeastNTimes x l 2

{- It removes all duplicates in a list, but not preserving the order. If you need to preserve the order,
see `removeDup'`. -}
removeDup :: Eq a => [a] -> [a]
removeDup [] = []
removeDup (elem : t) =
    if any (== elem) t
    then removeDup t
    else elem : removeDup t

{- Like removeDup, but it does preserve the order and less efficient. -}
removeDup' :: Eq a => [a] -> [a]
removeDup' l = __remove l []
    where
        __remove [] _ = []
        __remove (elem : t) trm =
            if any (== elem) trm
            then __remove t trm
            else if any (== elem) t
            then elem : __remove t (elem : trm)
            else elem : __remove t trm

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
elemAt 0 (e : t) = Just e
elemAt n (_ : t) = elemAt (n - 1) t

{- Indexed version of map. -}
imap :: (Int -> a -> b) -> [a] -> [b]
imap f l = __imap f l 0
    where
        __imap _ [] _ = []
        __imap f (e : t) i = f i e : (__imap f t $ i + 1)

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
allMatchEq :: Eq a => [a] -> [a] -> Bool
allMatchEq l l' = all (\e -> any (\e' -> e == e') l') l

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

theMost :: (a -> a -> Bool) -> NonEmpty a -> a
theMost _ (e :| []) = e
theMost pred (e :| e' : t)
    | pred e' e = theMost pred (e' :| t)
    | otherwise = theMost pred (e :| t)

greatest :: Ord a => NonEmpty a -> a
greatest = theMost (>)

lowest :: Ord a => NonEmpty a -> a
lowest = theMost (<)

intOfStr :: String -> Maybe Integer
intOfStr = readMaybe :: String -> Maybe Integer

forIterNo :: Int -> a -> [a]
forIterNo n op
    | n <= 0 = []
    | otherwise = op : forIterNo (n - 1) op

forIterNo' :: Int -> a -> (a -> a) -> a
forIterNo' n val op
    | n <= 0 = val
    | otherwise = forIterNo' (n - 1) (op val) op

do' :: (a -> a) -> a -> a
do' = ($)

{- Usage example:

    while isNothing x
        `do'` op
-}
while :: (a -> Bool) -> a -> (a -> a) -> a
while cond val doOn =
    if cond val
    then while cond (doOn val) doOn
    else val

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
