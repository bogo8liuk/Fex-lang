module Compiler.Syntax.NonTranzitInsertion
    ( algorithm
) where

{- The Non-tranzit-insertion-algorithm is called like this because it inserts
an item in a list of lists of items, it exploits Ord type-class and it
also checks for the presence of "ambiguities", i.e. an elem which is instance of
Ord, but does not follow the transitivity law (or even other laws). This can lead to
problems if an insertion is based on the Ord type-class and its laws (ambiguities).
It assumes the hypothesis that all the other elements have been inserted with
this algorithm is true. Given an item x, it works in the following way:

-if the list is empty, then a singleton sub-list is inserted
    [] => [[x]]

-if there's a sub-list where there's at least an item y which respects the following statement:
    x `compare` y == LT
and immediately after there's a sub-list where there's at least an item y which respects the following statement:
    x `compare` y == GT
then a singleton sub-list is inserted between the two sub-lists
    [..., [..., LT, ...], [..., GT, ...], ...] => [..., [..., LT, ...], [x], [..., GT, ...], ...]
it works even if only one of the sub-lists does not exist

-if there's a sub-list where all the elements y which respects the following statements:
    x `compare` y == EQ
and this list is between two sub-lists which respects the previous statement (LT-GT), then x is inserted
in the first list
    [..., [..., LT, ...], [EQ, EQ, EQ] [..., GT, ...], ...] => [..., [..., LT, ...], [x, EQ, EQ, EQ] [..., GT, ...], ...]
it works even if only one of the external sub-lists does not exist or even if there's more than one
central sub-list (in this case, x is inserted in the first central sub-list)
 -}

algorithm :: Ord a => [[a]] -> a -> Maybe [[a]]
algorithm [] x = Just [[x]]
algorithm ll @ (l : lt) x
    | any (< x) l =
        if areAmbigous x ll
        then Nothing
        else Just ([x] : l : lt)
    | all (== x) l =
        if areAmbigous x lt
        then Nothing
        else Just ((x : l) : lt)
    | otherwise =
        case algorithm lt x of
            Nothing -> Nothing
            Just lt' -> Just (l : lt')

areAmbigous :: Ord a => a -> [[a]] -> Bool
areAmbigous _ [] = False
areAmbigous x (l : lt) = any (> x) l || areAmbigous x lt
