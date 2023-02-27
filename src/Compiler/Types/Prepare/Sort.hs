module Compiler.Types.Prepare.Sort
    ( sort
) where

import Compiler.State as With
import Compiler.Types.Tables(PropMethodsTable)
import Compiler.Types.Prepare.Lib

type SortedComponent = SortedBindings
type RemainingBindingsNo = Int

tryInsert :: RawBinding -> [RawBinding] -> [String] -> PropMethodsTable With.ProgState -> Maybe SortedBindings
tryInsert b [] _ _ = Just [b]
tryInsert b bs @ (b1 : t) deps mhts =
    let b1Reps = symRepsOf b1 in
    let b1Deps = depsOfBinding b1 mhts in
    let reps = symRepsOf b in
        if any (`elem` deps) b1Reps
        then
            case tryInsert b t deps mhts of
                Nothing -> Nothing
                Just bs' -> Just (b1 : bs')
        else if any (`elem` b1Deps) reps
        then Just (b : bs)
        else Nothing

{- The algorithm is a slightly more complex version of insertion sort. First of all, it must hold that the starting
bindings have NOT strongly connected components, namely there is no mutual recursion among them. Then, the algorithm
works like this:
    - there is a graph (all the bindings) which can be seen as a set of subgraphs of (not strongly) connected components;
    - each component is sorted with the insertion sort;
    - each component has no precedence on the other components, because there is not dependency among them, so they can
      safely concatenated.
-}
sort'
    {- Bindings to insert in the current sorted component. -}
    :: [RawBinding]
    {- Remaining bindings to insert. -}
    -> [RawBinding]
    {- Current sorted component. -}
    -> SortedComponent
    {- Accumulated sorted components (final result to concatenate). -}
    -> [SortedComponent]
    {- Number of the previous remaining bindings. -}
    -> RemainingBindingsNo
    {- For dependency detection. -}
    -> PropMethodsTable With.ProgState
    -> SortedBindings
{- No more bindings, no more remaining bindings, then the algorithm stops. -}
sort' [] [] cur res _ _ = concat $ cur : res
{- No bindings to visit, then the remaining ones are checked:
    if they are the same as the previous iteration, then the current component is sorted and has no more nodes to add
    into it, so it is added to resulting components, else other nodes have to be added to the actual component. -}
sort' [] rest cur res n mhts =
    let restNo = length rest in
        if n == restNo
        then sort' (reverse rest) [] [] (cur : res) restNo mhts
        else sort' (reverse rest) [] cur res restNo mhts
{- Performing lazy insertion sort: it is possible that a node is not actually "sortable", because both its dependencies
and the nodes which it is a dependency for are not yet in the component. -}
sort' (b : t) rest cur res n mhts =
    let deps = depsOfBinding b mhts in
        case tryInsert b cur deps mhts of
            {- In this case, the information on the current component is not enough precise to insert node `b`, so the
            latter is put in the actual remaining bindings. -}
            Nothing -> sort' t (b : rest) cur res n mhts
            Just cur' -> sort' t rest cur' res (n - 1) mhts

sort :: [RawBinding] -> PropMethodsTable With.ProgState -> SortedBindings
{- Remaining bindings number are set to -1, because at the first iteration, at least one element is inserted in the
current sorted component (which is actually empty), so it is not important the number of remaining bindings. -}
sort bs = sort' bs [] [] [] (-1)
