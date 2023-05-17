{- NB: the condition which tells at least one type variable must in a constraint is not checked here.
Look at Names.Check.Correct module to see it.
TODO: can Names module incorporate this module??? -}

module Compiler.Constraints.Check
    ( perform
) where

import Utils.Data.Foldable
import Lib.Result
import Data.List(foldl')
import Compiler.State as With
import Compiler.Ast.Common
import qualified Compiler.Ast.Tree as Raw

type Head = Raw.Constraint With.ProgState

data ConstraintErr =
      NotSmaller Head (Raw.Constraint With.ProgState)
    | MoreOcc (Raw.ParamTypeName With.ProgState) Head (Raw.Constraint With.ProgState)

notSmaller :: Head -> Raw.Constraint With.ProgState -> String
notSmaller head c = "Instance constraint " ++ Raw.showCont c ++ ", at " ++ show (stateOf c) ++ ", is not smaller than \
    \the head of the instance " ++ Raw.showCont head ++ ", at " ++ show (stateOf head)

moreOccurrences
    :: Raw.ParamTypeName With.ProgState
    -> Head
    -> Raw.Constraint With.ProgState
    -> String
moreOccurrences pty head c = "In instance constraint " ++ Raw.showCont c ++ ", at " ++ show (stateOf c) ++ ", type \
    \variable " ++ strOf pty ++ " has more occurrences than inside the head of the instance " ++ Raw.showCont head ++
    ", at " ++ show (stateOf head)

instance InfoShow ConstraintErr where
    infoShow (NotSmaller head c) = notSmaller head c
    infoShow (MoreOcc pty head c) = moreOccurrences pty head c

instance DebugShow ConstraintErr where
    dbgShow (NotSmaller head c) = notSmaller head c
    dbgShow (MoreOcc pty head c) = moreOccurrences pty head c

instance UnreachableState ConstraintErr where
    isUnreachable NotSmaller {} = Nothing
    isUnreachable MoreOcc {} = Nothing

{- It does the check by comparing on how many type variables are nested in the head and the various constraints of
an instance. -}
tyConCondition :: Raw.Instance With.ProgState -> Maybe (Head, Raw.Constraint With.ProgState)
tyConCondition inst =
    let instTypes = argsOf inst in
    let head = Raw.buildHead inst in
    let conts = Raw.contsFromInst inst in
        case foldl' (doCheck instTypes) Nothing conts of
            Nothing -> Nothing
            Just c -> Just (head, c)
    where
        doCheck
            :: [Raw.UnConType With.ProgState]
            -> Maybe (Raw.Constraint With.ProgState)
            -> Raw.Constraint With.ProgState
            -> Maybe (Raw.Constraint With.ProgState)
        doCheck _ errc @ (Just _) _ = errc
        doCheck instTypes Nothing c =
            let contTypes = Raw.unConsFromCont c in
                if count 0 instTypes >= count 0 contTypes
                then Nothing
                else Just c

        count :: Int -> [Raw.UnConType With.ProgState] -> Int
        count init uts = sum $ map (countWrappings init) uts

        countWrappings :: Int -> Raw.UnConType With.ProgState -> Int
        {- The `cur` parameter can be thought as the level of nesting. -}
        countWrappings cur uty =
            Raw.doOnUnCon uty
                {- This is a "leaf" case of a type, it counts as zero, because there is no type variable. -}
                (const 0)
                (const $ 1 + cur)
                (\_ ts -> count (cur + 1) ts)
                (\_ ts -> 1 + count (cur + 1) ts)

{- It checks the type variables of the head of an instance appears a number of times which is not lesser than
the number of times the same type variables (checked one by one) appears in instances constraints. -}
occurrencesCondition
    :: Raw.Instance With.ProgState
    -> Maybe (Raw.ParamTypeName With.ProgState, Head, Raw.Constraint With.ProgState)
occurrencesCondition inst =
    let pts = Raw.paramTNamesFromInst inst in
    let head = Raw.buildHead inst in
    let instTypes = argsOf inst in
    let conts = Raw.contsFromInst inst in
        case foldl' (doCheck pts instTypes) Nothing conts of
            Nothing -> Nothing
            Just (pty, c) -> Just (pty, head, c)
    where
        {- It returns a constraint which does not satisfy the condition. If all constraints are ok, it returns
        Nothing. -}
        doCheck
            :: [Raw.ParamTypeName With.ProgState]
            -> [Raw.UnConType With.ProgState]
            -> Maybe (Raw.ParamTypeName With.ProgState, Raw.Constraint With.ProgState)
            -> Raw.Constraint With.ProgState
            -> Maybe (Raw.ParamTypeName With.ProgState, Raw.Constraint With.ProgState)
        doCheck _ _ errc @ (Just _) _ = errc
        doCheck pts instTypes Nothing c =
            let contTypes = Raw.unConsFromCont c in
                case firstThat (\pty -> countTyVars pty instTypes < countTyVars pty contTypes) pts of
                    Nothing -> Nothing
                    Just errpty -> Just (errpty, c)

        {- Just a counter of occurrences. -}
        countTyVars :: Raw.ParamTypeName With.ProgState -> [Raw.UnConType With.ProgState] -> Int
        countTyVars pty uts = foldl' (\n uty -> n + countTyVarsIn pty uty) 0 uts

        {- The implementation could be very smaller, but this is done in order to gain efficiency (a few). -}
        countTyVarsIn :: Raw.ParamTypeName With.ProgState -> Raw.UnConType With.ProgState -> Int
        countTyVarsIn pty uty =
            Raw.doOnUnCon uty
                (const 0)
                (\pty' ->
                    if repOf pty == repOf pty'
                    then 1
                    else 0
                )
                (\_ ts -> countTyVars pty ts)
                (\pty' ts ->
                    if repOf pty == repOf pty'
                    then 1 + countTyVars pty ts
                    else countTyVars pty ts
                )

checkOp :: Raw.AstOpRes With.ProgState ConstraintErr ()
checkOp = Raw.lookupInst () verifyInstConditions
    where
        verifyInstConditions :: () -> Raw.Instance With.ProgState -> Either ConstraintErr ()
        verifyInstConditions _ inst =
            case occurrencesCondition inst of
                Just (pty, head, c) -> Left $ MoreOcc pty head c
                Nothing ->
                    case tyConCondition inst of
                        Nothing -> Right ()
                        Just (head, c) -> Left $ NotSmaller head c

perform :: Raw.Program With.ProgState -> Either ConstraintErr (Raw.Program With.ProgState)
perform p =
    case Raw.runAstOpRes p checkOp of
        Left err -> Left err
        Right (_, p') -> Right p'
