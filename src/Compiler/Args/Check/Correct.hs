{-# LANGUAGE FlexibleContexts #-}

module Compiler.Args.Check.Correct
    ( ArgsError(..)
    , argsCheck
) where

import Compiler.Ast.Common
import Compiler.Ast.Tree as Raw
import Compiler.State as With
import qualified Compiler.Args.Check.Count as Count

data ArgsError =
      AdtErr (With.ProgState, Raw.UnConType With.ProgState)
    | AliasErr (With.ProgState, Raw.UnConType With.ProgState)
    | PropErrCont (With.ProgState, Raw.Constraint With.ProgState)
    | PropErrInst (With.ProgState, Raw.Instance With.ProgState)
    {- A name which does not exist: this should not happen -}
    | NoName String

argsNE :: String
argsNE = " has a number of arguments not equal to the number of arguments in the property definition at "

instance Show ArgsError where
    show (AdtErr (adtst, ty)) =
        "Type at " ++ show (stateOf ty) ++ " has a number of arguments greater than the number " ++
        "of arguments in its definition at " ++ show adtst
    show (AliasErr (aliasst, ty)) =
        "Type at " ++ show (stateOf ty) ++ " has a number of arguments not equal to the number " ++
        "of arguments in its definition at " ++ show aliasst
    show (PropErrCont (intfst, c)) =
        "Constraint at " ++ show (stateOf c) ++ argsNE ++ show intfst
    show (PropErrInst (intfst, i)) =
        "Instance at " ++ show (stateOf i) ++ " for " ++ repOf (Raw.intfNameFromInst i) ++ argsNE ++
        show intfst
    show _ = "Unreachable state"

argsCheckOnTypes :: Count.ArgsMap -> Raw.AstOpRes With.ProgState ArgsError ()
argsCheckOnTypes m = Raw.lookupAllUnCons () $ \_ ty ->
    case Raw.baseNameFromUnCon ty of
        Right _ -> Right ()  --on parametric types, the check is not performed
        Left r -> let name = repOf r in
            case Count.lookup (name, Count.ItemAdt) m of
                Nothing -> case Count.lookup (name, Count.ItemAlias) m of
                    Nothing -> Left $ NoName name   --This should not happen (the check on names should be already performed)
                    Just (n, st) ->
                        {- In the case of an alias, the number of arguments must match exactly, otherwise it is not
                        possible to expand an alias. -}
                        if n /= length (argsOf ty)
                        then Left $ AliasErr (st, ty)
                        else Right ()
                Just (n, st) ->
                    {- The check: number of actual arguments cannot be greater (strictly) than the number of arguments
                    in the definition for the type. -}
                    if n < length (argsOf ty)
                    then Left $ AdtErr (st, ty)
                    else Right ()

argsCheckOnTok :: HasArgs tok (Raw.UnConType With.ProgState)
               => (tok -> Raw.IntfName With.ProgState)
               -> tok
               -> Count.ArgsMap
               -> ((With.ProgState, tok) -> ArgsError)     --The error constructor
               -> Either ArgsError ()
argsCheckOnTok f tok m errcon =
    let iName = repOf $ f tok in
        case Count.lookup (iName, Count.ItemProp) m of
            Nothing -> Left $ NoName iName
            Just (n, st) ->
                if n /= length (argsOf tok)
                then Left $ errcon (st, tok)
                else Right ()

argsCheckOnConts :: Count.ArgsMap -> Raw.AstOpRes With.ProgState ArgsError ()
argsCheckOnConts m = Raw.lookupConts () $ \_ c -> argsCheckOnTok Raw.intfNameFromCont c m PropErrCont

argsCheckOnInsts :: Count.ArgsMap -> Raw.AstOpRes With.ProgState ArgsError ()
argsCheckOnInsts m = Raw.lookupInst () $ \_ i -> argsCheckOnTok Raw.intfNameFromInst i m PropErrInst

argsCheck :: Raw.AstOpRes With.ProgState ArgsError ()
argsCheck = do
    m <- astOpRes Count.countOp
    argsCheckOnTypes m
    argsCheckOnConts m
    argsCheckOnInsts m
