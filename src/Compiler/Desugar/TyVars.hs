{-# LANGUAGE FlexibleContexts #-}
{- See functions `bind` and `replace` for the reason of ScopedTypeVariables extension. -}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.Desugar.TyVars
    ( updateTyVars
    , RebindErr(..)
    , BoundData
    , addToBound
    , getBind
) where

import Lib.Result
import Data.Map.Strict as M hiding (map)
import Compiler.Ast.Common
import qualified Compiler.Ast.Tree as Raw
import qualified Compiler.Types.Lib.FreeVars as Fresh

newtype RebindErr =
      ExternalErr String

instance InfoShow RebindErr where
    infoShow (ExternalErr _) = unexpNoInfo

instance DebugShow RebindErr where
    dbgShow (ExternalErr reason) = reason

instance UnreachableState RebindErr where
    isUnreachable (ExternalErr reason) = Just reason

{- This is useful to discover the variable another variable has been replaced with. The values in
the map should correspond to bound variables in a container. -}
type BoundData = Map String String

addToBound :: Raw.ParamTypeName a -> String -> BoundData -> BoundData
addToBound pty = insert (strOf pty)

getBind :: Raw.ParamTypeName a -> BoundData -> Maybe String
getBind pty = M.lookup (strOf pty)

bindTyVar
    :: Raw.ParamTypeName a
    -> Either RebindErr (Fresh.FV (), BoundData)
    -> (Raw.ParamTypeName a, Either RebindErr (Fresh.FV (), BoundData))
bindTyVar pty err @ (Left _) = (pty, err)
bindTyVar pty (Right (fv, bd)) =
    let pRep = strOf pty in
        {- Checking if pty is already bound. -}
        case getBind pty bd of
            Just var -> (Raw.buildPtyName var $ stateOf pty, Right (fv, bd))
            Nothing ->
                case Fresh.tryAllocFreeVar pRep () fv of
                    Just fv' -> (pty, Right (fv', addToBound pty pRep bd))
                    Nothing ->
                        let (var, fv') = Fresh.allocFreeVar () fv in
                            ( Raw.buildPtyName var $ stateOf pty
                            , Right (fv', addToBound pty var bd)
                            )

replaceTyVar
    :: Raw.ParamTypeName a
    -> Either RebindErr (Fresh.FV (), BoundData)
    -> (Raw.ParamTypeName a, Either RebindErr (Fresh.FV (), BoundData))
replaceTyVar pty err @ (Left _) = (pty, err)
replaceTyVar pty (Right (fv, bd)) =
    case getBind pty bd of
        Just var -> (Raw.buildPtyName var $ stateOf pty, Right (fv, bd))
        {- In this case, the variable is not bound yet, so it is a binding! -}
        Nothing -> bindTyVar pty (Right (fv, bd))

bind
    :: forall tok a. Binder (tok a) (Raw.ParamTypeName a)
    => (Fresh.FV (), BoundData)
    -> tok a
    -> Either RebindErr ((Fresh.FV (), BoundData), tok a)
bind (fv, bd) token =
    {- This is a hack to quiet the compiler: without the help of ScopedTypeVariables extension, the function
    call of `onBindingsOf` below would result in a compiler error (something like "could not deduce...")
    because the context `Binder (tok a) (ParamTypeName a)` would not have matched the type of the previous
    named function call, because `bindTyVar` would have had a different `a` type variable. -}
    let bindF = bindTyVar
         :: Raw.ParamTypeName a
         -> Either RebindErr (Fresh.FV (), BoundData)
         -> (Raw.ParamTypeName a, Either RebindErr (Fresh.FV (), BoundData)) in
        case onBindingsOf token (Right (fv, bd)) bindF of
            (_, Left err) -> Left err
            (token', Right res) -> Right (res, token')

replace
    :: forall tok a. Binder (tok a) (Raw.ParamTypeName a)
    => (Fresh.FV (), BoundData)
    -> tok a
    -> Either RebindErr ((Fresh.FV (), BoundData), tok a)
replace (fv, bd) token =
    {- See `bind` for the reason of this -}
    let replaceF = replaceTyVar :: Raw.ParamTypeName a
                                -> Either RebindErr (Fresh.FV (), BoundData)
                                -> (Raw.ParamTypeName a, Either RebindErr (Fresh.FV (), BoundData)) in
        case onScopedOf token (Right (fv, bd)) replaceF of
            (_, Left err) -> Left err
            (token', Right res) -> Right (res, token')

rebind
    :: Binder (tok a) (Raw.ParamTypeName a)
    => Fresh.FV ()
    -> tok a
    -> Either RebindErr (Fresh.FV (), tok a)
rebind fv token =
    case bind (fv, empty) token of
        Left err -> Left err
        Right ((fv', bd'), token') ->
            case replace (fv', bd') token' of
                Left err -> Left err
                Right ((fv'', _), token'') -> Right (fv'', token'')

rebindAdt :: Fresh.FV () -> Raw.AstOp a RebindErr (Fresh.FV ())
rebindAdt fv = Raw.setAdt fv rebind

rebindProp :: Fresh.FV () -> Raw.AstOp a RebindErr (Fresh.FV ())
rebindProp fv = Raw.setProp fv rebind

rebindInst :: Fresh.FV () -> Raw.AstOp a RebindErr (Fresh.FV ())
rebindInst fv = Raw.setInst fv rebind

rebindMultiSymDecl :: Fresh.FV () -> Raw.AstOp a RebindErr (Fresh.FV ())
rebindMultiSymDecl fv = Raw.setMultiSymDecl fv rebind

rebindSymDecl :: Fresh.FV () -> Raw.AstOp a RebindErr (Fresh.FV ())
rebindSymDecl fv = Raw.setSymDecl fv rebind

updateOp :: Raw.AstOp a RebindErr (Fresh.FV ())
updateOp = do
    fv1 <- rebindAdt Fresh.newFreeVarsContainer
    fv2 <- rebindProp fv1
    fv3 <- rebindInst fv2
    fv4 <- rebindMultiSymDecl fv3
    rebindSymDecl fv4

updateTyVars
    :: Raw.Program a
    -> Either RebindErr (Fresh.FV (), Raw.Program a)
updateTyVars p =
    Raw.runAstOp p (ExternalErr "initial state of rebinding phase") updateOp
