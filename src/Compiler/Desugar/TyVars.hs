{-# LANGUAGE FlexibleContexts #-}
{- See functions `bind` and `replace` for the reason of ScopedTypeVariables extension. -}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.Desugar.TyVars
    ( updateTyVars
    --, RebindErr(..)
    , BoundData
    , addToBound
    , getBind
) where

import Data.Tuple(swap)
import Data.Map.Strict as M hiding (map)
import Compiler.Ast.Common
import qualified Compiler.Ast.Tree as Raw
import qualified Compiler.Types.Lib.FreeVars as Fresh

{-
newtype RebindErr

instance InfoShow RebindErr where
    infoShow (ExternalErr _) = unexpNoInfo

instance DebugShow RebindErr where
    dbgShow (ExternalErr reason) = reason

instance UnreachableState RebindErr where
    isUnreachable (ExternalErr reason) = Just reason
        -}

{- This is useful to discover the variable another variable has been replaced with. The values in
the map should correspond to bound variables in a container. -}
type BoundData = Map TyVarRep TyVarRep

addToBound :: Raw.ParamTypeName a -> TyVarRep -> BoundData -> BoundData
addToBound pty = insert (repOf pty)

getBind :: Raw.ParamTypeName a -> BoundData -> Maybe TyVarRep
getBind pty = M.lookup (repOf pty)

bindTyVar
    :: Raw.ParamTypeName a
    -> (Fresh.FV (), BoundData)
    -> (Raw.ParamTypeName a, (Fresh.FV (), BoundData))
bindTyVar pty (fv, bd) =
    let pRep = repOf pty in
    let pStr = tokenRepToStr pRep in
        {- Checking if pty is already bound. -}
        case getBind pty bd of
            Just var -> (Raw.buildPtyName var $ stateOf pty, (fv, bd))
            Nothing ->
                case Fresh.tryAllocFreeVar pStr () fv of
                    Just fv' -> (pty, (fv', addToBound pty pRep bd))
                    Nothing ->
                        let (var, fv') = Fresh.allocFreeVar () fv in
                        let varRep = tokenRepFromStr var in
                            ( Raw.buildPtyName varRep $ stateOf pty
                            , (fv', addToBound pty varRep bd)
                            )

replaceTyVar
    :: Raw.ParamTypeName a
    -> (Fresh.FV (), BoundData)
    -> (Raw.ParamTypeName a, (Fresh.FV (), BoundData))
replaceTyVar pty vars @ (_, bd) =
    case getBind pty bd of
        Just var -> (Raw.buildPtyName var $ stateOf pty, vars)
        {- In this case, the variable is not bound yet, so it is a binding! -}
        Nothing -> bindTyVar pty vars

bind
    :: forall tok a. Binder (tok a) (Raw.ParamTypeName a)
    => (Fresh.FV (), BoundData)
    -> tok a
    -> ((Fresh.FV (), BoundData), tok a)
bind vars token =
    {- This is a hack to quiet the compiler: without the help of ScopedTypeVariables extension, the function
    call of `onBindingsOf` below would result in a compiler error (something like "could not deduce...")
    because the context `Binder (tok a) (ParamTypeName a)` would not have matched the type of the previous
    named function call, because `bindTyVar` would have had a different `a` type variable. -}
    let bindF = bindTyVar
         :: Raw.ParamTypeName a
         -> (Fresh.FV (), BoundData)
         -> (Raw.ParamTypeName a, (Fresh.FV (), BoundData)) in
        swap $ onBindingsOf token vars bindF

replace
    :: forall tok a. Binder (tok a) (Raw.ParamTypeName a)
    => (Fresh.FV (), BoundData)
    -> tok a
    -> ((Fresh.FV (), BoundData), tok a)
replace vars token =
    {- See `bind` for the reason of this -}
    let replaceF = replaceTyVar
         :: Raw.ParamTypeName a
         -> (Fresh.FV (), BoundData)
         -> (Raw.ParamTypeName a, (Fresh.FV (), BoundData)) in
        swap $ onScopedOf token vars replaceF

rebind
    :: Binder (tok a) (Raw.ParamTypeName a)
    => Fresh.FV ()
    -> tok a
    -> (Fresh.FV (), tok a)
rebind fv token =
    let ((fv', bd'), token') = bind (fv, empty) token in
    let ((fv'', _), token'') = replace (fv', bd') token' in
        (fv'', token'')

rebindAdt :: Fresh.FV () -> Raw.AstOp a (Fresh.FV ())
rebindAdt fv = Raw.safeSetAdt fv rebind

rebindProp :: Fresh.FV () -> Raw.AstOp a (Fresh.FV ())
rebindProp fv = Raw.safeSetProp fv rebind

rebindInst :: Fresh.FV () -> Raw.AstOp a (Fresh.FV ())
rebindInst fv = Raw.safeSetInst fv rebind

rebindMultiSymDecl :: Fresh.FV () -> Raw.AstOp a (Fresh.FV ())
rebindMultiSymDecl fv = Raw.safeSetMultiSymDecl fv rebind

rebindSymDecl :: Fresh.FV () -> Raw.AstOp a (Fresh.FV ())
rebindSymDecl fv = Raw.safeSetSymDecl fv rebind

updateOp :: Raw.AstOp a (Fresh.FV ())
updateOp = do
    fv1 <- rebindAdt Fresh.newFreeVarsContainer
    fv2 <- rebindProp fv1
    fv3 <- rebindInst fv2
    fv4 <- rebindMultiSymDecl fv3
    rebindSymDecl fv4

updateTyVars :: Raw.Program a -> (Fresh.FV (), Raw.Program a)
updateTyVars p =
    Raw.runAstOp p updateOp
