{- This module is useful to make expressions outside the context of type-inference, but after the type-inference
performed. This could be necessary, because to build some expressions may be diffcult because of some implementation
details. These API are not included in Compiler.Ast.Typed, because some implementation details involves the use of
some notions which are not up to Compiler.Ast.Typed to know. -}
module Compiler.Types.Lib.SpecialExpr
    ( newApplication
) where

import Compiler.Ast.Common
import qualified Compiler.Types.Lib.FreeVars as Fresh
import qualified Compiler.Ast.Typed as Ty

{- It builds a new application expression, caring of free variables in an FV container.
NB: type of any expressions MUST be already instantiated.
NB: constraints are ignored.
NB: if you are the type-inference client, you should NOT use this. -}
newApplication
    :: Ty.NotedExpr a
    -> [Ty.NotedExpr a]
    -> Fresh.FV ()
    -> Either (Ty.UnificationError a) (Ty.NotedExpr a, Fresh.FV ())
newApplication ne [] fv = Right (ne, fv)
newApplication ne (arg : t) fv =
    let (varRep, fv') = Fresh.allocFreeVar () fv in
    let argSt = stateOf arg in
    let exprSt = stateOf ne in
    let monoTy = Ty.newLHTyFromLVTy $ Ty.newLVTy varRep Ty.LKConst Ty.Representational argSt in
    let argTy = unwrap $ Ty.instantiateUnqualifying (Ty.typeOf arg) [] in
    let argFunTy = argTy <> monoTy in
    let exprTy = unwrap $ Ty.instantiateUnqualifying (Ty.typeOf ne) [] in
        case Ty.unify exprTy argFunTy of
            Left err -> Left err
            Right subst ->
                let appTy = Ty.specType argTy subst in
                let appTy' = Ty.liftMonoType appTy in
                let appExpr = Ty.newAppNotedExpr' ne arg appTy' exprSt exprSt in
                    newApplication appExpr t fv'
    where
        unwrap (Just ty) = ty
