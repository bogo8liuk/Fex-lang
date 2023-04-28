{- This module just builds a LangKindType values from the result of Builder.Kind module. -}
module Compiler.Types.Builder.New
    ( build
    , cleanVars
) where

import Data.Map.Strict as Map hiding (map, foldl')
import Compiler.Ast.Common
import Compiler.Ast.Tree as Raw
import Compiler.Ast.Typed as Ty
import Compiler.State as With
import Compiler.Types.Builder.Kind
import Compiler.Types.Tables

findRoles :: [(String, ProgState)] -> Raw.AlgebraicDataType With.ProgState -> [(String, Role, ProgState)]
findRoles [] _ = []
findRoles ((varRep, st) : t) adt =
    if isRepresentational
    then (varRep, Representational, st) : findRoles t adt
    else (varRep, Phantom, st) : findRoles t adt
    where
        isRepresentational =
            let onScopedOf' =
                 onScopedOf
                 :: Raw.AlgebraicDataType With.ProgState
                 -> Bool
                 -> (Raw.ParamTypeName With.ProgState -> Bool -> (Raw.ParamTypeName With.ProgState, Bool))
                 -> (Raw.AlgebraicDataType With.ProgState, Bool) in
                snd . onScopedOf' adt False $
                \pty occurred ->
                    if occurred || repOf pty == varRep
                    then (pty, True)
                    else (pty, False)

addAdt
    :: KindsTable
    -> TypesTable With.ProgState
    -> Raw.AlgebraicDataType With.ProgState
    -> Either TypeGenErr (TypesTable With.ProgState)
addAdt kt tt adt =
    let name = repOf $ Raw.adtNameFrom adt in
    let params = findRoles (map (\pty -> (repOf pty, stateOf pty)) $ argsOf adt) adt in
    let st = stateOf adt in
        case Map.lookup name kt of
            Nothing -> Left $ UnreachableState nameNotFound
            Just (lk, _) ->
                case Ty.newLNTy name lk params st of
                    Nothing -> Left $ UnreachableState "Inconsistent kind with number of states"
                    Just lnty -> Right $ addElem lnty tt

buildTypesTable :: KindsTable -> Raw.AstOp With.ProgState TypeGenErr (TypesTable With.ProgState)
buildTypesTable kt =
    Raw.lookupAdt noElems $ addAdt kt

{- Given a table of kinds which already contains all the kinds of type names in a program, it checks if
there are remaining kind variables and it substitutes them with kind constants. It is necessary and safe
because the kind inference algorithm can infer the kind of all the names, except the ones which does not
depends from anyone or the ones which depends from someone in a closed circular way. But if a type variable
must be of kind different from `LKConst` (so `SubLK` instance), then there must exist in the program a type
(variable or not) from which it depends, so the kind inference algorithm is able to infer its kind
(partially or completely) and so remaining kind variables at the end of the algorithm execution must be
turned into `LKConst` values. -}
cleanVars :: KindsTable -> KindsTable
cleanVars kt = fromList . clean $ toAscList kt
    where
        clean [] = []
        clean ((name, (LKVar _, st)) : t) = (name, (LKConst, st)) : clean t
        clean ((name, (SubLK ks, st)) : t) = (name, (SubLK $ cleanKinds ks, st)) : clean t
        clean (assoc : t) = assoc : clean t

        cleanKinds [] = []
        cleanKinds (LKVar _ : t) = LKConst : cleanKinds t
        cleanKinds (SubLK ks : t) = SubLK (cleanKinds ks) : cleanKinds t
        cleanKinds (other : t) = other : cleanKinds t

cleanVarsOp :: KindsTable -> Raw.AstOp With.ProgState err KindsTable
cleanVarsOp kt = return $ cleanVars kt

build :: Raw.AstOp With.ProgState TypeGenErr (TypesTable With.ProgState)
build = do
    (kt, _) <- kindDiscover empty
    kt' <- cleanVarsOp kt
    buildTypesTable kt'
