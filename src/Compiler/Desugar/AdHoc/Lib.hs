module Compiler.Desugar.AdHoc.Lib
    ( DispatchErr(..)
    , AdHocHandle
    , buildEnv
    , dispatchErr
    , OldVarRep
    , NewVarRep
    , TopSymRep
    , RecRep
    , SymbolToReplace(..)
    , Replacement
    , RecReplacement
    , getTopSym
    , RecDep
    , AdHocBinding(..)
    , getBindingSingleton
    , getRecReps
    , mkAdHocBinding
    --, rmDupReplacements
    , findSymbolRep
    , findSymbolAndRecReps
    , findSymbolInBinding
    , addBinding
) where

import Lib.Utils
--import Data.List(nubBy)
--import Data.List.NonEmpty hiding (nubBy)
import Control.Monad.Trans
import Lib.Result
import Compiler.State as With
import Compiler.Ast.Common
import qualified Compiler.Ast.Typed as Ty
import qualified Compiler.Types.Lib.State as S
import Compiler.Types.Tables

data DispatchErr = 
      SymNotFound (SymbolRep, Maybe TopSymRep)
    | RecBindingNotFound SymbolRep
    | NonRecInRec SymbolRep
    | NestedAsGlobal SymbolRep

instance InfoShow DispatchErr where
    infoShow (SymNotFound _) = unexpNoInfo
    infoShow (RecBindingNotFound _) = unexpNoInfo
    infoShow (NonRecInRec _) = unexpNoInfo
    infoShow (NestedAsGlobal _) = unexpNoInfo

instance DebugShow DispatchErr where
    dbgShow (SymNotFound (symRep, Nothing)) =
        symbolRepToStr symRep ++ " not found in typed program"
    dbgShow (SymNotFound (symRep, Just topSymRep)) =
        symbolRepToStr symRep ++ " not found in " ++ symbolRepToStr topSymRep
    dbgShow (RecBindingNotFound symRep) =
        symbolRepToStr symRep ++ " not found in a recursive binding"
    dbgShow (NonRecInRec symRep) =
        "Non-recursive binding " ++ symbolRepToStr symRep ++ " found in recursive binding"
    dbgShow (NestedAsGlobal symRep) =
        "Nested symbol " ++ symbolRepToStr symRep ++ " found in typed program"

instance UnreachableState DispatchErr where
    isUnreachable err @ (SymNotFound _) = Just $ dbgShow err
    isUnreachable err @ (RecBindingNotFound _) = Just $ dbgShow err
    isUnreachable err @ (NonRecInRec _) = Just $ dbgShow err
    isUnreachable err @ (NestedAsGlobal _) = Just $ dbgShow err

type AdHocHandle a = S.EitherHandle () DispatchErr a

buildEnv :: TypedProgram With.ProgState -> PropMethodsTable With.ProgState -> S.GenericState ()
buildEnv tp mhts = S.initNoFreeVarsState noElems noElems noElems noElems mhts noElems tp ()

dispatchErr :: DispatchErr -> AdHocHandle a
dispatchErr = lift . Left

type OldVarRep = SymbolRep
type NewVarRep = SymbolRep
type TopSymRep = SymbolRep
type RecRep = SymbolRep

data SymbolToReplace =
      Nested TopSymRep OldVarRep
    | TopLevel OldVarRep
type Replacement = (SymbolToReplace, NewVarRep, [Ty.LangSpecConstraint With.ProgState])
type RecReplacement = Replacement

getTopSym :: Replacement -> TopSymRep
getTopSym (TopLevel oldVarRep, _, _) = oldVarRep
getTopSym (Nested topSymRep _, _, _) = topSymRep

type RecDep = (RecRep, BindingSingleton With.ProgState)

{- A binding which has been replaced. -}
data AdHocBinding =
    {- Exactly like `TyNonRec` of `TypedBinding`. -}
      NonRec (BindingSingleton With.ProgState)
    {- The replaced binding singleton and the lazy replacements to apply to the other bindings which builds up the
    recursive binding. -}
    | Rec (BindingSingleton With.ProgState) [RecDep]

getBindingSingleton :: AdHocBinding -> AdHocHandle (BindingSingleton With.ProgState)
getBindingSingleton (NonRec bSing) = return bSing
getBindingSingleton (Rec bSing _) = return bSing

getRecReps :: AdHocBinding -> AdHocHandle (Maybe [RecDep])
getRecReps (NonRec _) = return Nothing
getRecReps (Rec _ recDeps) = return $ Just recDeps

mkAdHocBinding :: BindingSingleton With.ProgState -> Maybe [RecDep] -> AdHocBinding
mkAdHocBinding bSing Nothing = NonRec bSing
mkAdHocBinding bSing (Just recReps) = Rec bSing recReps

findSymbolRep :: SymbolRep -> AdHocHandle (TypedBinding With.ProgState)
findSymbolRep symRep = do
    tp <- S.getProg
    case kFind symRep tp of
        Nothing -> dispatchErr $ SymNotFound (symRep, Nothing)
        Just tyb -> return tyb

findSymbolAndRecReps
    :: SymbolRep
    -> AdHocHandle
        ( BindingSingleton With.ProgState
        , Maybe [RecDep]
        )
findSymbolAndRecReps symRep = do
    tyb <- findSymbolRep symRep
    case tyb of
        TyNonRec b -> return (b, Nothing)
        TyRec bs -> do
            (nVar, nVars, ne) <- findSymbolInBinding symRep tyb
            let nVarRep = strOf nVar
            let recReps = fltmap (onlyNotSelected nVarRep) bs
            return ((nVar, nVars, ne), Just recReps)
    where
        onlyNotSelected nVarRep b @ (nVar', _, _) =
            let nVarRep' = strOf nVar' in
                if nVarRep' == nVarRep
                then Nothing
                else Just (nVarRep', b)

findSymbolInBinding :: SymbolRep -> TypedBinding With.ProgState -> AdHocHandle (BindingSingleton With.ProgState)
findSymbolInBinding _ (TyNonRec b) = return b
findSymbolInBinding symRep (TyRec bs) = do
    case firstThat (\(nVar, _, _) -> strOf nVar == symRep) bs of
        Nothing -> dispatchErr $ RecBindingNotFound symRep
        Just b -> return b

addBinding :: TypedBinding With.ProgState -> AdHocHandle ()
addBinding tb = do
    tp <- S.getProg
    let tp' = addElem tb tp
    S.putProg tp'
