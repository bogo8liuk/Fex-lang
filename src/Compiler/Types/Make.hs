{- This is a gathering module: that means it is a common point for type-system-related
functionalities. -}
module Compiler.Types.Make
    ( KInfr.TypeGenErr(..)
    , module Compiler.Types.Lib.FreeVars
    , module Compiler.Types.Tables
    , Prep.RawBinding(..)
    , Prep.SortedBindings
    , TyInf.BindingToRefine
    , inferKind
    , buildCons
    , inferConstraint
    , mkInsts
    , getBindings
    , addInstsBindings
    , disambiguateNested
    , splitRec
    , sortDefs
    , mkTypedProg
) where

import Lib.Utils((<|))
import qualified Lib.Counter as C
import Compiler.State as With
import Compiler.Ast.Tree as Raw
import Compiler.Types.Tables
import Compiler.Types.Builder.New as NewTypes
import qualified Compiler.Types.Builder.Kind as KInfr(TypeGenErr(..))
import qualified Compiler.Types.Builder.Cons as Cons
import qualified Compiler.Types.Builder.Constraints as NewConts
import qualified Compiler.Types.Builder.Instances as Inst
import qualified Compiler.Desugar.Nested as Nested
import qualified Compiler.Types.Prepare as Prep
import qualified Compiler.Types.Builder.Type as TyInf
import Compiler.Types.Lib.FreeVars

initState :: String -> String
initState phase =
    "Initial state of " ++ phase ++ ". This a special state of the compiler which " ++
    "should NEVER be reached"

inferKind
    :: Raw.Program With.ProgState
    -> Either KInfr.TypeGenErr (TypesTable With.ProgState, Raw.Program With.ProgState)
inferKind p = runAstOpRes p NewTypes.build

buildCons
    :: Raw.Program With.ProgState
    -> FV ()
    -> TypesTable With.ProgState
    -> Either Cons.ConsBuildError (DataConsTable With.ProgState, FV (), Raw.Program With.ProgState)
buildCons p fv tt =
    case runAstOpRes p $ Cons.build tt fv of
        Left err -> Left err
        Right ((dct, fv'), p') -> Right (dct, fv', p')

inferConstraint
    :: Raw.Program With.ProgState
    -> TypesTable With.ProgState
    -> Either NewConts.ContGenErr (ConstraintsTable With.ProgState, Raw.Program With.ProgState)
inferConstraint p tt = runAstOpRes p $ NewConts.build tt

mkInsts
    :: Raw.Program With.ProgState
    -> TypesTable With.ProgState
    -> ConstraintsTable With.ProgState
    -> FV ()
    -> Either Inst.InstanceErr
        ( InstsTable With.ProgState
        , PropMethodsTable With.ProgState
        , ImplTable With.ProgState
        , FV ()
        , Raw.Program With.ProgState
        )
mkInsts p tt ct fv =
    case runAstOpRes p $ Inst.build tt ct fv of
        Left err -> Left err
        Right ((insts, mhts, it, fv'), p') -> Right (insts, mhts, it, fv', p')

getBindings :: Raw.Program With.ProgState -> [Raw.SDUnion With.ProgState]
getBindings p =
    fst $ Raw.runAstOp p Raw.getGenSymDecls

addInstsBindings :: InstsTable With.ProgState -> [Raw.SDUnion With.ProgState] -> [Raw.SDUnion With.ProgState]
addInstsBindings insts bs =
    let instsBs = getAllElems insts in
        instsBs ++ bs

disambiguateNested :: [Raw.SDUnion With.ProgState] -> [Raw.SDUnion With.ProgState]
disambiguateNested = Nested.replaceNested

splitRec
    :: PropMethodsTable With.ProgState
    -> [Raw.SDUnion With.ProgState]
    -> [Prep.RawBinding]
splitRec = flip Prep.splitRecDefs

sortDefs :: PropMethodsTable With.ProgState -> [Prep.RawBinding] -> Prep.SortedBindings
sortDefs = flip Prep.sortDefs

mkTypedProg
    :: FV ()
    -> TypesTable With.ProgState
    -> DataConsTable With.ProgState
    -> ConstraintsTable With.ProgState
    -> PropMethodsTable With.ProgState
    -> ImplTable With.ProgState
    -> Prep.SortedBindings
    -> Either TyInf.TyInfErr
        ( TypedProgram With.ProgState
        , FV ()
        , C.AlphabeticCounterObj
        , [TyInf.BindingToRefine]
        )
mkTypedProg = TyInf.build
