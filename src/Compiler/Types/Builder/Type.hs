{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{- TODO: a very huge optimization could be applying substitutions only to symbols which are under the context of
"current" top-level symbols. Same for finding free variables in safe generalization. This optimizations are safe,
because a top-level symbol, when inferred, should have a type which will be never refined and this is true, because
type inference algorithm works under hypothesys that all symbols are sorted according to their dependencies. -}

module Compiler.Types.Builder.Type
    ( build
    , TyInfErr(..)
    , BindingToRefine
) where

import Debug.Trace(trace)
import Lib.Utils
import Lib.Result
import Data.List as L
import Data.List.NonEmpty(NonEmpty(..))
import Data.Map.Strict as M hiding (map, drop, elemAt)
import Data.Semigroup
import Control.Monad.State.Lazy
import Lib.Monad.Utils
import qualified Compiler.NameSpace as NS
import qualified Lib.Counter as C
import qualified Compiler.Types.Lib.State as S
import qualified Compiler.Desugar.Names as Desugar
import qualified Compiler.Desugar.BreadthPM as RmMult
import qualified Compiler.Config.Types as BITy
import qualified Compiler.Config.Props as BIProps
import qualified Compiler.Types.Lib.FreeVars as Fresh
import qualified Compiler.Types.Lib.InferKind as KInfr
import Compiler.Ast.Common
import Compiler.State as With
import qualified Compiler.Ast.Tree as Raw
import qualified Compiler.Ast.Typed as Ty
import Compiler.Types.Tables
import qualified Compiler.Types.Lib.Create as Create
import qualified Compiler.Types.Prepare as Prep

{- The type to be respected while doing inference. -}
type ExpectedType = Ty.LangHigherType With.ProgState
type ExpectedQualType = Ty.LangQualType With.ProgState
type ActualType = Ty.LangHigherType With.ProgState
type ActualQualType = Ty.LangQualType With.ProgState
{- The type coming from type-hinting. -}
type TypeHinting = Ty.LangHigherType With.ProgState
type QualTypeHinting = Ty.LangQualType With.ProgState

data TyInfErr =
      UnexpType ExpectedQualType ActualQualType With.ProgState
    | UnificationError (Ty.UnificationError With.ProgState)
    | SatisfiabilityError (Ty.SatisfiabilityError With.ProgState)
    | TooManyArgs String With.ProgState (Maybe ExpectedType)
    | NoMatchingInst (Ty.LangSpecConstraint With.ProgState)
    | GenSpecTestErr
    -- | DispatchErr DispatchErr
    | TypeBuildErr (Create.Err KInfr.TypeGenErr)
    | ContMakingErr (Ty.ContBuildError With.ProgState)
    | RmMultPM RmMult.RmBreadthPmErr
    | ConNotFound (Raw.ADTConName With.ProgState)
    | ArgsConErr (Raw.ADTConName With.ProgState)
    | TypeNotInfrd With.ProgState
    | NotGotVar
    | ZeroaryProperty (Ty.LangNewConstraint With.ProgState)
    {- For all those situations in which it is not possible creating new typed tokens, without any
    additional information. -}
    | GenTokCreationErr String
    | SymNotFound String
    | BindingNotFound String
    | ValNotFound String
    | PropNotFound String
    | TypeNotFound String
    | SpecStackErr String
    | GenUnreachableState String

cannotMkNewVar :: String
cannotMkNewVar = "Cannot get a type variable during type inference"

propNotFound :: String -> String
propNotFound name = "Cannot find " ++ name ++ " property in constraints table"

typeNotFound :: String -> String
typeNotFound name = "Cannot find " ++ name ++ " type in types table"

symNotFound :: String -> String
symNotFound name = "Cannot find " ++ name ++ " symbol in the program"

bindingNotFound :: String -> String
bindingNotFound name = "Symbol " ++ name ++ " has not bound arguments and/or expression in typing environment"

valNotFound :: String -> String
valNotFound name = "Cannot find " ++ name ++ " value in occurred tokens table"

rawUnexpType :: String -> String -> ProgState -> String
rawUnexpType tyRep tyRep' st =
    "Could not match expected type " ++ tyRep ++ " with actual type " ++ tyRep' ++ " at " ++ show st

unexpType :: ExpectedType -> ActualType -> ProgState -> String
unexpType ty ty' =
    rawUnexpType (Ty.showLHTy ty) (Ty.showLHTy ty')

unexpQualType :: ExpectedQualType -> ActualQualType -> ProgState -> String
unexpQualType ty ty' =
    rawUnexpType (Ty.showLQTy ty) (Ty.showLQTy ty')

noMatchingInst :: Ty.LangSpecConstraint With.ProgState -> String
noMatchingInst lspc =
    "There is no instance which makes true the predicate " ++ Ty.showCont lspc ++ " at " ++ show (stateOf lspc)

undecidableInst
    :: Ty.LangSpecConstraint With.ProgState
    -> [Ty.LangSpecConstraint With.ProgState]
    -> String
undecidableInst lspc cs =
    "Could not select the right instance which makes true: \n" ++
    Ty.showCont lspc ++
    "\namong: " ++
    concatMap ((++ "\n") . Ty.showCont) cs

conNotFound :: Raw.ADTConName With.ProgState -> String
conNotFound con = repOf con ++ " constructor not found in constructors table"

argsConErr :: Raw.ADTConName With.ProgState -> String
argsConErr con =
    "Inconsistent use of constructor " ++ repOf con ++ " at " ++ show (stateOf con) ++ "; it should have a " ++
    "different number of applied arguments"

typeNotInferred :: With.ProgState -> String
typeNotInferred st = "Cannot infer the type of the token at " ++ show st

genCannotCreateTok :: String -> String
genCannotCreateTok info = "Typed token creation error due to: " ++ info

genSpecTestErr :: String
genSpecTestErr = "Something goes wrong during specialization test computation"

zeroaryProp :: Ty.LangNewConstraint With.ProgState -> String
zeroaryProp lnc = "Property " ++ repOf lnc ++ " should not be zero-ary"

tooManyArgs :: String -> With.ProgState -> Maybe ExpectedType -> String
tooManyArgs tokDesc st mayty =
    tokDesc ++ " at " ++ show st ++ " has too many applied arguments" ++
    case mayty of
        Nothing -> ""
        Just ty -> ", the expected type is: " ++ Ty.showLHTy ty

occursCheck :: Ty.LangHigherType With.ProgState -> Ty.LangHigherType With.ProgState -> String
occursCheck lhty lhty' =
    "Occurs check (at " ++ show (stateOf lhty) ++ ") between types: " ++ Ty.showLHTy lhty ++ " <--> " ++
    Ty.showLHTy lhty'

unmatchKinds :: Ty.LangHigherType With.ProgState -> Ty.LangHigherType With.ProgState -> String
unmatchKinds lhty lhty' =
    "Kinds not matching (at " ++ show (stateOf lhty) ++ ") of types: " ++ Ty.showLHTy lhty ++ " <--> " ++
    Ty.showLHTy lhty'

specStackErr :: String -> String
specStackErr reason = "Specialization stack error: " ++ reason

instance InfoShow TyInfErr where
    infoShow (UnexpType lqty lqty' st) = unexpQualType lqty lqty' st
    infoShow (UnificationError (Ty.UnmatchTypes expTy actTy)) = unexpType expTy actTy $ stateOf expTy
    infoShow (UnificationError (Ty.OccursCheck ty ty')) = occursCheck ty ty'
    infoShow (UnificationError (Ty.UnmatchKinds ty ty')) = unmatchKinds ty ty'
    infoShow (UnificationError (Ty.TrySwap expTy actTy)) = unexpType expTy actTy $ stateOf expTy
    infoShow (SatisfiabilityError (Ty.CouldNotDeduce lspc)) = noMatchingInst lspc
    infoShow (SatisfiabilityError (Ty.UndecidableInst lspc cs)) = undecidableInst lspc cs
    infoShow (TooManyArgs desc st mayty) = tooManyArgs desc st mayty
    infoShow (NoMatchingInst lspc) = noMatchingInst lspc
    --infoShow (DispatchErr err) = infoShow err
    infoShow GenSpecTestErr = unexpNoInfo
    infoShow (TypeBuildErr (Create.ContErr err @ (Ty.KindErr _ _))) = descOf err
    infoShow (TypeBuildErr (Create.CustomErr err)) = infoShow err
    infoShow (TypeBuildErr _) = unexpNoInfo
    infoShow (ContMakingErr err @ (Ty.KindErr _ _)) = descOf err
    infoShow (RmMultPM err) = infoShow err
    infoShow (ContMakingErr _) = unexpNoInfo
    infoShow (ConNotFound _) = unexpNoInfo
    infoShow (ArgsConErr con) = argsConErr con
    infoShow (TypeNotInfrd _) = unexpNoInfo
    infoShow NotGotVar = unexpNoInfo
    infoShow (ZeroaryProperty _) = unexpNoInfo
    infoShow (GenTokCreationErr _) = unexpNoInfo
    infoShow (SymNotFound _) = unexpNoInfo
    infoShow (BindingNotFound _) = unexpNoInfo
    infoShow (ValNotFound _) = unexpNoInfo
    infoShow (PropNotFound _) = unexpNoInfo
    infoShow (TypeNotFound _) = unexpNoInfo
    infoShow (SpecStackErr _) = unexpNoInfo
    infoShow (GenUnreachableState _) = unexpNoInfo

instance DebugShow TyInfErr where
    dbgShow (UnexpType lqty lqty' st) = unexpQualType lqty lqty' st
    dbgShow (UnificationError (Ty.UnmatchTypes expTy actTy)) = unexpType expTy actTy $ stateOf expTy
    dbgShow (UnificationError (Ty.OccursCheck ty ty')) = occursCheck ty ty'
    dbgShow (UnificationError (Ty.UnmatchKinds ty ty')) = unmatchKinds ty ty'
    dbgShow (UnificationError (Ty.TrySwap expTy actTy)) = unexpType expTy actTy $ stateOf expTy
    dbgShow (SatisfiabilityError (Ty.CouldNotDeduce lspc)) = noMatchingInst lspc
    dbgShow (SatisfiabilityError (Ty.UndecidableInst lspc cs)) = undecidableInst lspc cs
    dbgShow (TooManyArgs desc st ty) = tooManyArgs desc st ty
    dbgShow (NoMatchingInst lspc) = noMatchingInst lspc
    --dbgShow (DispatchErr err) = dbgShow err
    dbgShow GenSpecTestErr = genSpecTestErr
    dbgShow (TypeBuildErr (Create.CustomErr err)) = dbgShow err
    dbgShow (TypeBuildErr err) = descOf err
    dbgShow (ContMakingErr err) = descOf err
    dbgShow (RmMultPM err) = dbgShow err
    dbgShow (ConNotFound con) = conNotFound con
    dbgShow (ArgsConErr con) = argsConErr con
    dbgShow (TypeNotInfrd st) = typeNotInferred st
    dbgShow NotGotVar = cannotMkNewVar
    dbgShow (ZeroaryProperty lnc) = zeroaryProp lnc
    dbgShow (GenTokCreationErr info) = genCannotCreateTok info
    dbgShow (SymNotFound name) = symNotFound name
    dbgShow (BindingNotFound name) = bindingNotFound name
    dbgShow (ValNotFound name) = valNotFound name
    dbgShow (PropNotFound name) = propNotFound name
    dbgShow (TypeNotFound name) = typeNotFound name
    dbgShow (SpecStackErr reason) = specStackErr reason
    dbgShow (GenUnreachableState reason) = reason

instance UnreachableState TyInfErr where
    isUnreachable UnexpType {} = Nothing
    isUnreachable (UnificationError Ty.UnmatchTypes {}) = Nothing
    isUnreachable (UnificationError Ty.OccursCheck {}) = Nothing
    isUnreachable (UnificationError Ty.UnmatchKinds {}) = Nothing
    isUnreachable (UnificationError Ty.TrySwap {}) = Nothing
    isUnreachable (SatisfiabilityError (Ty.CouldNotDeduce _)) = Nothing
    isUnreachable (SatisfiabilityError (Ty.UndecidableInst _ _)) = Nothing
    isUnreachable TooManyArgs {} = Nothing
    isUnreachable NoMatchingInst {} = Nothing
    --isUnreachable (DispatchErr err) = isUnreachable err
    isUnreachable GenSpecTestErr = Just $ dbgShow GenSpecTestErr
    isUnreachable (TypeBuildErr (Create.ContErr (Ty.KindErr _ _))) = Nothing
    isUnreachable (TypeBuildErr (Create.CustomErr err)) = isUnreachable err
    isUnreachable err @ (TypeBuildErr _) = Just $ dbgShow err
    isUnreachable (ContMakingErr (Ty.KindErr _ _)) = Nothing
    isUnreachable (RmMultPM err) = isUnreachable err
    isUnreachable err @ (ContMakingErr _) = Just $ dbgShow err
    isUnreachable err @ (ConNotFound _) = Just $ dbgShow err
    isUnreachable (ArgsConErr _) = Nothing
    isUnreachable err @ (TypeNotInfrd _) = Just $ dbgShow err
    isUnreachable NotGotVar = Just $ dbgShow NotGotVar
    isUnreachable err @ (ZeroaryProperty _) = Just $ dbgShow err
    isUnreachable err @ (GenTokCreationErr _) = Just $ dbgShow err
    isUnreachable err @ (SymNotFound _) = Just $ dbgShow err
    isUnreachable err @ (BindingNotFound _) = Just $ dbgShow err
    isUnreachable err @ (ValNotFound _) = Just $ dbgShow err
    isUnreachable err @ (PropNotFound _) = Just $ dbgShow err
    isUnreachable err @ (TypeNotFound _) = Just $ dbgShow err
    isUnreachable err @ (SpecStackErr _) = Just $ dbgShow err
    isUnreachable err @ (GenUnreachableState _) = Just $ dbgShow err

type MutRecSymbol = String
type MutRecHint = Ty.LangHigherType With.ProgState

{- The CurrentTyEnv type is a something like a synonym for the typing environement, but it keeps also scope
information. The expression is wrapped by a Maybe type constructor, because not all the symbols have an associated
expression, think about the parameter of lambda abstraction. Moreover, it allows to insert a symbol and the
associated expression later in the flow of the algorithm. Same for the list of noted variables which represents
the arguments of a symbol. It is named "contextual" because it's useful to keep information about the binding
(mutually recursive or not) which is being inferred. It should be erased each time a global binding is inferred. -}
type ContextualBinding =
    ( Ty.NotedVar With.ProgState
    , Maybe [Ty.NotedVar With.ProgState]
    , Maybe (Ty.NotedExpr With.ProgState)
    , [MutRecSymbol]
    , [MutRecHint]
    )
type CurrentTyEnv = Map (String, NS.Scope) ContextualBinding

type ScopedContextBinding =
    ( Ty.NotedVar With.ProgState
    , NS.Scope
    , Maybe [Ty.NotedVar With.ProgState]
    , Maybe (Ty.NotedExpr With.ProgState)
    , [MutRecSymbol]
    , [MutRecHint]
    )
type ScopedContextOp a =
       Ty.NotedVar With.ProgState
    -> NS.Scope
    -> Maybe [Ty.NotedVar With.ProgState]
    -> Maybe (Ty.NotedExpr With.ProgState)
    -> [MutRecSymbol]
    -> [MutRecHint]
    -> a

type ConstraintProblem = (Ty.LangSpecConstraint With.ProgState, String)

{- Is the current local (so, non-global) binding recursive? -}
type InnerRec = Maybe String

{- Counter to use in order to disambiguate names in the code. -}
type UniqueVarCounter = C.AlphabeticCounterObj
{- Counter to create new placeholders, look at instance dispatch algorithm. -}
type PlaceholderCounter = C.CounterObj

type BindingToRefine = NonEmpty String
type NestedSymbol = String

data TyInfInfo =
    TIInfo
        CurrentTyEnv
        {- The current constraint problems to be solved. -}
        [ConstraintProblem]
        {- The mutually recursive symbols of the current binding. -}
        [MutRecSymbol]
        {- The eventual recursive inner current binding. -}
        InnerRec
        NS.Scope
        {- Counter to create new placeholders. The placeholders are used in the algorithm of instance dispatch which
        works like this: the method of a property has a normal identifier; however, it has to be replaced with the
        right implementation which depends on the types in the context, so, in order to make unique symbols, the
        property method has attached placeholders which are part of the name:

            <prop-method><ph1>...<phn>

        the placeholders are then filled during type inference with names of types which are used in the context in
        order to select the right instance. If a method has only filled placeholders, then its constraint problem is
        solved and it should result in an instance method:

            <prop-method><ty1>...<tyn>
        -}
        PlaceholderCounter
        UniqueVarCounter
        {- The symbols stack: it tracks the actual symbols which are being inferred. -}
        [NestedSymbol]
        {- The bindings which needs "refinement" after instance inference. -}
        [BindingToRefine]

type TyInfSt = S.GenericState TyInfInfo

infoGetCurTyEnv :: TyInfInfo -> CurrentTyEnv
infoGetCurTyEnv (TIInfo cte _ _ _ _ _ _ _ _) = cte

infoGetCurConts :: TyInfInfo -> [ConstraintProblem]
infoGetCurConts (TIInfo _ ccs _ _ _ _ _ _ _) = ccs

infoGetMutRec :: TyInfInfo -> [MutRecSymbol]
infoGetMutRec (TIInfo _ _ mr _ _ _ _ _ _) = mr

infoGetInnerRec :: TyInfInfo -> InnerRec
infoGetInnerRec (TIInfo _ _ _ ir _ _ _ _ _) = ir

infoGetScope :: TyInfInfo -> NS.Scope
infoGetScope (TIInfo _ _ _ _ sc _ _ _ _) = sc

infoGetPhC :: TyInfInfo -> PlaceholderCounter
infoGetPhC (TIInfo _ _ _ _ _ pc _ _ _) = pc

infoGetVC :: TyInfInfo -> UniqueVarCounter
infoGetVC (TIInfo _ _ _ _ _ _ vc _ _) = vc

infoGetNested :: TyInfInfo -> [NestedSymbol]
infoGetNested (TIInfo _ _ _ _ _ _ _ ns _) = ns

infoGetToRefine :: TyInfInfo -> [BindingToRefine]
infoGetToRefine (TIInfo _ _ _ _ _ _ _ _ btr) = btr

infoPutCurTyEnv :: TyInfInfo -> CurrentTyEnv -> TyInfInfo
infoPutCurTyEnv (TIInfo _ ccs mr ir sc pc vc ns btr) cte = TIInfo cte ccs mr ir sc pc vc ns btr

infoPutCurConts :: TyInfInfo -> [ConstraintProblem] -> TyInfInfo
infoPutCurConts (TIInfo cte _ mr ir sc pc vc ns btr) ccs = TIInfo cte ccs mr ir sc pc vc ns btr

infoPutMutRec :: TyInfInfo -> [MutRecSymbol] -> TyInfInfo
infoPutMutRec (TIInfo cte ccs _ ir sc pc vc ns btr) mr = TIInfo cte ccs mr ir sc pc vc ns btr

infoPutInnerRec :: TyInfInfo -> InnerRec -> TyInfInfo
infoPutInnerRec (TIInfo cte ccs mr _ sc pc vc ns btr) ir = TIInfo cte ccs mr ir sc pc vc ns btr

infoPutScope :: TyInfInfo -> NS.Scope -> TyInfInfo
infoPutScope (TIInfo cte ccs mr ir _ pc vc ns btr) sc = TIInfo cte ccs mr ir sc pc vc ns btr

infoPutPhC :: TyInfInfo -> PlaceholderCounter -> TyInfInfo
infoPutPhC (TIInfo cte ccs mr ir sc _ vc ns btr) pc = TIInfo cte ccs mr ir sc pc vc ns btr

infoPutVC :: TyInfInfo -> UniqueVarCounter -> TyInfInfo
infoPutVC (TIInfo cte ccs mr ir sc pc _ ns btr) vc = TIInfo cte ccs mr ir sc pc vc ns btr

infoPutNested :: TyInfInfo -> [NestedSymbol] -> TyInfInfo
infoPutNested (TIInfo cte ccs mr ir sc pc vc _ btr) ns = TIInfo cte ccs mr ir sc pc vc ns btr

infoPutToRefine :: TyInfInfo -> [BindingToRefine] -> TyInfInfo
infoPutToRefine (TIInfo cte ccs mr ir sc pc vc ns _) btr = TIInfo cte ccs mr ir sc pc vc ns btr

{- The reason why of these stGet* functions is not doing pattern matching in get* functions. -}
stGetCurTyEnv :: TyInfSt -> CurrentTyEnv
stGetCurTyEnv = infoGetCurTyEnv . S.fetchData

stGetCurConts :: TyInfSt -> [ConstraintProblem]
stGetCurConts = infoGetCurConts . S.fetchData

stGetMutRec :: TyInfSt -> [MutRecSymbol]
stGetMutRec = infoGetMutRec . S.fetchData

stGetInnerRec :: TyInfSt -> InnerRec
stGetInnerRec = infoGetInnerRec . S.fetchData

stGetScope :: TyInfSt -> NS.Scope
stGetScope = infoGetScope . S.fetchData

stGetPhC :: TyInfSt -> PlaceholderCounter
stGetPhC = infoGetPhC . S.fetchData

stGetVC :: TyInfSt -> UniqueVarCounter
stGetVC = infoGetVC . S.fetchData

stGetNested :: TyInfSt -> [NestedSymbol]
stGetNested = infoGetNested . S.fetchData

stGetToRefine :: TyInfSt -> [BindingToRefine]
stGetToRefine = infoGetToRefine . S.fetchData

stPut :: TyInfSt -> a -> (TyInfInfo -> a -> TyInfInfo) -> TyInfSt
stPut tiSt newObj infoPut =
    let tiInfo = S.fetchData tiSt in
        S.updateData tiSt $ infoPut tiInfo newObj

stPutCurTyEnv :: TyInfSt -> CurrentTyEnv -> TyInfSt
stPutCurTyEnv tiSt curTe = stPut tiSt curTe infoPutCurTyEnv

stPutCurConts :: TyInfSt -> [ConstraintProblem] -> TyInfSt
stPutCurConts tiSt curConts = stPut tiSt curConts infoPutCurConts

stPutMutRec :: TyInfSt -> [MutRecSymbol] -> TyInfSt
stPutMutRec tiSt mrSyms = stPut tiSt mrSyms infoPutMutRec

stPutInnerRec :: TyInfSt -> InnerRec -> TyInfSt
stPutInnerRec tiSt ir = stPut tiSt ir infoPutInnerRec

stPutScope :: TyInfSt -> NS.Scope -> TyInfSt
stPutScope tiSt sc = stPut tiSt sc infoPutScope

stPutPhC :: TyInfSt -> PlaceholderCounter -> TyInfSt
stPutPhC tiSt pc = stPut tiSt pc infoPutPhC

stPutVC :: TyInfSt -> UniqueVarCounter -> TyInfSt
stPutVC tiSt vc = stPut tiSt vc infoPutVC

stPutNested :: TyInfSt -> [NestedSymbol] -> TyInfSt
stPutNested tiSt ns = stPut tiSt ns infoPutNested

stPutToRefine :: TyInfSt -> [BindingToRefine] -> TyInfSt
stPutToRefine tiSt btr = stPut tiSt btr infoPutToRefine

type TyInf res = S.EitherHandle TyInfInfo TyInfErr res

tyInfErr :: TyInfErr -> TyInf res
tyInfErr err = lift $ Left err

unificationErr :: Ty.UnificationError With.ProgState -> TyInf res
unificationErr err = lift . Left $ UnificationError err

noMatchingCases :: TyInf a
noMatchingCases = tyInfErr $ GenTokCreationErr "No matching cases"

showAtomTypedToken :: (AtomRep (tok With.ProgState), Ty.HasType tok) => tok With.ProgState -> TyInf String
showAtomTypedToken token = do
    return (repOf token ++ " : " ++ Ty.showLPTy (Ty.typeOf token))

showCurTyEnv :: TyInf String
showCurTyEnv = do
    te <- getCurTyEnv
    return $ "CURRENT TYPING ENVIRONMENT:" ++ concatMap showBinding (toList te)
    where
        showBinding ((symRep, sc), (nVar, _, _, _, _)) =
            "\n  " ++ symRep ++ " : sc=" ++ show sc ++ " : " ++ Ty.showLPTy (Ty.typeOf nVar)

getTT :: TyInf (TypesTable With.ProgState)
getTT = S.getTypes

getCT :: TyInf (ConstraintsTable With.ProgState)
getCT = S.getConts

getFV :: TyInf (Fresh.FV ())
getFV = S.getFV

getProg :: TyInf (TypedProgram With.ProgState)
getProg = S.getProg

getInsts :: TyInf (InstsTable With.ProgState)
getInsts = S.getInsts

getMethods :: TyInf (PropMethodsTable With.ProgState)
getMethods = S.getMethods

getIT :: TyInf (ImplTable With.ProgState)
getIT = S.getImpls

getCons :: TyInf (DataConsTable With.ProgState)
getCons = S.getDataCons

getCurTyEnv :: TyInf CurrentTyEnv
getCurTyEnv = gets stGetCurTyEnv

getCurConts :: TyInf [ConstraintProblem]
getCurConts = gets stGetCurConts

getMutRec :: TyInf [MutRecSymbol]
getMutRec = gets stGetMutRec

getInnerRec :: TyInf InnerRec
getInnerRec = gets stGetInnerRec

getScope :: TyInf NS.Scope
getScope = gets stGetScope

getPhC :: TyInf PlaceholderCounter
getPhC = gets stGetPhC

getVC :: TyInf UniqueVarCounter
getVC = gets stGetVC

getNested :: TyInf [NestedSymbol]
getNested = gets stGetNested

getToRefine :: TyInf [BindingToRefine]
getToRefine = gets stGetToRefine

put' :: s -> (TyInfSt -> s -> TyInfSt) -> TyInf ()
put' newObj update = S.putWith update newObj

local :: TyInf res -> s -> TyInf s -> (s -> TyInf ()) -> TyInf res
local op newobj getObj putObj = do
    obj <- getObj
    putObj newobj
    res <- op
    putObj obj
    return res

{- There are three type of functions here:
1) put* functions: they just update the state, in particular, they update just what they tell to update.
2) with* functions: they are just a shorthand for put* functions, what they do is just updating the state
and then doing a computation. The updated state keeps holding even after the computation.
3) withLocal* functions: they do the same of with* functions, but after the computation, the state is resumed
to what it was before the computation. -}

putTT :: TypesTable With.ProgState -> TyInf ()
putTT = S.putTypes

withTT :: TyInf res -> TypesTable With.ProgState -> TyInf res
withTT op newtt = do
    putTT newtt
    op

withLocalTT :: TyInf res -> TypesTable With.ProgState -> TyInf res
withLocalTT op newtt = local op newtt getTT putTT

putCT :: ConstraintsTable With.ProgState -> TyInf ()
putCT = S.putConts

withCT :: TyInf res -> ConstraintsTable With.ProgState -> TyInf res
withCT op newct = do
    putCT newct
    op

withLocalCT :: TyInf res -> ConstraintsTable With.ProgState -> TyInf res
withLocalCT op newct = local op newct getCT putCT

putFV :: Fresh.FV () -> TyInf ()
putFV = S.putFV

withFV :: TyInf res -> Fresh.FV () -> TyInf res
withFV op newfv = do
    putFV newfv
    op

withLocalFV :: TyInf res -> Fresh.FV () -> TyInf res
withLocalFV op newfv = local op newfv getFV putFV

putProg :: TypedProgram With.ProgState -> TyInf ()
putProg = S.putProg

withProg :: TyInf res -> TypedProgram With.ProgState -> TyInf res
withProg op newtp = do
    putProg newtp
    op

withLocalProg :: TyInf res -> TypedProgram With.ProgState -> TyInf res
withLocalProg op newtp = local op newtp getProg putProg

putIT :: ImplTable With.ProgState -> TyInf ()
putIT = S.putImpls

withIT :: TyInf res -> ImplTable With.ProgState -> TyInf res
withIT op newit = do
    putIT newit
    op

withLocalIT :: TyInf res -> ImplTable With.ProgState -> TyInf res
withLocalIT op newit = local op newit getIT putIT

putInsts :: InstsTable With.ProgState -> TyInf ()
putInsts = S.putInsts

withInsts :: TyInf res -> InstsTable With.ProgState -> TyInf res
withInsts op newinsts = do
    putInsts newinsts
    op

withLocalInsts :: TyInf res -> InstsTable With.ProgState -> TyInf res
withLocalInsts op newinsts = local op newinsts getInsts putInsts

putMethods :: PropMethodsTable With.ProgState -> TyInf ()
putMethods = S.putMethods

withMethods :: TyInf res -> PropMethodsTable With.ProgState -> TyInf res
withMethods op newmhts = do
    putMethods newmhts
    op

withLocalMethods :: TyInf res -> PropMethodsTable With.ProgState -> TyInf res
withLocalMethods op newmhts = local op newmhts getMethods putMethods

putCons :: DataConsTable With.ProgState -> TyInf ()
putCons = S.putDataCons

withCons :: TyInf res -> DataConsTable With.ProgState -> TyInf res
withCons op newcons = do
    putCons newcons
    op

withLocalCons :: TyInf res -> DataConsTable With.ProgState -> TyInf res
withLocalCons op newcons = local op newcons getCons putCons

putCurTyEnv :: CurrentTyEnv -> TyInf ()
putCurTyEnv newcurTe = put' newcurTe stPutCurTyEnv

withCurTyEnv :: TyInf res -> CurrentTyEnv -> TyInf res
withCurTyEnv op newcurTe = do
    putCurTyEnv newcurTe
    op

withLocalCurTyEnv :: TyInf res -> CurrentTyEnv -> TyInf res
withLocalCurTyEnv op newcurTe = local op newcurTe getCurTyEnv putCurTyEnv

doWithCurTyEnvBindings :: ([ScopedContextBinding] -> TyInf a) -> TyInf a
doWithCurTyEnvBindings bsop = do
    curTe <- getCurTyEnv
    bsop . map getNecessary $ M.toList curTe
    where
        getNecessary ((_, sc), (nVar, mayNVars, mayExpr, mRecSyms, mRecHints)) =
            (nVar, sc, mayNVars, mayExpr, mRecSyms, mRecHints)

putCurConts :: [ConstraintProblem] -> TyInf ()
putCurConts newcurCs = put' newcurCs stPutCurConts

withCurConts :: TyInf res -> [ConstraintProblem] -> TyInf res
withCurConts op newcurCs = do
    putCurConts newcurCs
    op

withLocalCurConts :: TyInf res -> [ConstraintProblem] -> TyInf res
withLocalCurConts op newcurCs = local op newcurCs getCurConts putCurConts

addCurConts :: [ConstraintProblem] -> TyInf ()
addCurConts newcurCs = do
    curCs <- getCurConts
    {- Avoiding duplicates among constraint problems. -}
    let curCs' = nubBy contEq $ newcurCs ++ curCs
    putCurConts curCs'
    where
        contEq (c, _) (c', _) = c == c'

resetCurConts :: TyInf ()
resetCurConts = putCurConts []

putMutRec :: [MutRecSymbol] -> TyInf ()
putMutRec mrSyms = put' mrSyms stPutMutRec

withMutRec :: TyInf res -> [MutRecSymbol] -> TyInf res
withMutRec op newmrSyms = do
    putMutRec newmrSyms
    op

withLocalMutRec :: TyInf res -> [MutRecSymbol] -> TyInf res
withLocalMutRec op newmrSyms = local op newmrSyms getMutRec putMutRec

putInnerRec :: InnerRec -> TyInf ()
putInnerRec ir = put' ir stPutInnerRec

withInnerRec :: TyInf res -> InnerRec -> TyInf res
withInnerRec op newir = do
    putInnerRec newir
    op

withLocalInnerRec :: TyInf res -> InnerRec -> TyInf res
withLocalInnerRec op newir = local op newir getInnerRec putInnerRec

{- NB: using this is quite unsafe. After having used this function, the client should handle local symbols,
because this function does not touch them. Functions like withDecScope handles the scope in a better way. -}
putScope :: NS.Scope -> TyInf ()
putScope newsc = put' newsc stPutScope

withScope :: TyInf res -> NS.Scope -> TyInf res
withScope op newsc = do
    putScope newsc
    op

withLocalScope :: TyInf res -> NS.Scope -> TyInf res
withLocalScope op newsc = local op newsc getScope putScope

withIncScope :: TyInf res -> TyInf res
withIncScope op = do
    sc <- getScope
    op `withScope` NS.incrScope sc

incScope :: TyInf ()
incScope = withIncScope doNothing'

withLocalIncScope :: TyInf res -> TyInf res
withLocalIncScope op = do
    res <- withIncScope op
    withDecScope $ return res

removeNotInScope :: TyInf ()
removeNotInScope = do
    curTe <- getCurTyEnv
    sc <- getScope
    putCurTyEnv $ M.filterWithKey (\(_, sc') _ -> sc' <= sc) curTe

{- It decreases the actual scope. It also removes all those tokens which are in the current scope (before it
is changed).
NB: it does not check if the scope is already set to the lowest value. -}
withDecScope :: TyInf res -> TyInf res
withDecScope op = do
    sc <- getScope
    putScope $ NS.decrScope sc
    removeNotInScope
    op

decScope :: TyInf ()
decScope = withDecScope doNothing'

putPhC :: PlaceholderCounter -> TyInf ()
putPhC newpc = put' newpc stPutPhC

withPhC :: TyInf res -> PlaceholderCounter -> TyInf res
withPhC op newpc = do
    putPhC newpc
    op

putVC :: UniqueVarCounter -> TyInf ()
putVC newvc = put' newvc stPutVC

withVC :: TyInf res -> UniqueVarCounter -> TyInf res
withVC op newvc = do
    putVC newvc
    op

putNested :: [NestedSymbol] -> TyInf ()
putNested newns = put' newns stPutNested

withNested :: TyInf res -> [NestedSymbol] -> TyInf res
withNested op newns = do
    putNested newns
    op

withLocalNested :: TyInf res -> [NestedSymbol] -> TyInf res
withLocalNested op newns = local op newns getNested putNested

pushNested :: NestedSymbol -> TyInf ()
pushNested sym = do
    ns <- getNested
    putNested $ sym : ns

emptyNested :: TyInf a
emptyNested = tyInfErr $ GenUnreachableState "Empty nested symbols stack"

popNested :: TyInf ()
popNested = do
    ns <- getNested
    case ns of
        [] -> doNothing'
        (_ : t) -> putNested t

putToRefine :: [BindingToRefine] -> TyInf ()
putToRefine newbtr = put' newbtr stPutToRefine

withToRefine :: TyInf res -> [BindingToRefine] -> TyInf res
withToRefine op newbtr = do
    putToRefine newbtr
    op

addBindingToRefine :: BindingToRefine -> TyInf ()
addBindingToRefine b = do
    bs <- getToRefine
    putToRefine $ b : bs

getSuffix :: TyInf String
getSuffix = do
    vc <- getVC
    let (suf, newvc) = Desugar.mkProgUniqueName vc
    return suf `withVC` newvc

{- It performs an operation and it returns its result and the final scope of the operation. This is useful
if the operation has to be performed with a local scope ("local" here means that the scope is resumed to a
previous value after the operation has been performed). -}
scopeAlong :: TyInf res -> TyInf (res, NS.Scope)
scopeAlong op = do
    res <- op
    sc <- getScope
    return (res, sc)

clearCurTyEnv :: TyInf ()
clearCurTyEnv = putCurTyEnv empty

querySymbols :: String -> TyInf [ScopedContextBinding]
querySymbols symRep = do
    curTe <- getCurTyEnv
    {- This is just a query to have just variables which match the input variable. -}
    let curTokens = M.toList $ M.filterWithKey (\(symRep', _) _ -> symRep' == symRep) curTe
    let curTokens' =
         map
            (\((_, sc), (nVar, mayNVars, mayExpr, recSyms, recHints)) ->
                (nVar, sc, mayNVars, mayExpr, recSyms, recHints)
            ) curTokens
    tp <- getProg
    token <- bindingFrom tp
    return $ token ++ curTokens'
    where
        bindingFrom tp =
            case kFind symRep tp of
                Nothing -> return []
                Just (TyNonRec (nVar, nVars, ne)) ->
                    return [(nVar, NS.globalScope, Just nVars, Just ne, [], [])]
                Just (TyRec bs) ->
                    case firstThat (\(nVar, _, _) -> repOf nVar == symRep) bs of
                        Nothing ->
                            tyInfErr $ GenUnreachableState ("No recursive binding with symbol " ++ symRep)
                        Just (nVar, nVars, ne) ->
                            return
                                [(nVar
                                , NS.globalScope
                                , Just nVars
                                , Just ne
                                , map (\(nVar, _, _) -> repOf nVar) bs
                                , []
                                )]

queryInScopeSymbols :: String -> TyInf [ScopedContextBinding]
queryInScopeSymbols symRep = do
    symbols <- querySymbols symRep
    filterOutOfScope symbols
    where
        filterOutOfScope tokens = do
            sc <- getScope
            return $ L.filter (\(_, sc', _, _, _, _) -> sc' <= sc) tokens

defineOrder :: (a, NS.Scope, b, c, d, e) -> (a, NS.Scope, b, c, d, e) -> Ordering
defineOrder (_, sc, _, _, _, _) (_, sc', _, _, _, _) = sc `compare` sc'

getSymbol :: String -> TyInf (Maybe ScopedContextBinding)
getSymbol symRep = do
    symbols <- querySymbols symRep
    case maximumBy' defineOrder symbols of
        Nothing -> return Nothing
        Just res -> return $ Just res

{- Same of getSymbol, but it returns an error if it cannot find the symbol. -}
getSymbol' :: String -> TyInf ScopedContextBinding
getSymbol' symRep = do
    res <- getSymbol symRep
    case res of
        Nothing -> tyInfErr $ SymNotFound symRep
        Just binding -> return binding

getGlobalSymbol :: String -> TyInf (Maybe ScopedContextBinding)
getGlobalSymbol symRep = do
    symbols <- querySymbols symRep
    case minimumBy' defineOrder symbols of
        Nothing -> return Nothing
        justB @ (Just (_, sc, _, _, _ ,_)) ->
            if sc /= NS.globalScope
            then return Nothing
            else return justB

getGlobalSymbol' :: String -> TyInf ScopedContextBinding
getGlobalSymbol' symRep = do
    res <- getGlobalSymbol symRep
    case res of
        Nothing -> tyInfErr $ SymNotFound symRep
        Just binding -> return binding

putSymbol
    :: Ty.NotedVar With.ProgState
    -> Maybe [Ty.NotedVar With.ProgState]
    -> Maybe (Ty.NotedExpr With.ProgState)
    -> [MutRecSymbol]
    -> TyInf ()
putSymbol nVar maynVars mayExpr mRecSyms = do
    sc <- getScope
    putSymbolWithScope nVar sc maynVars mayExpr mRecSyms

putSymbolWithScope
    :: Ty.NotedVar With.ProgState
    -> NS.Scope
    -> Maybe [Ty.NotedVar With.ProgState]
    -> Maybe (Ty.NotedExpr With.ProgState)
    -> [MutRecSymbol]
    -> TyInf ()
putSymbolWithScope nVar sc mayNVars mayExpr mRecSyms = do
    curTe <- getCurTyEnv
    let newcurTe = M.insert (repOf nVar, sc) (nVar, mayNVars, mayExpr, mRecSyms, []) curTe
    putCurTyEnv newcurTe

{- If the symbol has not the expression, it has not a dispatch map as well. -}
putSymbolNoExpr :: Ty.NotedVar With.ProgState -> [MutRecSymbol] -> TyInf ()
putSymbolNoExpr nVar = putSymbol nVar Nothing Nothing

putSymbolWithExpr
    :: Ty.NotedVar With.ProgState
    -> [Ty.NotedVar With.ProgState]
    -> Ty.NotedExpr With.ProgState
    -> [MutRecSymbol]
    -> TyInf ()
putSymbolWithExpr nVar nVars nExpr = putSymbol nVar <| Just nVars <| Just nExpr

{- It puts a symbol in the program without expression, with the most general type (singleton type variable). It is
meant for recursive symbols, indeed, it takes also the list of mutable recursive symbols representations. -}
addMostGenNVar :: Raw.SDUnion With.ProgState -> [MutRecSymbol] -> TyInf ()
addMostGenNVar sd mutRecSymbols = do
    let sym = Raw.symNameFromSD sd
    let symRep = repOf sym
    let st = stateOf sym
    nVar <- mostGenNVar symRep st
    putSymbolNoExpr nVar mutRecSymbols

{- NB: do not use this to update bindings in the typing environment. This allows arbitrary changes. -}
updateSymbol :: String -> NS.Scope -> (ContextualBinding -> ContextualBinding) -> TyInf ()
updateSymbol symRep sc f = do
    te <- getCurTyEnv
    let newcurTe = M.adjust f (symRep, sc) te
    putCurTyEnv newcurTe

updateSymbolWithArgsAndExpr
    :: Ty.NotedVar With.ProgState
    -> NS.Scope
    -> [Ty.NotedVar With.ProgState]
    -> Ty.NotedExpr With.ProgState
    -> TyInf ()
updateSymbolWithArgsAndExpr nVar sc nVars nExpr =
    updateSymbol (repOf nVar) sc $
        \(_, _, _, mRecSyms, mRecHints) ->
            (nVar, Just nVars, Just nExpr, mRecSyms, mRecHints)

addTypeHintToRecSym :: String -> NS.Scope -> Ty.LangHigherType With.ProgState -> TyInf ()
addTypeHintToRecSym symRep sc ty = do
    updateSymbol symRep sc $
        \(nVar, mayNVars, mayNExpr, mRecSyms, mRecHints) ->
            (nVar, mayNVars, mayNExpr, mRecSyms, ty : mRecHints)

putProgSymbol :: TypedBinding With.ProgState -> TyInf ()
putProgSymbol tyb = do
    tp <- getProg
    let tp' = addElem tyb tp
    putProg tp'

isMutRec :: TyInf Bool
isMutRec = do
    mrSyms <- getMutRec
    return . not $ L.null mrSyms

isMutRecSym :: String -> TyInf Bool
isMutRecSym symRep = do
    mrSyms <- getMutRec
    isRec <- isRecInner symRep
    return $ symRep `elem` mrSyms || isRec

isRecInnerBinding :: Raw.SDUnion With.ProgState -> TyInf Bool
isRecInnerBinding sd = do
    mhts <- getMethods
    let symRep = repOf $ Raw.symNameFromSD sd
    {- The only possible different mut. rec. dependency for head symbol in `sd` is exactly the head symbol in `sd`.
    Other mutually recursive dependencies cannot exist, however, the algorithm here adds also mut. rec. dependencies
    of the current global binding. -}
    return $ symRep `elem` Prep.depsOf sd mhts

isRecInner :: String -> TyInf Bool
isRecInner symRep = do
    ir <- getInnerRec
    case ir of
        Nothing -> return False
        Just symRep' -> return $ symRep == symRep'

ifRecInner :: String -> TyInf a -> TyInf a -> TyInf a
ifRecInner symRep whenIt'sRec cont = do
    isRec <- isRecInner symRep
    if isRec
    then whenIt'sRec
    else cont

ifMutRecSym :: String -> TyInf a -> TyInf a -> TyInf a
ifMutRecSym symRep whenIt'sRec cont = do
    isRec <- isMutRecSym symRep
    if isRec
    then whenIt'sRec
    else cont

ifMutRecGlobalSym :: String -> TyInf a -> TyInf a -> TyInf a
ifMutRecGlobalSym symRep whenIt'sRec cont = do
    mrSyms <- getMutRec
    isRecIn <- isRecInner symRep
    if symRep `elem` mrSyms && not isRecIn
    then whenIt'sRec
    else cont

withInnerRecBindingCheck :: Raw.SDUnion With.ProgState -> (Raw.SDUnion With.ProgState -> TyInf res) -> TyInf res
withInnerRecBindingCheck sd op = do
    isRec <- isRecInnerBinding sd
    if isRec
    then do
        let symRep = repOf $ Raw.symNameFromSD sd
        putInnerRec $ Just symRep
        addMostGenNVar sd [symRep]
    else putInnerRec Nothing
    op sd

withLocalInnerRecBindingCheck :: Raw.SDUnion With.ProgState -> (Raw.SDUnion With.ProgState -> TyInf res) -> TyInf res
withLocalInnerRecBindingCheck sd op = do
    ir <- getInnerRec
    res <- withInnerRecBindingCheck sd op
    putInnerRec ir
    return res

{- It applies a substitution on a generic token, by checking for true constraints after each substitution as well. -}
specToken
    :: Ty.SpecType tok
    => tok With.ProgState
    -> [Ty.Substitution With.ProgState]
    -> TyInf (tok With.ProgState)
specToken token [] = return token
specToken token (subst : t) = do
    let token' = token `Ty.specType` subst
    specToken token' t

specContProblem :: [Ty.Substitution With.ProgState] -> ConstraintProblem -> TyInf ConstraintProblem
specContProblem substs (c, i) = do
    c' <- c `specToken` substs
    return (c', i)

specCurConts :: [Ty.Substitution With.ProgState] -> TyInf ()
specCurConts substs = do
    cps <- getCurConts
    cps' <- mapM (specContProblem substs) cps
    putCurConts $ nubBy eqCont cps'
    where
        eqCont (c, _) (c', _) = c == c'

specCurTyEnv :: [Ty.Substitution With.ProgState] -> TyInf ()
specCurTyEnv substs = do
    curTe <- getCurTyEnv
    let bindings = M.toList curTe
    updBindings <- mapM specBinding bindings
    let updCurTe = M.fromList updBindings
    putCurTyEnv updCurTe
    where
        specBinding (k, (nVar, mayNVars, mayExpr, mRecSyms, mRecHints)) = do
            nVar' <- specToken nVar substs
            case (mayNVars, mayExpr) of
                (Nothing, Nothing) ->
                    return (k, (nVar', Nothing, Nothing, mRecSyms, mRecHints))
                (Nothing, Just ne) -> do
                    ne' <- specToken ne substs
                    return (k, (nVar', Nothing, Just ne', mRecSyms, mRecHints))
                (Just nVars, Nothing) -> do
                    nVars' <- mapM (`specToken` substs) nVars
                    return (k, (nVar', Just nVars', Nothing, mRecSyms, mRecHints))
                (Just nVars, Just ne) -> do
                    ne' <- specToken ne substs
                    nVars' <- mapM (`specToken` substs) nVars
                    return (k, (nVar', Just nVars', Just ne', mRecSyms, mRecHints))

{- It specializes current typing environment and current constraint problems. -}
specCurContext :: [Ty.Substitution With.ProgState] -> TyInf ()
specCurContext substs = do
    specCurConts substs
    specCurTyEnv substs

{- If searches for a symbol in the typing environment with a certain string representation. If it is found, then its
type is instantiated. -}
ifExistingSymRep :: String -> ScopedContextOp (TyInf a) -> (TyInf a -> TyInf a)
ifExistingSymRep symRep withNVar cont = do
    maybeVar <- getSymbol symRep
    case maybeVar of
        Nothing -> cont
        --Ignoring `op`
        Just (nVar, sc, mayNVars, mayExpr, mRecSyms, mRecHints) -> do
            nVar' <- instantiateToken nVar
            withNVar nVar' sc mayNVars mayExpr mRecSyms mRecHints

{- If searches for a symbol in the typing environment. If it is found, then its type is instantiated. -}
ifExistingSym :: Raw.SymbolName With.ProgState -> ScopedContextOp (TyInf a) -> (TyInf a -> TyInf a)
ifExistingSym sn = ifExistingSymRep $ repOf sn

{- If searches for a global symbol in the typing environment. If it is found, then its type is instantiated. -}
ifExistingGlobalSym :: Raw.SymbolName With.ProgState -> ScopedContextOp (TyInf a) -> (TyInf a -> TyInf a)
ifExistingGlobalSym sn withNVar op = do
    symbols <- querySymbols $ repOf sn
    case firstThat isGlobalVar symbols of
        Nothing -> op
        Just (nVar, sc, mayNVars, mayExpr, mRecSyms, mRecHints) -> do
            nVar' <- instantiateToken nVar
            withNVar nVar' sc mayNVars mayExpr mRecSyms mRecHints
    where
        isGlobalVar (_, sc, _, _, _, _) = sc == NS.globalScope

{- If searches for a property symbol. If it is found, then its type is instantiated. -}
ifExistingSymPropMethod
    :: Raw.SymbolName With.ProgState
    -> (Ty.NotedVar With.ProgState -> TyInf a)
    -> (TyInf a -> TyInf a)
ifExistingSymPropMethod sn withNVar cont = do
    methods <- getMethods
    case kFind (repOf sn) methods of
        Nothing -> cont
        Just nVar -> do
            nVar' <- instantiateToken nVar
            withNVar nVar'

{- Wrapping in TyInf, in order not to get the types table every time. -}
kindFromLam
    :: TyInf
        (  Raw.ParamTypeName With.ProgState
        -> Raw.UnConType With.ProgState
        -> Either KInfr.TypeGenErr Ty.LangKind
        )
kindFromLam = do
    tt <- getTT
    return $
        \pty ty ->
            case KInfr.getKinds''' pty [ty] tt of
                Left err -> Left err
                Right (lk, _) -> Right lk

typeSpecCheck
    :: Ty.LangHigherType With.ProgState
    -> Ty.LangHigherType With.ProgState
    -> (  Ty.LangHigherType With.ProgState
       -> Ty.LangHigherType With.ProgState
       -> Either (Ty.UnificationError With.ProgState) (Ty.Substitution With.ProgState)
       )
    -> (Ty.UnificationError With.ProgState -> TyInf a)
    -> (Ty.Substitution With.ProgState -> TyInf a)
    -> TyInf a
typeSpecCheck ty ty' test notOk ok = do
    case test ty ty' of
        Left err -> notOk err
        Right subst -> ok subst

{- It does the type lesser check. It takes also a success callback and a callback for the not-equality case.
It can be read as: is the first type less specialized than (and then contains) the second type? -}
typeLessSpecCheck
    :: Ty.LangHigherType With.ProgState
    -> Ty.LangHigherType With.ProgState
    -> (Ty.UnificationError With.ProgState -> TyInf a)
    -> (Ty.Substitution With.ProgState -> TyInf a)
    -> TyInf a
typeLessSpecCheck ty ty' = typeSpecCheck ty ty' Ty.isLessSpecThan

{- Same of typeLessSpecCheck, but using type greater check. -}
typeMoreSpecCheck
    :: Ty.LangHigherType With.ProgState
    -> Ty.LangHigherType With.ProgState
    -> (Ty.UnificationError With.ProgState -> TyInf a)
    -> (Ty.Substitution With.ProgState -> TyInf a)
    -> TyInf a
typeMoreSpecCheck ty ty' = typeSpecCheck ty ty' Ty.isMoreSpecThan

unification
    :: Ty.LangHigherType With.ProgState
    -> Ty.LangHigherType With.ProgState
    -> (Ty.UnificationError With.ProgState -> TyInf a)
    -> (Ty.Substitution With.ProgState -> TyInf a)
    -> TyInf a
unification ty ty' onErr withSubst =
    case Ty.unify ty ty' of
        Left err -> onErr err
        Right subst -> withSubst subst

unification'
    :: Ty.LangHigherType With.ProgState
    -> Ty.LangHigherType With.ProgState
    -> TyInf (Ty.Substitution With.ProgState)
unification' ty ty' =
    unification ty ty'
        (tyInfErr . UnificationError)
        return

getTypeOf :: Ty.HasType tok => tok With.ProgState -> TyInf (Ty.LangTypeScheme With.ProgState)
getTypeOf = return . Ty.typeOf

getInstTypeOf :: Ty.HasType tok => tok With.ProgState -> TyInf (Ty.LangQualType With.ProgState)
getInstTypeOf token = do
    polyTy <- getTypeOf token
    instantiate polyTy

getInstTypeOf' :: Ty.HasType tok => tok With.ProgState -> TyInf (Ty.LangTypeScheme With.ProgState)
getInstTypeOf' token = do
    polyTy <- getTypeOf token
    instantiate' polyTy

getMonoTypeOf :: Ty.HasType tok => tok With.ProgState -> TyInf (Ty.LangHigherType With.ProgState)
getMonoTypeOf token = do
    qualTy <- getInstTypeOf token
    unqualify qualTy

getUnqualTypeOf
    :: Ty.HasType tok
    => tok With.ProgState
    -> TyInf (Ty.LangHigherType With.ProgState, [Ty.LangSpecConstraint With.ProgState])
getUnqualTypeOf token = do
    qualTy <- getInstTypeOf token
    monoTy <- unqualify qualTy
    return (monoTy, Ty.contsOf qualTy)

{- NB: the final type scheme is just lifted, it has no binder. -}
getUnqualTypeOf'
    :: Ty.HasType tok
    => tok With.ProgState
    -> TyInf (Ty.LangTypeScheme With.ProgState, [Ty.LangSpecConstraint With.ProgState])
getUnqualTypeOf' token = do
    polyTy <- getTypeOf token
    qualTy <- instantiate polyTy
    (cs, monoTy) <- splitUnqualify qualTy
    newPolyTy <- liftMonoType monoTy
    return (newPolyTy, cs)

newIndex :: TyInf String
newIndex = do
    phc <- getPhC
    let (index, phc') = Desugar.mkDispatchName phc
    putPhC phc'
    return index

getIndex :: ConstraintProblem -> TyInf String
getIndex (_, i) = return i

divideContsFrom
    :: (Ty.HasType tok, Ty.UpdateType tok)
    => tok With.ProgState
    -> TyInf (tok With.ProgState, [Ty.LangSpecConstraint With.ProgState])
divideContsFrom token = do
    (polyTy, cs) <- getUnqualTypeOf' token
    let updToken = Ty.updateType token polyTy
    return (updToken, cs)

createConstrainedType
    :: Raw.Type With.ProgState
    -> TyInf (Ty.LangQualType With.ProgState)
createConstrainedType ty = do
    tt <- getTT
    ct <- getCT
    fetchK <- kindFromLam
    case Create.aConstrainedType tt ct fetchK ty of
        Left err -> tyInfErr $ TypeBuildErr err
        Right qualTy -> return qualTy

newFreeVar :: TyInf String
newFreeVar = do
    fv <- getFV
    let (varRep, fv') = Fresh.allocFreeVar () fv
    return varRep `withFV` fv'

newFreeVar' :: Ty.LangKind -> Ty.Role -> ProgState -> TyInf (Ty.LangVarType With.ProgState)
newFreeVar' lk rl st = do
    varRep <- newFreeVar
    return $ Ty.newLVTy varRep lk rl st

newFreeVarFrom :: Ty.LangVarType With.ProgState -> TyInf (Ty.LangVarType With.ProgState)
newFreeVarFrom lvty = do
    let lk = Ty.kindOf lvty
    let rl = Ty.roleOf lvty
    let st = stateOf lvty
    newFreeVar' lk rl st

{- It binds a new *-kind type variable and it updates also the state with the new container keeping
the new type variable. -}
newMonoTypeFreeVar :: ProgState -> TyInf (Ty.LangHigherType With.ProgState)
newMonoTypeFreeVar st = do
    tyVar <- newFreeVar' Ty.LKConst Ty.Representational st
    return $ Ty.newLHTyFromLVTy tyVar

newMonoTypeFreeVar' :: ProgState -> TyInf (Ty.LangTypeScheme With.ProgState)
newMonoTypeFreeVar' st = do
    monoTy <- newMonoTypeFreeVar st
    liftMonoType monoTy

newMonoTypeFreeVar'' :: ProgState -> TyInf (Ty.LangQualType With.ProgState)
newMonoTypeFreeVar'' st = do
    monoTy <- newMonoTypeFreeVar st
    return $ Ty.newQualType [] monoTy

mostGenNVar :: String -> ProgState -> TyInf (Ty.NotedVar With.ProgState)
mostGenNVar symRep st = do
    monoTy <- newMonoTypeFreeVar' st
    return $ Ty.newNotedVar symRep monoTy st

instantiateToken
    :: (Ty.HasType tok, Ty.UpdateType tok)
    => tok With.ProgState
    -> TyInf (tok With.ProgState)
instantiateToken token = do
    qualTy <- getInstTypeOf' token
    return $ Ty.updateType token qualTy

{- NB: this is generalization rule, but it is terribly UNSAFE because it does not check for free type variables in
the typing environment. Since it does not perform the check on typing environment, it is much faster than safe
generalization. This should be used only when it is granted free type variables in the input type appear only in
the input type. -}
unsafeGeneralize :: Ty.LangHigherType With.ProgState -> TyInf (Ty.LangTypeScheme With.ProgState)
unsafeGeneralize = return . Ty.generalize

unsafeGeneralize' :: Ty.LangQualType With.ProgState -> TyInf (Ty.LangTypeScheme With.ProgState)
unsafeGeneralize' = return . Ty.generalize'

type DispatchConstraint = ConstraintProblem

unsafeGeneralizeAndImply
    :: Ty.LangHigherType With.ProgState
    -> TyInf ([ConstraintProblem], [DispatchConstraint], Ty.LangTypeScheme With.ProgState)
unsafeGeneralizeAndImply ty = do
    cs <- getCurConts
    return $ Ty.generalizeAndTryImply cs ty []

unsafeGeneralizeToken
    :: (Ty.HasType tok, Ty.UpdateType tok)
    => tok With.ProgState
    -> TyInf (tok With.ProgState)
unsafeGeneralizeToken token = do
    ty <- getTypeOf token
    let polyTy = Ty.generalize'' ty
    return $ Ty.updateType token polyTy

{- It performs unsafe generalization and it tries to "inject" the implication with current predicates in the final
type scheme, excluding ones which have to be normalized. The returned constraints have to be normalized. -}
unsafeGeneralizeTokenAndImply
    :: (Ty.HasType tok, Ty.UpdateType tok)
    => tok With.ProgState
    -> TyInf ([ConstraintProblem], [DispatchConstraint], tok With.ProgState)
unsafeGeneralizeTokenAndImply token = do
    ty <- getMonoTypeOf token
    (cs, dispCs, updTy) <- unsafeGeneralizeAndImply ty
    return (cs, dispCs, Ty.updateType token updTy)

{- Given `getFreeVars symRep sc`, it finds all free type variables in the current typing environment, excluding
ones which are out of the input scope `sc` and ones of symbols matching both string representation `symRep` and
scope `sc`.
NB: as the docs tells, the check on out-of-scope symbols is done on `sc`, not on the current scope. -}
getFreeVars :: String -> TyInf [Ty.LangVarType With.ProgState]
getFreeVars symRep = do
    maysym <- getSymbol symRep
    case maysym of
        Nothing -> doWithCurTyEnvBindings getFreeVarsFrom
        Just (_, sc, _, _, _, _) -> doWithCurTyEnvBindings $ getFreeVarsExcl sc
    where
        getFreeVarsExcl _ [] = return []
        getFreeVarsExcl sc ((nVar', sc', _, _, _, _) : bt) = do
            {- 1) First condition to exclude type variables of out-of-scope symbols.
               2) Second condition to exclude type variables of the input noted variable. -}
            if sc < sc' || (symRep == repOf nVar' && sc == sc')
            then getFreeVarsExcl sc bt
            else do
                polyTy <- getTypeOf nVar'
                let fVars = Ty.fVarsOf polyTy
                fVars' <- getFreeVarsExcl sc bt
                return $ L.union fVars fVars'

        getFreeVarsFrom [] = return []
        getFreeVarsFrom ((nVar', _, _, _, _, _) : bt) = do
            polyTy <- getTypeOf nVar'
            let fVars = Ty.fVarsOf polyTy
            fVars' <- getFreeVarsFrom bt
            return $ L.union fVars fVars'

{- Safe (but very less efficient) version of generalization. -}
generalize :: Ty.NotedVar With.ProgState -> TyInf (Ty.NotedVar With.ProgState)
generalize nVar = do
    let symRep = repOf nVar
    {- Gathering all free variables in the current typing environment, excluding ones which are part of the
    input noted variable. -}
    teFreeVars <- getFreeVars symRep
    polyTy <- getTypeOf nVar
    let polyTy' = polyTy `Ty.generalizeExcluding''` teFreeVars
    return $ Ty.updateType nVar polyTy'

{- It performs safe generalization and it tries to "inject" the implication with current predicates in the final
type scheme, excluding ones which have to be normalized. The returned constraints have to be normalized. -}
generalizeAndImply
    :: Ty.NotedVar With.ProgState
    -> TyInf ([ConstraintProblem], [DispatchConstraint], Ty.NotedVar With.ProgState)
generalizeAndImply nVar = do
    let symRep = repOf nVar
    {- Gathering all free variables in the current typing environment, excluding ones which are part of the
    input noted variable. -}
    teFreeVars <- getFreeVars symRep
    monoTy <- getMonoTypeOf nVar
    curCs <- getCurConts
    let (cs, dispCs, polyTy) = Ty.generalizeAndTryImply curCs monoTy teFreeVars
    return (cs, dispCs, Ty.updateType nVar polyTy)

liftMonoType :: Ty.LangHigherType With.ProgState -> TyInf (Ty.LangTypeScheme With.ProgState)
liftMonoType = return . Ty.liftMonoType

liftQualType :: Ty.LangQualType With.ProgState -> TyInf (Ty.LangTypeScheme With.ProgState)
liftQualType = return . Ty.liftQualType

replaceVar :: Ty.LangVarType With.ProgState -> TyInf (Ty.LangVarType With.ProgState, Ty.LangVarType With.ProgState)
replaceVar tyVar = do
    newTyVar <- newFreeVarFrom tyVar
    return (tyVar, newTyVar)

instantiate :: Ty.LangTypeScheme With.ProgState -> TyInf (Ty.LangQualType With.ProgState)
instantiate polyTy = do
    let boundVars = Ty.bVarsOf polyTy
    replVars <- mapM replaceVar boundVars
    return $ Ty.instantiate polyTy replVars

instantiateUnqualifying :: Ty.LangTypeScheme With.ProgState -> TyInf (Ty.LangHigherType With.ProgState)
instantiateUnqualifying polyTy = do
    let boundVars = Ty.bVarsOf polyTy
    replVars <- mapM replaceVar boundVars
    case Ty.instantiateUnqualifying polyTy replVars of
        Nothing -> tyInfErr . TypeNotInfrd $ stateOf polyTy
        Just lhty -> return lhty

{- The same of `instantiate`, but returning a LangTypeScheme value. The latter has the same form (namely with no bound
variables) of the LangQualType value returned by `instantiate`. -}
instantiate' :: Ty.LangTypeScheme With.ProgState -> TyInf (Ty.LangTypeScheme With.ProgState)
instantiate' polyTy = do
    qualTy <- instantiate polyTy
    liftQualType qualTy

unqualify :: Ty.LangQualType With.ProgState -> TyInf (Ty.LangHigherType With.ProgState)
unqualify qualTy = do
    case Ty.unconstrainedTypeOf qualTy of
        Nothing -> tyInfErr . TypeNotInfrd $ stateOf qualTy
        Just lhty -> return lhty

splitUnqualify
    :: Ty.LangQualType With.ProgState
    -> TyInf ([Ty.LangSpecConstraint With.ProgState], Ty.LangHigherType With.ProgState)
splitUnqualify qualTy = do
    case Ty.splitQualType qualTy of
        (_, Nothing) -> tyInfErr . TypeNotInfrd $ stateOf qualTy
        (cs, Just lhty) -> return (cs, lhty)

{- It computes an action if a token has type-hinting, else it returns a continuation. -}
ifHintIn
    :: Raw.HasHint tok
    => tok With.ProgState
    -> (Raw.Type With.ProgState -> TyInf a)
    -> (TyInf a -> TyInf a)
ifHintIn token f =
    Raw.ifHint' (Raw.hintOf token) (const . f) id

ifHintIn'
    :: Raw.HasHint tok
    => tok With.ProgState
    -> (Ty.LangTypeScheme With.ProgState -> TyInf a)
    -> (TyInf a -> TyInf a)
ifHintIn' token f cont =
    ifHintIn token
        (\rawTy -> do
            qualTy <- createConstrainedType rawTy
            polyTy <- unsafeGeneralize' qualTy
            f polyTy
        ) `elseInfer` cont

ifHintIn''
    :: Raw.HasHint tok
    => tok With.ProgState
    -> (Ty.LangQualType With.ProgState -> TyInf a)
    -> (TyInf a -> TyInf a)
ifHintIn'' token f cont =
    ifHintIn token
        (\rawTy -> do
            qualTy <- createConstrainedType rawTy
            f qualTy
        ) `elseInfer` cont

ifHintInSd
    :: Raw.SDUnion With.ProgState
    -> (Ty.NotedVar With.ProgState -> TyInf a)
    -> (TyInf a -> TyInf a)
ifHintInSd sd withNVar cont =
    case sd of
        Raw.SD sd -> lookupHint sd
        Raw.MSD msd -> lookupHint msd
    where
        lookupHint token =
            ifHintIn' token
                (\polyTy -> do
                    let sym = headOf token
                    let nVar = Ty.newNotedVar (repOf sym) polyTy $ stateOf sym
                    withNVar nVar
                ) `elseInfer` cont

ifHintInSd'
    :: Raw.SDUnion With.ProgState
    -> (  Ty.NotedVar With.ProgState
       -> Ty.LangHigherType With.ProgState
       -> [Ty.LangSpecConstraint With.ProgState]
       -> TyInf a
       )
    -> (TyInf a -> TyInf a)
ifHintInSd' sd withNVar cont =
    ifHintInSd sd
        (\nVar -> do
            qualTy <- getInstTypeOf nVar
            (cs, monoTy) <- splitUnqualify qualTy
            withNVar nVar monoTy cs
        ) `elseInfer` do
            cont

ifTypeHinting :: [QualTypeHinting] -> (Ty.LangQualType With.ProgState -> TyInf a) -> (TyInf a -> TyInf a)
ifTypeHinting [] _ cont = cont
ifTypeHinting [th] withMonoTy _ = withMonoTy th
ifTypeHinting (th : th1 : t) withMonoTy cont =
    if th == th1
    then ifTypeHinting (th : t) withMonoTy cont
    else tyInfErr . UnexpType th th1 $ stateOf th1

ifTypeHinting' :: [QualTypeHinting] -> (Ty.LangTypeScheme With.ProgState -> TyInf a) -> (TyInf a -> TyInf a)
ifTypeHinting' hts withMonoTy = ifTypeHinting hts builtF
    where
        builtF qualTy = do
            qualTy' <- liftQualType qualTy
            withMonoTy qualTy'

ifTypeHintingUnqual
    :: [QualTypeHinting]
    -> (Ty.LangHigherType With.ProgState -> [Ty.LangSpecConstraint With.ProgState] -> TyInf a)
    -> (TyInf a -> TyInf a)
ifTypeHintingUnqual hts withMonoTy cont =
    ifTypeHinting hts
        (\qualTy -> do
            (cs, monoTy) <- splitUnqualify qualTy
            withMonoTy monoTy cs
        ) `elseInfer`
            cont

ifExistTypeHinting :: Maybe QualTypeHinting -> (Ty.LangQualType With.ProgState -> TyInf a) -> (TyInf a -> TyInf a)
ifExistTypeHinting Nothing = ifTypeHinting []
ifExistTypeHinting (Just qualTy) = ifTypeHinting [qualTy]

ifExistTypeHintingUnqual
    :: Maybe QualTypeHinting
    -> (Ty.LangHigherType With.ProgState -> [Ty.LangSpecConstraint With.ProgState] -> TyInf a)
    -> (TyInf a -> TyInf a)
ifExistTypeHintingUnqual Nothing = ifTypeHintingUnqual []
ifExistTypeHintingUnqual (Just qualTy) = ifTypeHintingUnqual [qualTy]

thenInfer :: (a -> b) -> a -> b
thenInfer = ($)

{- Useful to make more readable code by exploiting continuations. -}
elseInfer :: (TyInf a -> TyInf a) -> TyInf a -> TyInf a
elseInfer = ($)

expecting :: Maybe ExpectedType -> [ExpectedType]
expecting Nothing = []
expecting (Just ty) = [ty]

{- The combination between getInstTypeOf and typeLessSpecCheck. -}
tokenLessSpecCheck
    :: Ty.HasType tok
    => tok With.ProgState
    -> Ty.LangHigherType With.ProgState
    -> (Ty.UnificationError With.ProgState -> TyInf a)
    -> (Ty.Substitution With.ProgState -> TyInf a)
    -> TyInf a
tokenLessSpecCheck token ty notOk ok = do
    tokenTy <- getMonoTypeOf token
    typeLessSpecCheck tokenTy ty notOk ok

{- Same of tokenLessSpecCheck, but using typeMoreSpecCheck. -}
tokenMoreSpecCheck
    :: Ty.HasType tok
    => tok With.ProgState
    -> Ty.LangHigherType With.ProgState
    -> (Ty.UnificationError With.ProgState -> TyInf a)
    -> (Ty.Substitution With.ProgState -> TyInf a)
    -> TyInf a
tokenMoreSpecCheck token ty notOk ok = do
    tokenTy <- getMonoTypeOf token
    typeMoreSpecCheck tokenTy ty notOk ok

typeMoreSpec
    :: Ty.LangHigherType With.ProgState
    -> Ty.LangHigherType With.ProgState
    -> TyInf (Ty.Substitution With.ProgState)
typeMoreSpec monoTy monoTy1 =
    typeMoreSpecCheck monoTy monoTy1
        unificationErr
        return

typeLessSpec
    :: Ty.LangHigherType With.ProgState
    -> Ty.LangHigherType With.ProgState
    -> TyInf (Ty.Substitution With.ProgState)
typeLessSpec monoTy monoTy1 =
    typeLessSpecCheck monoTy monoTy1
        unificationErr
        return

tokenMoreSpec
    :: Ty.HasType tok
    => tok With.ProgState
    -> Ty.LangHigherType With.ProgState
    -> TyInf (Ty.Substitution With.ProgState)
tokenMoreSpec token monoTy =
    tokenMoreSpecCheck token monoTy
        unificationErr
        return

tokenLessSpec
    :: Ty.HasType tok
    => tok With.ProgState
    -> Ty.LangHigherType With.ProgState
    -> TyInf (Ty.Substitution With.ProgState)
tokenLessSpec token monoTy =
    tokenLessSpecCheck token monoTy
        unificationErr
        return

{- It returns a True value if the constraint evaluates to true (namely it has no type variables inside and there is
an instance which satisfy it), False if the constraint has type variables (which does not mean it evaluates to true),
an error if the constraint evaluates to false or the associated property is not found.
NB: the boolean return value denotes the presence of type variables in the constraint, not if the constraint evaluates
to true. -}
checkTrueConstraint :: Ty.LangSpecConstraint With.ProgState -> TyInf Bool
checkTrueConstraint lspc =
    if not . L.null $ Ty.tyVarsOf lspc
    then return False
    else do
        it <- getIT
        let propRep = repOf $ headOf lspc
        case kFind propRep it :: Maybe [Ty.LangSpecConstraint With.ProgState] of
            Nothing -> tyInfErr $ PropNotFound propRep
            Just cs ->
                if lspc `elem` cs
                then return True
                else tyInfErr $ NoMatchingInst lspc

rmTrueConstraints :: [Ty.LangSpecConstraint With.ProgState] -> TyInf [Ty.LangSpecConstraint With.ProgState]
rmTrueConstraints = filterM constraintWithTyVars
    where
        constraintWithTyVars = notM . checkTrueConstraint

contsSatisfiabilityContext :: TyInf [Ty.LangSpecConstraint With.ProgState]
contsSatisfiabilityContext = getAllElems <$> getIT

{- It normalizes a predicate. -}
normalization :: Ty.LangSpecConstraint With.ProgState -> TyInf (Ty.LangSpecConstraint With.ProgState)
normalization c = do
    satCs <- contsSatisfiabilityContext
    case c `Ty.normalize` satCs of
        Right c' -> return c'
        Left err -> tyInfErr $ SatisfiabilityError err

{- Given an constructor name, it searches it in the constructors table and if it finds a typed constructor
in the table, it instantiates it. If it does not find anything in the table, it returns an error. -}
getConstructor
    :: Raw.ADTConName With.ProgState
    -> TyInf (Ty.NotedVal With.ProgState)
getConstructor con = do
    cons <- getCons
    case kFind (repOf con) cons of
        Nothing -> tyInfErr $ ConNotFound con
        {- Instantiating the constructor before returning it. -}
        Just datacon -> instantiateToken datacon

mkConstraints
    :: Ty.LangNewConstraint With.ProgState
    -> [Ty.LangSpecConstraint With.ProgState]
    -> [Ty.LangHigherType With.ProgState]
    -> ProgState
    -> TyInf [Ty.LangSpecConstraint With.ProgState]
mkConstraints lnc cs lhts st =
    case Ty.newLSpCont lnc lhts st of
        Right (lspc, subst) -> do
            let lam = Ty.specLambda subst
            let specCs = map (`Ty.specTypeWith` lam) cs
            {- Removing constraints which evaluates to true. -}
            {- TODO: remove this and do the right check. -}
            tyVarsCs <- rmTrueConstraints specCs
            return $ lspc : tyVarsCs
        Left err -> tyInfErr $ ContMakingErr err

{- It creates a new type which should represent the type of a literal. The type is a constrained type variable. -}
mkLitTyVar :: String -> ProgState -> TyInf (Ty.LangQualType With.ProgState)
mkLitTyVar propName st = do
    ct <- getCT
    case kFind propName ct of
        Nothing -> tyInfErr $ PropNotFound propName
        Just (lnc, cs) -> do
            {- Taking the first (and possibly last) type variable to use as a "model" (look at `newFreeVarFrom`) to
            build the free variable. -}
            tyVar <- getHeadVar lnc
            freeVar <- newFreeVarFrom tyVar
            let lhty = Ty.newLHTyFromLVTy freeVar
            cs' <- mkConstraints lnc cs [lhty] st
            return $ Ty.newQualType cs' lhty
    where
        getHeadVar
            :: Ty.LangNewConstraint With.ProgState
            -> TyInf (Ty.LangVarType With.ProgState)
        getHeadVar lnc =
            case head' $ argsOf lnc of
                Nothing -> tyInfErr $ ZeroaryProperty lnc
                Just v -> return v

mkTypeFromLNTy :: Ty.LangNewType With.ProgState -> TyInf (Ty.LangQualType With.ProgState)
mkTypeFromLNTy lnty = do
    let oldArgs = argsOf lnty
    args <- mapM newFreeVarFrom oldArgs
    let args' = map Ty.newLHTyFromLVTy args
    case Ty.newLowLHTy lnty args' $ stateOf lnty of
        Nothing -> tyInfErr $ GenTokCreationErr "could not create new mono type"
        Just lhty -> return $ Ty.newQualType [] lhty

{- It creates a new type which should represent the type of a literal. The type is a concrete type (not a type
variable, like the one of mkLitTyVar). -}
mkLitConcrete :: String -> TyInf (Ty.LangQualType With.ProgState)
mkLitConcrete tyName = do
    tt <- getTT
    case kFind tyName tt of
        Nothing -> tyInfErr $ TypeNotFound tyName
        Just lnty -> mkTypeFromLNTy lnty

{- TODO: the implementation of literals could change in the future. It could depend from code generation. -}
mkLiteral :: Raw.Literal With.ProgState -> TyInf (Ty.NotedVal With.ProgState)
mkLiteral (Raw.IntLit (i, st)) = do
    ty <- mkLitTyVar BIProps.nameNum st
    ty' <- liftQualType ty
    {-TODO:
        problem 1: Int value from Integer can cause overflow.
        problem 2: This should not be the natural literal, change it. -}
    return $ Ty.newNotedIntLit (fromInteger i) ty' st
mkLiteral (Raw.DoubleLit (fl, st)) = do
    ty <- mkLitTyVar BIProps.nameFract st
    ty' <- liftQualType ty
    return $ Ty.newNotedDoubleLit fl ty' st
mkLiteral (Raw.CharLit (c, st)) = do
    ty <- mkLitConcrete BITy.nameChar
    ty' <- liftQualType ty
    return $ Ty.newNotedCharLit c ty' st
mkLiteral (Raw.StringLit (str, st)) = do
    ty <- mkLitConcrete BITy.nameByteString
    ty' <- liftQualType ty
    return $ Ty.newNotedStringLit str ty' st

inferLiteral
    :: Raw.Literal With.ProgState
    -> [QualTypeHinting]
    -> TyInf (Ty.NotedVal With.ProgState)
inferLiteral lit hts = do
    nVal <- mkLiteral lit
    ifTypeHintingUnqual hts
        (\monoTy cs -> do
            {- The unification, here, has the only goal of type-checking, because the noted value of the literal
            has a free type variable which does not appear in the typing environment. So performing the substitution
            would be a useless operation, even if it would be right. -}
            tokenLessSpec nVal monoTy
            polyTy <- unsafeGeneralize monoTy
            let nVal' = Ty.updateType nVal polyTy
            {- Handling constraint problems. -}
            cps <- mapM mkProblem cs
            addCurConts cps
            return nVal'
        ) `elseInfer`
            return nVal

inferMatchConApp
    :: [QualTypeHinting]
    -> Raw.ADTAppMatchExpression With.ProgState
    -> ProgState
    -> TyInf (Ty.NotedMatchExpr With.ProgState)
inferMatchConApp hts (Raw.ADTAppMExpr (cn, ms, ast)) st = do
    dataCon <- getConstructor cn
    {- The situation is like this:

        dataCon's type:
            t1 -> ... -> tn -> resTy

        matching expressions:
            m1 ... mn
    -}
    if Ty.nValArgsNumber dataCon /= length ms
    then tyInfErr $ ArgsConErr cn
    else do
        ifTypeHintingUnqual hts
            (\monoTy cs -> do
                (pairsMs, resTy) <- diffTokensType "matching expression" st ms monoTy
                let pairsMs' = map (\(tok, lhty) -> (tok, Ty.addContsToLHTy cs lhty)) pairsMs
                nms <- mapM inferMatchWithHint pairsMs'
                conTy <- getMonoTypeOf dataCon
                {- After unfolding:
                    t1 -> ... -> tn -> resTy
                -}
                let conTs = Ty.unfoldType conTy
                dataConResTy <- returnType conTs
                {- Zipping operation will cut off return type, indeed:
                    nme1 ... nmen
                     |    |   |
                     V    V   V
                     t1  ...  tn  dataConResTy

                typeCheckMatchExprs performs type check to the elements resulting from the zip operation, one by one.
                This is done because the data constructor sub-types have to match, one by one, with sub-types coming
                from type-hinting.
                -}
                (updDataConResTy, updDataCon) <- typeCheckMatchExprs (zip nms conTs) dataConResTy dataCon
                {- Checking also return type, after substitution application. -}
                subst <- typeMoreSpec resTy updDataConResTy
                {- Applying substitution coming from return type check. -}
                updDataCon1 <- updDataCon `specToken` [subst]
                polyTy <- unsafeGeneralize monoTy
                return $ Ty.newCompNotedMExpr updDataCon1 nms polyTy ast
            ) `elseInfer` do
                nms <- mapM (inferMatchExpr []) ms
                conTy <- getMonoTypeOf dataCon
                (pairsNms, resTy) <- diffTokensType "matching expression" st nms conTy
                (nms1, resTy1, dataCon1) <- mguMatchExprs pairsNms [] resTy dataCon
                liftedResTy <- liftMonoType resTy1
                return $ Ty.newCompNotedMExpr dataCon1 nms1 liftedResTy ast
    where
        returnType ts =
            case last' ts of
                Nothing -> tyInfErr $ GenUnreachableState "No types in the list after unfolding"
                Just resTy -> return resTy

        inferMatchWithHint (me, qualTy) = inferMatchExpr [qualTy] me

        typeCheckMatchExprs [] resTy dataCon = return (resTy, dataCon)
        typeCheckMatchExprs ((nme, conSubTy) : t) resTy dataCon = do
            subst <- tokenMoreSpec nme conSubTy
            let subst' = [subst]
            {- Applying substitution to:
                - dataCon's supposed return type (`resTy`);
                - dataCon (`dataCon`);
                - dataCon's sub-types to check (`t`); -}
            updResTy <- resTy `specToken` subst'
            updDataCon <- dataCon `specToken` subst'
            t1 <- mapM (`specSecond` subst') t
            typeCheckMatchExprs t1 updResTy updDataCon

        mguMatchExprs [] nms resTy dataCon = return (nms, resTy, dataCon)
        mguMatchExprs ((nme, ty) : t) nms resTy dataCon = do
            nmeTy <- getMonoTypeOf nme
            {- Calculating the most general unifier. -}
            subst <- unification' nmeTy ty
            let subst' = [subst]
            {- Applying substitution to:
                - currently evaluated matching expression (`nme`);
                - previously evaluated matching expressions (`nms`);
                - matching expressions to evaluate (`t`);
                - return type (`resTy`);
                - data constructor (`dataCon`);
                - typing environment. -}
            nme1 <- nme `specToken` subst'
            nms1 <- mapM (`specToken` subst') nms
            t1 <- mapM (`specPair` subst') t
            resTy1 <- resTy `specToken` subst'
            dataCon1 <- dataCon `specToken` subst'
            specCurContext subst'
            mguMatchExprs t1 (nms1 ++ [nme1]) resTy1 dataCon1

        specPair (nme, ty) substs = do
            nme1 <- nme `specToken` substs
            ty1 <- ty `specToken` substs
            return (nme1, ty1)

        specSecond (nme, ty) substs = do
            ty1 <- ty `specToken` substs
            return (nme, ty1)

inferUAMatchExpr
    :: [QualTypeHinting]
    -> Raw.UnAltMatchingExpression With.ProgState
    -> ProgState
    -> TyInf (Ty.NotedMatchExpr With.ProgState)
inferUAMatchExpr hts (Raw.MDefault dst) _ = do
    ifTypeHinting' hts
        (\polyTy ->
            return $ Ty.newDefNotedMExpr polyTy dst
        ) `elseInfer` do
            polyTy <- newMonoTypeFreeVar' dst
            return $ Ty.newDefNotedMExpr polyTy dst
inferUAMatchExpr hts (Raw.MBase sn) st = do
    ifTypeHinting' hts
        `thenInfer`
            addSymbolAndMkMExpr
        `elseInfer` do
            polyTy <- newMonoTypeFreeVar' $ stateOf sn
            addSymbolAndMkMExpr polyTy
    where
        addSymbolAndMkMExpr polyTy = do
            let nVar = Ty.newNotedVar (repOf sn) polyTy $ stateOf sn
            putSymbolNoExpr nVar []
            return $ Ty.newVarNotedMExpr nVar st
inferUAMatchExpr hts (Raw.MADTBase cn) st = do
    dataCon <- getConstructor cn
    ifTypeHintingUnqual hts
        (\monoTy cs -> do --FIXME
            conTy <- getMonoTypeOf dataCon
            {- Just a type-check, not performing the substitution is safe. -}
            typeMoreSpec monoTy conTy
            polyTy <- unsafeGeneralize monoTy
            let updDataCon = Ty.updateType dataCon polyTy
            return $ Ty.newValNotedMExpr updDataCon st
        ) `elseInfer` do
            return $ Ty.newValNotedMExpr dataCon st
inferUAMatchExpr hts (Raw.MLit lit) st = do
    nVal <- inferLiteral lit hts
    return $ Ty.newValNotedMExpr nVal st
inferUAMatchExpr hts (Raw.MADTApp conApp) st =
    inferMatchConApp hts conApp st

inferMatchExpr
    :: [QualTypeHinting]
    -> Raw.MatchingExpression With.ProgState
    -> TyInf (Ty.NotedMatchExpr With.ProgState)
inferMatchExpr hts (Raw.MatchExpr uame st) = inferUAMatchExpr hts uame st

{- It tries to split a type into at least n pieces, where n is the length of a list of generic tokens. If it does,
it returns a list of generic tokens with an associated type and the rest of type which has not been associated with
tokens. -}
diffTokensType
    {- The first two parameters are just for error handling -}
    :: String
    -> ProgState
    -> [tok]
    -> Ty.LangHigherType With.ProgState
    -> TyInf
        ( [(tok, Ty.LangHigherType With.ProgState)]
        , Ty.LangHigherType With.ProgState
        )
diffTokensType tokDesc tokSt tokens ty = do
    {- Splitting with unfoldType. -}
    let ts = Ty.unfoldType ty
    (eTokens, resTs) <- splitTypes ty ts
    return (eTokens, sconcat resTs)
    where
        splitTypes expTy ts = do
            let argsNo = length tokens
            if length ts - 1 < argsNo
            then tyInfErr $ TooManyArgs tokDesc tokSt $ Just expTy
            else case drop argsNo ts of
                {- The first case should NEVER be evaluated, because the non-emptiness is granted by the
                if-condition. It is handled anyway, in order not to have a partial function. -}
                [] -> tyInfErr $ TooManyArgs tokDesc tokSt $ Just expTy
                (ty : t) -> return (zip tokens ts, ty :| t)

diffTokensQualType
    {- The first two parameters are just for error handling -}
    :: String
    -> ProgState
    -> [tok]
    -> Ty.LangQualType With.ProgState
    -> TyInf
        ( [(tok, Ty.LangQualType With.ProgState)]
        , Ty.LangQualType With.ProgState
        )
diffTokensQualType tokDesc tokSt tokens qualTy = do
    (cs, monoTy) <- splitUnqualify qualTy
    (eTokens, resTy) <- diffTokensType tokDesc tokSt tokens monoTy
    let eTokens' = map (\(tok, lhty) -> (tok, Ty.addContsToLHTy cs lhty)) eTokens
    let resTy' = Ty.addContsToLHTy cs resTy
    return (eTokens', resTy')

{- It checks that type-hinting has minimal constraints which satisfy ones from the current context. It also divides
constraints which are both in the context and in the type-hinting from constraints which are only present in type-hinting. -}
checkMinimalConstraints
    :: [Ty.LangSpecConstraint With.ProgState]
    -> [Ty.LangSpecConstraint With.ProgState]
    -> TyInf ([Ty.LangSpecConstraint With.ProgState], [Ty.LangSpecConstraint With.ProgState])
checkMinimalConstraints [] thcs =
    return ([], thcs)
checkMinimalConstraints origcs thcs = do
    let ixOrigcs = snd $ L.foldl' (\(ix, ixcs) c -> (ix + 1, (ix, c) : ixcs)) (0 :: Integer, []) origcs
    let ixOrigcs' = L.sortBy moreArgsFirst ixOrigcs
    sats <- allMatching [] ixOrigcs' thcs
    let notSats = L.filter (not . (`contElem` sats)) thcs
    {- Granting the original order of original constraints. -}
    let sats' = L.sortBy smallestIndex sats
    return (map snd sats', notSats)
    where
        allMatching sats ((ix, origC) : t) thcs' =
            case takeTheMatchingOne origC ix thcs' sats of
                Nothing -> tyInfErr $ NoMatchingInst origC --TODO: this error is wrong
                Just (mayhC, sats', substs) -> do
                    t' <- mapM (`specIxCont` substs) t
                    case mayhC of
                        Nothing -> allMatching sats' t' thcs'
                        Just hC -> allMatching sats' t' $ L.delete hC thcs'
        allMatching sats [] _ = return sats

        specIxCont (ix, c) substs = do
            c' <- c `specToken` substs
            return (ix, c')

        moreArgsFirst (_, c) (_, c1) = length (argsOf c) `compare` length (argsOf c1)

        smallestIndex (ix, _) (ix1, _) = ix `compare` ix1

        takeTheMatchingOne _ _ [] _ = Nothing
        takeTheMatchingOne origC ix (hC : t) sats =
            case Ty.exactlyTheSameConstraints origC hC $ Right () of
                Nothing -> takeTheMatchingOne origC ix t sats
                Just (Left _) -> takeTheMatchingOne origC ix t sats
                Just (Right subst) ->
                    if hC `contElem` sats
                    then Just (Nothing, sats, subst)
                    else Just (Just hC, (ix, hC) : sats, subst)

        contElem c ixCs = any (\(_, c') -> c == c') ixCs

mkDispatchVar
    :: [Ty.LangVarType With.ProgState]
    -> ProgState
    -> ConstraintProblem
    -> TyInf (Ty.NotedVar With.ProgState)
mkDispatchVar binders nVarSt (c, symRep) =
    return $ Ty.newDispatchNotedVar symRep (Ty.newConstraintScheme binders c nVarSt) nVarSt

mkProblem :: Ty.LangSpecConstraint With.ProgState -> TyInf ConstraintProblem
mkProblem c = do
    i <- newIndex
    return (c, i)

inferExprSymbol
    :: [QualTypeHinting]
    -> Raw.SymbolName With.ProgState
    -> ProgState
    -> TyInf (Ty.NotedExpr With.ProgState, [Ty.Substitution With.ProgState])
inferExprSymbol hts sn st = do
    {- ifExistingSym does the instantiation. -}
    ifExistingSym sn
        (\nVar sc _ _ _ _ ->
            checkTyHintAndMkExpr sc nVar
        ) `elseInfer` do
            {- If the symbol is not present in the typing environment, then it has to be a property method. -}
            ifExistingSymPropMethod sn
                `thenInfer`
                    {- Property method has global scope. -}
                    checkTyHintAndMkExpr NS.globalScope
                `elseInfer` do
                    tyInfErr . SymNotFound $ repOf sn
    where
        checkTyHintAndMkExpr sc nVar = do
            let symRep = repOf sn
            ifTypeHintingUnqual hts
                (\monoTy cs -> do
                    {- Unification due to type hinting. -}
                    subst <- tokenLessSpec nVar monoTy
                    polyTy <- unsafeGeneralize monoTy
                    {- Fetching constraints from existing symbol. -}
                    (updNVar, origCs) <- divideContsFrom nVar
                    {- Splitting constraints which are required by the symbol from the constraints which are not
                    required by the symbol type, but are in the type-hinting. -}
                    nExpr <-
                        if L.null cs && L.null origCs
                        then do
                            --TODO: should be a mono-type instead of a poly-type
                            let nVar' = Ty.updateType updNVar polyTy
                            pure $ Ty.newVarNotedExpr nVar' st
                        else do
                            {- Constraints required by symbol have to be present in type-hinting. -}
                            (satCs, _) <- checkMinimalConstraints origCs cs
                            {- These are built just for dispatch tokens. -}
                            satCps <- mapM mkProblem satCs
                            cps <- mapM mkProblem cs
                            {- Updating constraints problems. -}
                            addCurConts cps
                            {- Adding the original (and updated according to type-hinting) constraints to type of
                            noted variable. -}
                            --TODO: should be a mono-type instead of a poly-type
                            let nVar' = Ty.updateType updNVar $ Ty.addContsToLPTy satCs polyTy
                            polyTy' <- getTypeOf nVar'
                            dispExpr polyTy' (stateOf nVar') nVar' satCps
                    {- If the symbol is mutually recursive (local or global), then the type-hinting has to be
                    collected in order to check it later. -}
                    ifMutRecSym symRep
                        `thenInfer` do
                            {- Collecting type-hinting. -}
                            addTypeHintToRecSym symRep sc monoTy
                        `elseInfer`
                            doNothing'
                    return (nExpr, [subst])
                ) `elseInfer` do
                    (updNVar, cs) <- divideContsFrom nVar
                    if L.null cs
                    then return (Ty.newVarNotedExpr updNVar st, [])
                    else do
                        cps <- mapM mkProblem cs
                        addCurConts cps
                        polyTy <- getTypeOf updNVar
                        dispNe <- dispExpr polyTy (stateOf updNVar) updNVar cps
                        return (dispNe, [])

        dispExpr polyTy nVarsSt nVar cps = do
            let binders = Ty.bVarsOf polyTy
            dispVars <- mapM (mkDispatchVar binders nVarsSt) cps
            return $ Ty.newVarsDispatchNotedExpr nVar dispVars nVarsSt

inferExprDataCon
    :: [QualTypeHinting]
    -> Raw.ADTConName With.ProgState
    -> ProgState
    -> TyInf (Ty.NotedExpr With.ProgState, [Ty.Substitution With.ProgState])
inferExprDataCon hts cn st = do
    nVal <- getConstructor cn
    ifTypeHintingUnqual hts
        (\monoTy cs -> do
            tokenLessSpec nVal monoTy
            polyTy <- unsafeGeneralize monoTy
            let nVal' = Ty.updateType nVal polyTy
            {- Handling constraint problems. -}
            cps <- mapM mkProblem cs
            addCurConts cps
            {- Note that there is no returned substitution and this is safe because data constructors are not into
            the typing environment, so its free type variables are all "isolated" (they cannot appear in other tokens).
            Returning the substitution would be right, but it would be also a useless operation which would waste time.
            The unification above is present only as type-checking. -}
            return (Ty.newValNotedExpr nVal' st, [])
        )
        `elseInfer`
            return (Ty.newValNotedExpr nVal st, [])

mkNestedLams
    :: [Ty.NotedVar With.ProgState]
    -> Ty.NotedExpr With.ProgState
    -> ProgState
    -> TyInf (Ty.NotedExpr With.ProgState)
mkNestedLams [] ne _ = return ne
mkNestedLams (arg : t) ne st = do
    ne' <- mkNestedLams t ne st
    argTy <- getTypeOf arg
    exprTy <- getTypeOf ne'
    {- Making arrow type. -}
    let lamTy = argTy <> exprTy
    let lamToken = Ty.NotedLam arg ne' lamTy $ stateOf arg
    return $ Ty.newNotedLam lamToken st

mkLambdaArg :: Raw.SymbolName With.ProgState -> TyInf (Ty.NotedVar With.ProgState)
mkLambdaArg sym = do
    let symSt = stateOf sym
    monoTy <- newMonoTypeFreeVar' symSt
    let nVar = Ty.newNotedVar (repOf sym) monoTy symSt
    putSymbolNoExpr nVar []
    return nVar

addArgsInTyEnv :: [Ty.NotedVar With.ProgState] -> TyInf ()
addArgsInTyEnv = mapM_ (`putSymbolNoExpr` [])

inferLambdaArg
    :: TypeHinting
    -> Ty.NotedVar With.ProgState
    -> TyInf (Ty.NotedVar With.ProgState)
inferLambdaArg monoTy arg = do
    subst <- tokenLessSpec arg monoTy
    let subst' = [subst]
    arg' <- arg `specToken` subst'
    specCurContext subst'
    return arg'

inferLambda
    :: [QualTypeHinting]
    -> Raw.Lambda With.ProgState
    -> ProgState
    -> TyInf (Ty.NotedExpr With.ProgState, [Ty.Substitution With.ProgState])
inferLambda hts (Raw.Lambda (syms, e, lst)) st = do
    {- NB: the case where there's no type-hinting, the arguments get updated (by applying the substitutions), while
    if there is type hinting, this does not happen; this is safe because if there is type hinting, all type variables
    are bound to quantifiers, so they do not need of substitutions. -}
    ifTypeHintingUnqual hts
        (\monoTy cs -> do
            (pairedTs, resTy) <- diffTokensType "lambda" lst syms monoTy
            args <- mapM mkLambdaArg syms

            {- Handling constraint problems. -}
            cps <- mapM mkProblem cs
            addCurConts cps

            {- Constraints are "bubbled" into the expression. -}
            (ne, substs) <- withLocalIncScope $ inferExpr [Ty.addContsToLHTy cs resTy] e

            specCurContext substs
            updArgs <- mapM getArg args
            args' <- zipWithM inferLambdaArg (map snd pairedTs) updArgs

            lamNe <- mkNestedLams args' ne st
            return (lamNe, [])
        ) `elseInfer` do
            args <- mapM mkLambdaArg syms

            (ne, substs) <- withLocalIncScope $ inferExpr [] e

            --Applying substitutions to arguments
            updArgs <- mapM (`specToken` substs) args
            lamNe <- mkNestedLams updArgs ne st

            return (lamNe, substs)

{- Inference of general symbol-declaration-like tokens. -}
inferBound
    :: [QualTypeHinting]
    -> Raw.LetUnion With.ProgState
    -> ProgState
    -> TyInf (Ty.NotedExpr With.ProgState, [Ty.Substitution With.ProgState])
inferBound hts be st = do
    (genSd, e, bst) <-
        case be of
            Raw.BExpr (Raw.BoundExpr (sd, e, bst)) ->
                pure (Raw.SD sd, e, bst)
            Raw.MBExpr (Raw.MultiBoundExpr msd e bst) ->
                pure (Raw.MSD msd, e, bst)
    (bNVar, bNVars, bNe) <- mkBindingRes genSd
    (inNe, substsIn) <- inferExpr hts e
    let boundToken = Ty.NotedBound bNVar bNVars bNe bst
    let ne = Ty.newBoundNotedExpr boundToken inNe st
    return (ne, substsIn)
    where
        mkBindingRes genSd = do
            let symRep = repOf $ Raw.symNameFromSD genSd
            pushNested symRep
            res <- withLocalInnerRecBindingCheck genSd mkNewBinding
            popNested
            return res

inferApplied
    :: Maybe QualTypeHinting
    -> Ty.NotedExpr With.ProgState
    -> [Raw.Expression With.ProgState]
    -> [Ty.Substitution With.ProgState]
    -> ProgState
    -> TyInf (Ty.NotedExpr With.ProgState, [Ty.Substitution With.ProgState])
inferApplied _ ne [] substs _ =
    return (ne, substs)
inferApplied mayQualTy applierNe (e : t) substs appSt =
    case mayQualTy of
        Nothing -> do
            (ne, substs') <- concatApps
            return (ne, substs')
        Just qualTy -> do
            (ne, substs') <- concatApps

            (cs, monoTy) <- splitUnqualify qualTy
            {- Type-check substitution has to be performed as last. -}
            resTy <- getMonoTypeOf ne
            subst <- typeMoreSpec monoTy resTy
            ne' <- specToken ne [subst]

            {- Handling constraint problems. -}
            cps <- mapM mkProblem cs
            addCurConts cps

            return (ne', substs' ++ [subst])
    where
        concatApps = do
            {- Applying substitution to typing environment. -}
            specCurContext substs

            (appNe, appSubsts) <- inferExpr [] e
            appTy <- getMonoTypeOf appNe

            {- Applying substitution to the applier, after the applied has been inferred. -}
            applierNe' <- specToken applierNe appSubsts
            applierTy <- getMonoTypeOf applierNe'

            {- Creating the return type. -}
            resTy <- newMonoTypeFreeVar appSt
            {- Given the application (e0 e1), unifying the type of e0 and the type: <type_of_e1> -> newTyVar
            where newTyVar is the return type created previously. -}
            subst <- unification' applierTy (appTy <> resTy)

            {- Specializing the return type. -}
            ty <- specToken resTy [subst]
            ty' <- liftMonoType ty
            let appToken = Ty.NotedApp applierNe' appNe ty' appSt
            let appExpr = Ty.newAppNotedExpr'' appToken appSt

            {- Infering the rest of expressions (`t`); the original algorithm expects that all three substitutions
            (one from applier inference, one from the applied inference, one from unification) are returned, but
            the substitutions from applier inference is applied eagerly to the type environment. -}
            (nestedNe, finalSubsts) <- inferApplied Nothing appExpr t (appSubsts ++ [subst]) appSt
            return (nestedNe, finalSubsts)

inferApplication
    :: [QualTypeHinting]
    -> Raw.AppExpression With.ProgState
    -> TyInf (Ty.NotedExpr With.ProgState, [Ty.Substitution With.ProgState])
inferApplication hts (Raw.AppExpr (e, es, ast)) = do
    ifTypeHinting hts
        `thenInfer`
            (inferApp . Just)
        `elseInfer`
            inferApp Nothing
    where
        inferApp mayTyHint = do
            {- Even if there is type-hinting, the applier is not affected by it because the type-hinting only acts
            on the return type. -}
            (applierNe, applierSubsts) <- inferExpr [] e
            inferApplied mayTyHint applierNe es applierSubsts ast

{- The cases inference works following the "flow of the data structure". It relies on the fact that the eventual top
level expressions have been already inferred. The following drawing shows how type inference for cases works:

    match topE with
        | m00 m01 m02 -> e0
        | m10 m11 m12 -> e1
        ...

    the program above is inferred in the following way...

  topE
    |
    V
    m00 -> m01 -> m02 -> e0
                          |
    +---------------------+
    |
    V
    m10 -> m11 -> m12 -> e1
                          |
    +---------------------+
    |
    V
    ... and so on and so forth ...

where what happens is that:
    - each time a matching expression m_ij is inferred, then the unification between m_ij and topE_i is performed and
      the resulting substitution is applied to the typing environment and to all previous tokens which have been
      already inferred (the so-called pattern matching stuff, see `specPMStuff`).
    - each time a case expression e_j is inferred, then the resulting substitutions is applied to pattern matching
      stuff, then the unification between e_j and e_(j-1) is performed and the resulting substitution is applied to
      pattern mathcing stuff.
-}
inferCases
    {- Eventual pattern matching type-hinting (so this is the type-hinting for the expression). -}
    :: Maybe QualTypeHinting
    {- Eventual top level type-hinting -}
    -> Maybe [QualTypeHinting]
    -> [Raw.MultiMatchCase With.ProgState]
    -> ProgState
    {- Inferred top level expression (if it is a single pattern match construct) or a list of noted variables
    (if it is a multiple pattern match construct) NB: which are supposed to be already in the typing environment. -}
    -> Either (Ty.NotedExpr With.ProgState) [Ty.NotedVar With.ProgState]
    -> TyInf ([Ty.NotedVar With.ProgState], Ty.NotedPM With.ProgState)
inferCases pmTh topTh mcs st top = do
    (cs, top1) <- inferCases' mcs [] top
    mkPm top1 cs
    where
        {- When the top level expressions are just one, the building of noted-pm is easy. -}
        mkPm (Left topExpr) cs = do
            ncs <- mkCases cs
            pmTy <- getPMType cs
            return ([], Ty.newNotedPM topExpr ncs pmTy st)
        {- This is the case where there is no top level expression, so there is more than one matching expression
        for each row (multiple pattern matching) and so a phase of desugaring is necessary. -}
        mkPm (Right nVars) cs = do
            let cs' = map (\(nms, ne, _) -> (nms, ne)) cs
            fv <- getFV
            dct <- getCons
            case RmMult.mkPattMatch cs' nVars fv dct of
                Left err -> tyInfErr $ RmMultPM err
                Right (npm, fv') -> return (nVars, npm) `withFV` fv'

        getPMType [] = noMatchingCases
        getPMType ((_, ne, _) : _) = getTypeOf ne

        mkCases [] = return []
        mkCases (([nme], ne, ncst) : t) = do
            let nc = Ty.newNotedCase nme ne ncst
            ncs <- mkCases t
            return $ nc : ncs
        mkCases _ = tyInfErr $ GenTokCreationErr "Too many matching expressions (only one is allowed) in case creation"

        inferCases' [] [] _ = noMatchingCases
        inferCases' [] ncs topToken = return (ncs, topToken)
        inferCases' ((Raw.MultiCase mes e mcSt) : t) prevNcs topToken = do
            (nms, ne, topToken1, ncs) <- inferCase mes e topToken prevNcs
            inferCases' t (ncs ++ [(nms, ne, mcSt)]) topToken1

        inferCase mes e topToken prevNcs = do
            incScope
            (nms, topToken1, prevNcs1) <- inferCaseMatchExprs mes 0 topToken prevNcs []
            incScope
            caseExprRes <- inferCaseExpr e topToken1 prevNcs1 nms
            decScope
            decScope
            return caseExprRes

        ifExistIthTyHint i f cont =
            case topTh of
                Nothing -> cont
                Just hts ->
                    case elemAt i hts of
                        Nothing -> cont
                        Just th -> f th

        inferCaseMatchExprs [] _ topToken prevNcs nms =
            return (nms, topToken, prevNcs)
        inferCaseMatchExprs (me : t) i topExpr @ (Left topNe) prevNcs prevNms = do
            ifExistIthTyHint i
                (\qualTy -> do
                    nme <- inferMatchExpr [qualTy] me
                    {- No need to calculate the mgu because there is the type hinting. -}
                    inferCaseMatchExprs t (i + 1) topExpr prevNcs (prevNms ++ [nme])
                ) `elseInfer` do
                    nme <- inferMatchExpr [] me
                    neTy <- getMonoTypeOf topNe
                    {- Calculating the mgu between the current inferred matching expression (`nme`) and the top-level
                    expression and applying the substitutions. -}
                    (nme1, topExpr1, nms, ncs) <- matchExprsMgu nme neTy topExpr prevNms prevNcs
                    inferCaseMatchExprs t (i + 1) topExpr1 ncs (nms ++ [nme1])
        inferCaseMatchExprs (me : t) i topNVars @ (Right nVars) prevNcs prevNms = do
            ifExistIthTyHint i
                (\qualTy -> do
                    {- Constraints are not turned into constraint problems, but they are "bubbled" into the next
                    expression. -}
                    nme <- inferMatchExpr [qualTy] me
                    {- No need to calculate the mgu because there is the type hinting. -}
                    inferCaseMatchExprs t (i + 1) topNVars prevNcs (prevNms ++ [nme])
                ) `elseInfer` do
                    nme <- inferMatchExpr [] me
                    {- If there is no top level expression, then the mgu has to be calculated between the current
                    inferred matching expression (`nme`) and the i-th top level variable. -}
                    case elemAt i nVars of
                        Nothing ->
                            noMatchingCases
                        Just nVar -> do
                            nVarTy <- getMonoTypeOf nVar
                            (nme1, topNVars1, nms, ncs) <- matchExprsMgu nme nVarTy topNVars prevNms prevNcs
                            inferCaseMatchExprs t (i + 1) topNVars1 ncs (nms ++ [nme1])

        matchExprsMgu nme ty topToken prevNms prevNcs = do
            nmeTy <- getMonoTypeOf nme
            {- Finding the most general unifier between the just inferred matching expression and the input type. -}
            subst <- unification' ty nmeTy
            let subst' = [subst]
            {- Applying substitution. -}
            (topToken1, nms, ncs) <- specPMStuff subst' topToken prevNms prevNcs
            nme1 <- nme `specToken` subst'
            return (nme1, topToken1, nms, ncs)

        inferCaseExpr e topToken prevNcs nms = do
            ifExistTypeHinting pmTh
                (\qualTy -> do
                    {- Constraints are not turned into constraint problems, but they are "bubbled" into the next
                    expression. -}
                    (ne, substs) <- inferExpr [qualTy] e
                    {- Applying the substitutions. -}
                    (topToken1, nms1, ncs) <- specPMStuff substs topToken nms prevNcs
                    {- No need to calculate the mgu because there is the type hinting. -}
                    return (nms1, ne, topToken1, ncs)
                ) `elseInfer` do
                    (ne, substs) <- inferExpr [] e
                    {- Applying the substitutions. -}
                    (topToken1, nms1, ncs1) <- specPMStuff substs topToken nms prevNcs
                    (ncs2, ne2, topToken2, nms2) <- exprsMgu ncs1 ne topToken1 nms1
                    return (nms2, ne2, topToken2, ncs2)

        exprsMgu [] ne topToken nms =
            return ([], ne, topToken, nms)
        {- Considering just the previous expression. In this way, for each row the selected expression to perform
        unification is always the one of the previous row. -}
        exprsMgu ncs @ ((_, prevNe, _) : _) ne topToken nms = do
            prevNeTy <- getMonoTypeOf prevNe
            neTy <- getMonoTypeOf ne
            subst <- unification' neTy prevNeTy
            let subst' = [subst]
            (topToken1, nms1, ncs1) <- specPMStuff subst' topToken nms ncs
            ne1 <- specToken ne subst'
            return (ncs1, ne1, topToken1, nms1)

        specNcs (nms, ne, ncSt) substs = do
            nms1 <- mapM (`specToken` substs) nms
            ne1 <- ne `specToken` substs
            return (nms1, ne1, ncSt)

        {- Applying the substitution to:
            - typing environment;
            - top level tokens;
            - previous matching expressions in the same "row";
            - previous inferred cases. -}
        specPMStuff substs topToken nms ncs = do
            specCurContext substs
            topToken1 <-
                case topToken of
                    Left topNe -> do
                        topNe1 <- topNe `specToken` substs
                        return $ Left topNe1
                    Right nVars -> do
                        nVars1 <- mapM (`specToken` substs) nVars
                        return $ Right nVars1
            nms1 <- mapM (`specToken` substs) nms
            ncs1 <- mapM (`specNcs` substs) ncs
            return (topToken1, nms1, ncs1)

inferPattMatch
    :: [QualTypeHinting]
    -> Raw.PatternMatch With.ProgState
    -> ProgState
    -> TyInf (Ty.NotedExpr With.ProgState, [Ty.Substitution With.ProgState])
inferPattMatch hts (Raw.PattMatch (e, cs, pmst)) st = do
    {- Eventual type-hinting on the entire pattern match construct does not affect top level expression. -}
    (ne, topSubsts) <- inferExpr [] e
    {- Applying substitutions from to level expression inference immediately. -}
    specCurContext topSubsts
    let mcs = map Raw.multiCaseFromSingleCase cs
    {- Fetching type-hinting of top level expression because it affects also matching expressions in cases. -}
    topTh <- ifHintIn'' e (\qualTy -> pure $ Just [qualTy]) (pure Nothing)
    (_, npm) <-
        ifTypeHinting hts
            (\qualTy -> do
                inferCases (Just qualTy) topTh mcs pmst $ Left ne
            ) `elseInfer` do
                inferCases Nothing topTh mcs pmst $ Left ne
    return (Ty.newPMNotedExpr npm st, [])

multiCaseStates :: [Raw.MultiMatchCase With.ProgState] -> TyInf [ProgState]
multiCaseStates [] = noMatchingCases
multiCaseStates ((Raw.MultiCase ms _ _) : _) = return $ map stateOf ms

inferMultiLambda
    :: [QualTypeHinting]
    -> Raw.MultiLambda With.ProgState
    -> ProgState
    -> TyInf (Ty.NotedExpr With.ProgState, [Ty.Substitution With.ProgState])
inferMultiLambda hts (Raw.MultiLambda mpm mlst) st = do
    let mcs = Raw.casesFromMultiPatt mpm
    sts <- multiCaseStates mcs
    {- Regardless there is type-hinting, the algorithm is more or less the same:
        - creating the lambda abstraction variables;
        - put the variables in the typing environement;
        - infering cases;
        - decrementing the scope and return. -}
    ifTypeHintingUnqual hts
        (\monoTy cs -> do
            let qualTy = Ty.addContsToLHTy cs monoTy
            (tokens, resTy) <- diffTokensQualType "lambda" mlst sts qualTy

            {- Making lambda arguments. -}
            args <- newLambdaArgs sts
            addArgsInTyEnv args

            {- Handling constraint problems. -}
            cps <- mapM mkProblem cs
            addCurConts cps

            let ts = map snd tokens
            (_, npm) <-
                withLocalIncScope .
                    inferCases (Just resTy) (Just ts) mcs mlst $ Right args

            {- Updating lambda arguments. -}
            updNVars <- mapM getArg args
            argsTs <- mapM unqualify ts
            nVars <- zipWithM inferLambdaArg argsTs updNVars

            mkLamExpr npm nVars
        ) `elseInfer` do
            {- Making lambda arguments. -}
            args <- newLambdaArgs sts
            addArgsInTyEnv args

            (updNVars, npm) <-
                withLocalIncScope .
                    inferCases Nothing Nothing mcs mlst $ Right args
            mkLamExpr npm updNVars
    where
        mkLamExpr npm nVars = do
            let npmNe = Ty.newPMNotedExpr npm st
            ne <- mkNestedLams nVars npmNe st
            return (ne, [])

        genNVarTy nVarSt = do
            monoTy <- newMonoTypeFreeVar' nVarSt
            return (monoTy, nVarSt)

inferUAExpr
    :: [QualTypeHinting]
    -> Raw.UnAltExpression With.ProgState
    -> ProgState
    -> TyInf (Ty.NotedExpr With.ProgState, [Ty.Substitution With.ProgState])
inferUAExpr hts (Raw.Base sn) st =
    inferExprSymbol hts sn st
inferUAExpr hts (Raw.ADTBase cn) st =
    inferExprDataCon hts cn st
inferUAExpr hts (Raw.Lit l) st = do
    nVal <- inferLiteral l hts
    return (Ty.newValNotedExpr nVal st, [])
inferUAExpr hts (Raw.Bound be) st =
    inferBound hts (Raw.BExpr be) st
inferUAExpr hts (Raw.Lam lam) st =
    inferLambda hts lam st
inferUAExpr hts (Raw.App app) _ =
    inferApplication hts app
inferUAExpr hts (Raw.Match pm) st =
    inferPattMatch hts pm st
inferUAExpr hts (Raw.MultiBound mbe) st =
    inferBound hts (Raw.MBExpr mbe) st
inferUAExpr hts (Raw.MultiLam multiLam) st =
    inferMultiLambda hts multiLam st

inferExpr
    :: [QualTypeHinting]
    -> Raw.Expression With.ProgState
    -> TyInf (Ty.NotedExpr With.ProgState, [Ty.Substitution With.ProgState])
inferExpr hts rawE @ (Raw.Expr (uae, _, est)) = do
    exprTh <-
        ifHintIn' rawE
            (\polyTy -> do
                qualTy <- instantiate polyTy
                return [qualTy]
            ) `elseInfer`
                return []
    inferUAExpr (exprTh ++ hts) uae est

mkNewBinding :: Raw.SDUnion With.ProgState -> TyInf (BindingSingleton With.ProgState)
mkNewBinding gensd = do
    case gensd of
        Raw.SD sd -> mkNewBindingFromSD sd
        Raw.MSD msd -> mkNewBindingFromMSD msd

mkBindingType
    :: [Ty.NotedVar With.ProgState]
    -> Ty.NotedExpr With.ProgState
    -> TyInf (Ty.LangTypeScheme With.ProgState)
mkBindingType nVars ne = do
    argsTs <- mapM getTypeOf nVars
    resTy <- getTypeOf ne
    let polyTs = newNELast argsTs resTy
    {- Making arrow types. -}
    return $ sconcat polyTs

{- It builds up a new symbol, by taking the type of arguments and expression. It performs unification with the old
symbol in the current typing environment, if it exists. -}
{- TODO: handle constraints problems. -}
mkSymbol
    :: Raw.SymbolName With.ProgState
    -> [Ty.NotedVar With.ProgState]
    -> Ty.NotedExpr With.ProgState
    -> TyInf (BindingSingleton With.ProgState)
mkSymbol sn nVars ne = do
    polyTy <- mkBindingType nVars ne
    let symRep = repOf sn
    {- If a symbol with `symRep` representation already exists, then unification has to be performed, else it just
    builds the noted variable. -}
    maysym <- getSymbol symRep
    case maysym of
        Nothing -> do
            let nVar' = Ty.newNotedVar symRep polyTy $ stateOf sn
            return (nVar', nVars, ne)
        Just (nVar, _, _, _, _, _) -> do
            ty' <- getMonoTypeOf polyTy
            oldTy <- getMonoTypeOf nVar
            subst <- unification' ty' oldTy
            let subst' = [subst]
            {- After unification, applying substitution to the typing environment. -}
            specCurContext subst'
            {- TODO: specialization on args and expr is likely to be not necessary. -}
            nVar' <- nVar `specToken` subst'
            nVars' <- mapM (`specToken` subst') nVars
            ne' <- ne `specToken` subst'
            addRecOrNonRecSymbol' nVar' nVars' ne'
            return (nVar', nVars', ne')

addRecOrNonRecSymbol'
    :: Ty.NotedVar With.ProgState
    -> [Ty.NotedVar With.ProgState]
    -> Ty.NotedExpr With.ProgState
    -> TyInf ()
addRecOrNonRecSymbol' nVar nVars nExpr = do
    let symRep = repOf nVar
    isRec <- isMutRecSym symRep
    if isRec
    then do
        sc <- getScope
        updateSymbolWithArgsAndExpr nVar sc nVars nExpr
    else
        putSymbolWithExpr nVar nVars nExpr []

{- Optimization: if the scope is the global scope, then it should be safe to generalize
all free variables of the top-level symbol's type because it is supposed its free variables do not appear
anywhere but in its type. This is not true for recursive bindings. -}
tryGeneralize
    :: Ty.NotedVar With.ProgState
    -> TyInf ([ConstraintProblem], [DispatchConstraint], Ty.NotedVar With.ProgState)
tryGeneralize nVar = do
    sc <- getScope
    if sc == NS.globalScope
    then unsafeGeneralizeTokenAndImply nVar
    else generalizeAndImply nVar

normalizeProblems
    :: [ConstraintProblem]
    -> Ty.NotedExpr With.ProgState
    -> TyInf (Ty.NotedExpr With.ProgState)
normalizeProblems [] ne = return ne
normalizeProblems cs ne = do
    {- `solve` performs normalization. -}
    solved <- mapM solve cs
    {- Replacing occurrences of normalized constraints with dispatch values. -}
    let (updNe, ok) = Ty.widthVisitExprsInExpr' True ne $ replaceIndexes solved
    if ok
    then do
        {- It adds the current binding to ones to refine, because if there are more than zero problems to normalize,
        then any symbol in the expression can be subjected to static dispatch. -}
        setRefinement
        return updNe
    else tyInfErr $ GenUnreachableState "Error on dispatch values building"
    where
        setRefinement = do
            ns <- getNested
            {- Reversing the list, in order to top level symbol is first. -}
            case reverse ns of
                [] -> emptyNested
                (symRep : symT) -> addBindingToRefine $ symRep :| symT

        solve (c, i) = do
            c' <- normalization c
            return (c', i)

        replaceIndexes solved ok expr @ (Ty.ExprDispatchVar nVar dispToks st) =
            if not ok
            then (expr, False)
            else
                case maybemap (replaceIndex solved) dispToks of
                    Nothing -> (expr, False)
                    Just dispToks' -> (Ty.ExprDispatchVar nVar dispToks' st, True)
        replaceIndexes _ ok exprInfo = (exprInfo, ok)

        replaceIndex _ tok @ (Ty.DispatchVal _ _) = Just tok
        replaceIndex solved tok @ (Ty.DispatchVar nVar st) =
            let nVarTy = Ty.typeOf nVar in
            let nVarRep = repOf nVar in
                if any (\(_, ix) -> nVarRep == ix) solved
                then Ty.newDispatchVal nVarTy st
                else Just tok

{- NB: to use only in the scope of mkDispatchArgs* functions. -}
mkDispatchArg :: DispatchConstraint -> TyInf (Ty.NotedVar With.ProgState)
mkDispatchArg dc @ (c, _) = do
    {- "manual" generalization. -}
    let binders = Ty.bVarsOf c
    let st = stateOf c
    --TODO: binders should be always empty list, it can be optimized
    mkDispatchVar binders st dc

{- NB: if you need to make noted variables, you should not use this, because it performs generalization of free
variables in constraint problems. Use `mkDispatchVar` instead, by passing it binders explicitly. This function, as
name tells, can be used for arguments of functions (lambda abstraction). -}
mkDispatchArgs :: [DispatchConstraint] -> TyInf [Ty.NotedVar With.ProgState]
mkDispatchArgs = mapM mkDispatchArg

mkDispatchArgs' :: [Ty.LangSpecConstraint With.ProgState] -> TyInf [Ty.NotedVar With.ProgState]
mkDispatchArgs' = mapM mkArg
    where
        mkArg c = do
            ix <- newIndex
            mkDispatchArg (c, ix)

{- It handles constraints problems after generalization. Then, it adds the generalized variable in the current
typing environment. -}
handleConstraints
    :: Ty.NotedVar With.ProgState
    -> [Ty.NotedVar With.ProgState]
    -> Ty.NotedExpr With.ProgState
    -> [ConstraintProblem]
    -> [DispatchConstraint]
    -> TyInf (BindingSingleton With.ProgState)
handleConstraints genNVar nVars ne cs dispCs = do
    {- Normalization: it updates also the expression, by replacing symbols denoting normalized constraints
    with dispatch values. -}
    updNe <- normalizeProblems cs ne
    {- Updating arguments: adding dispatch arguments. -}
    dispArgs <- mkDispatchArgs dispCs
    let args = dispArgs ++ nVars
    return (genNVar, args, updNe)

{- It performs all that stuff which has to be performed at the end of binding ("let..") declarations:
    - generalization;
    - normalization of constraint problems;
    - dispatch arguments building;
    - adding of not normalized constraints to the type of binding;
    - adding the binding to the current typing environment. -}
endLetInference
    :: Ty.NotedVar With.ProgState
    -> [Ty.NotedVar With.ProgState]
    -> Ty.NotedExpr With.ProgState
    -> TyInf (BindingSingleton With.ProgState)
endLetInference nVar nVars ne = do
    {- Generalization: it returns also constraint problems to normalize and constraint problems to add to the
    type. -}
    (cs, dispCs, genNVar) <- tryGeneralize nVar
    (genNVar', args, updNe) <- handleConstraints genNVar nVars ne cs dispCs
    {- Adding symbol to typing environment. -}
    addRecOrNonRecSymbol' genNVar' args updNe
    return (genNVar', args, updNe)

{- If the symbol is mutually recursive, then generalization, normalization and other stuff to perform at the end
of "let.." construct inference are postponed. However, this is done only if the symbol is global, because if the
symbol is nested and it is recursive, it must be the self-recursive:

    let f args = ... f ...

and if it is self-recursive, there is nothing to postpone, because there is no left recursive symbol to infer, thus
it's safe to perform the stuff of "let.." inference end. -}
endingLet
    :: Ty.NotedVar With.ProgState
    -> [Ty.NotedVar With.ProgState]
    -> Ty.NotedExpr With.ProgState
    -> TyInf (BindingSingleton With.ProgState)
endingLet nVar nVars ne =
    {- TODO: only global mutually recursive symbols have a different "end of inference". Nested recursive symbols
    can be only self-recursive AT THE MOMENT, but if the language extends mutually recursive symbols to nested ones
    as well, this has to be changed. -}
    ifMutRecGlobalSym (repOf nVar)
        `thenInfer`
            return (nVar, nVars, ne)
        `elseInfer`
            endLetInference nVar nVars ne

getArg :: Ty.NotedVar With.ProgState -> TyInf (Ty.NotedVar With.ProgState)
getArg nVar = do
    (nVar', _, _, _, _, _) <- getSymbol' $ repOf nVar
    return nVar'

endHintInference
    :: Ty.NotedVar With.ProgState
    -> [Ty.NotedVar With.ProgState]
    -> Ty.NotedExpr With.ProgState
    -> [Ty.LangSpecConstraint With.ProgState]
    -> TyInf (BindingSingleton With.ProgState)
endHintInference nVar nVars ne cs = do
    {- Getting a variable with a mono-type type, in order to generalize it. The generalization gives us the constraints
    to normalize. -}
    monoTy' <- mkBindingType nVars ne
    let toGenNVar = Ty.updateType nVar monoTy'
    (toNormCs, dispCs, genNVar) <- tryGeneralize toGenNVar

    let origCs = map fst dispCs
    {- The minimal constraints have to be respected. -}
    (_, hintCs) <- checkMinimalConstraints origCs cs

    hintCps <- mapM mkProblem hintCs
    let cps = hintCps ++ dispCs
    (_, args, ne') <- handleConstraints genNVar nVars ne toNormCs cps

    addRecOrNonRecSymbol' genNVar args ne'
    return (genNVar, args, ne')

mkNewBindingFromSD :: Raw.SymbolDeclaration With.ProgState -> TyInf (BindingSingleton With.ProgState)
mkNewBindingFromSD sd =
    ifHintInSd' (Raw.SD sd)
        (\nVar _ cs -> do
            {- Splitting type-hinting. -}
            polyTy <- getTypeOf nVar
            {- Incrementing scope twice, once for arguments and once for the expression. -}
            (args, argsTs, qualTy) <- withIncScope . inferHintArgsSymD polyTy $ argsOf sd
            (ne, substs) <- withIncScope . inferExpr [qualTy] $ Raw.exprFromSymDecl sd
            specCurContext substs

            {- Re-fetching arguments to grab updates on their types. -}
            updNVars <- mapM getArg args
            nVars <- zipWithM inferLambdaArg argsTs updNVars

            {- Decrementing twice, because previously the scope has been incremented exactly twice. -}
            decScope
            decScope

            endHintInference nVar nVars ne cs
        ) `elseInfer` do
            {- It's not possible to insert immediately the noted variable into the typing environment without
            type-hinting. -}
            nVars <- withIncScope . inferArgsSymD $ argsOf sd
            (ne, substs) <- withIncScope $ inferExpr [] $ Raw.exprFromSymDecl sd

            {- Specializing and getting the updated arguments. -}
            specCurContext substs
            updNVars <- mapM getArg nVars

            {- Decrementing twice, because previously the scope has been incremented exactly twice. -}
            decScope
            decScope

            (nVar, updNVars', updNe) <- mkSymbol (Raw.symNameFrom sd) updNVars ne
            endingLet nVar updNVars' updNe
    where
        inferHintArgsSymD
            :: Ty.LangTypeScheme With.ProgState
            -> [Raw.SymbolName With.ProgState]
            -> TyInf ([Ty.NotedVar With.ProgState], [Ty.LangHigherType With.ProgState], Ty.LangQualType With.ProgState)
        inferHintArgsSymD polyTy rawArgs = do
            qualTy <- instantiate polyTy
            (pairedTs, resMonoTy) <- diffTokensQualType "symbol definition" (stateOf sd) rawArgs qualTy
            args <- mapM mkLambdaArg rawArgs
            addArgsInTyEnv args
            ts <- mapM (unqualify . snd) pairedTs
            return (args, ts, resMonoTy)

        {- The rule of symbol definition arguments inference is just the concatenation of many lambda abstraction
        rules. -}
        inferArgsSymD
            :: [Raw.SymbolName With.ProgState]
            -> TyInf [Ty.NotedVar With.ProgState]
        inferArgsSymD syms = do
            args <- mapM mkLambdaArg syms
            addArgsInTyEnv args
            return args

mkNewBindingFromMSD :: Raw.MultiSymbolDeclaration With.ProgState -> TyInf (BindingSingleton With.ProgState)
mkNewBindingFromMSD msd = do
    let mcs = Raw.casesFromMultiPatt $ Raw.multiPattMatchFrom msd
    sts <- multiCaseStates mcs
    let st = stateOf msd
    ifHintInSd' (Raw.MSD msd)
        (\nVar _ cs -> do
            {- Splitting type inference. -}
            polyTy <- getTypeOf nVar
            (args, ts, resTy) <- withIncScope $ inferHintArgs polyTy sts
            (_, npm) <- withIncScope . inferCases (Just resTy) (Just ts) mcs st $ Right args

            {- Re-fetching arguments to grab updates on their types. -}
            updNVars <- mapM getArg args
            argsTs <- mapM unqualify ts
            nVars <- zipWithM inferLambdaArg argsTs updNVars

            decScope
            decScope

            let ne = Ty.newPMNotedExpr npm st
            endHintInference nVar nVars ne cs
        ) `elseInfer` do
            nVars <- withIncScope $ inferArgs sts
            (updNVars, npm) <- withIncScope . inferCases Nothing Nothing mcs st $ Right nVars
            let ne = Ty.newPMNotedExpr npm st

            decScope
            decScope

            (nVar, updNVars', updNe) <- mkSymbol (Raw.symNameFromMultiSymDecl msd) updNVars ne
            endingLet nVar updNVars' updNe
    where
        inferHintArgs
            :: Ty.LangTypeScheme With.ProgState
            -> [ProgState]
            -> TyInf
                ( [Ty.NotedVar With.ProgState]
                , [Ty.LangQualType With.ProgState]
                , Ty.LangQualType With.ProgState
                )
        inferHintArgs polyTy sts = do
            qualTy <- instantiate polyTy
            (tokens, resTy) <- diffTokensQualType "symbol definition" (stateOf msd) sts qualTy
            nVars <- newLambdaArgs sts
            addArgsInTyEnv nVars
            let ts = map snd tokens
            return (nVars, ts, resTy)

        inferArgs :: [ProgState] -> TyInf [Ty.NotedVar With.ProgState]
        inferArgs sts = do
            nVars <- newLambdaArgs sts
            addArgsInTyEnv nVars
            return nVars

newLambdaArgs :: [ProgState] -> TyInf [Ty.NotedVar With.ProgState]
newLambdaArgs = mapM createNotedVar
    where
        createNotedVar st = do
            name <- getSuffix
            let sn = Raw.buildSymbolName name st
            mkLambdaArg sn

getConstraintsToNormInRec
    :: [ConstraintProblem]
    -> [BindingSingleton With.ProgState]
    -> TyInf
        ( [ConstraintProblem]
        , [DispatchConstraint]
        )
getConstraintsToNormInRec cps = do
    foldM dividing ([], [])
    where
        {- The first iteration, nothing to partition -}
        dividing ([], []) (nVar, _, _) = do
            monoTy <- getMonoTypeOf nVar
            return $ Ty.divideContsToNorm cps monoTy []
        dividing (cs, dispCs) (nVar, _, _) = do
            monoTy <- getMonoTypeOf nVar
            let (cs', _) = Ty.divideContsToNorm cps monoTy []
            let (newCs, resDispCs) = L.partition (`elem` cs') dispCs
            return (newCs ++ cs, resDispCs)

generalizeWithConstraints
    :: BindingSingleton With.ProgState
    -> [DispatchConstraint]
    -> TyInf (BindingSingleton With.ProgState)
generalizeWithConstraints (nVar, nVars, ne) dispCs = do
    monoTy <- getMonoTypeOf nVar
    let cs = map fst dispCs
    let qualTy = Ty.addContsToLHTy cs monoTy
    let polyTy = Ty.generalize' qualTy
    return (Ty.updateType nVar polyTy, nVars, ne)

{- The end of recursive bindings inference is a little bit different from non-recursive counterpart.
First of all, it is necessary to choose when a constraint problem has to be normalized in presence of many recursive
bindings:
    - given a set of bindings, a constraint problem has to be normalized if there is a binding in the set the constraint
      problem has to be normalized for.
Think about the following example:

    let z = z
    let f x y = let c = g z z in x == y
    let g x y = let c = f z z in x + y

Here, the (mono-)types of f and g are:

    f : a1 -> a1 -> Bool
    g : a2 -> a2 -> a2

The constraints problems are:

    Eq a1
    Num a2

Now, from point of view of `f`, the constraint `Num a2` should be normalized, so `Num a2` has to be normalized.
Same for `Eq a1`, but from the point of view of `g`. Thus, `Eq a1` and `Num a2` must be normalized and the generalized
types of `f` and `g` will be:

    f : forall a1. a1 -> a1 -> Bool
    g : forall a2. a2 -> a2 -> a2

But sometimes, it can happen that constraints problems have not be normalized for each single binding, look at the
following example:

    p x y = r (x + y) (x - y)
    r x y = p (x - y) (x + y)

in this case, there is a constraint problem:

    Num a

which has not to be normalized both for `p` and for `r`, so the final types are:

    p : forall a, b. Num a => a -> a -> b
    r : forall a, b. Num a => a -> a -> b

In this case, dispatch variables have to be set within the expressions where recursive calls of `p` and `r` stands.
This is necessary, because symbol inference is not capable of infering the right constraints, since at the time it is
performed, recursive bindings constraints are unknown.
-}
endRecInference :: [BindingSingleton With.ProgState] -> TyInf [BindingSingleton With.ProgState]
endRecInference bs = do
    cps <- getCurConts
    (cs, dispCs) <- getConstraintsToNormInRec cps bs
    genBs <- mapM (`generalizeWithConstraints` dispCs) bs

    {- Making dispatch variables. -}
    dispArgs <- mkDispatchArgs dispCs

    let dispToks = map (\arg -> Ty.DispatchVar arg $ stateOf arg) dispArgs
    let bsReps = map (\(nVar, _, _) -> repOf nVar) genBs
    {- Visiting the expressions of each single binding and adding the dispatch variables ahead of recursive calls.
    Note that dispatch variables are always the same, since the constraints problems not to normalize are the same
    for each binding. -}
    genBs' <- mapM (addIndexesInExpr bsReps dispToks) genBs

    {- Normalization. Here, dispatch arguments are created as well. They must be created in the same way dispatch
    arguments (`dispArgs`) are created. -}
    mapM (handleConstraintsRec cs dispCs) genBs'
    where
        handleConstraintsRec cs dispCs (nVar, nVars, ne) = do
            {- Why pushing the nested symbol right here (and then pop)? `handleConstraintsThenAdd` perfoms
            normalization and, with it, it sets the bindings to refine. In order to set them, it is necessary the
            symbols stack is not empty and at this point the stack is empty. -}
            pushNested $ repOf nVar
            (nVar', nVars', ne') <- handleConstraints nVar nVars ne cs dispCs
            addRecOrNonRecSymbol' nVar' nVars' ne'
            popNested
            return (nVar', nVars', ne')

        addIndexesInExpr bsReps dispToks (nVar, nVars, ne) = do
            let ne' = Ty.widthVisitExprsInExpr ne $ addDispArgs bsReps dispToks
            return (nVar, nVars, ne')

        addDispArgs bsReps dispToks expr @ (Ty.ExprVar nVar st) =
            if repOf nVar `elem` bsReps
            then Ty.ExprDispatchVar nVar dispToks st
            else expr
        addDispArgs _ _ expr = expr

inferBinding :: Prep.RawBinding -> TyInf ()
inferBinding (Prep.RawNonRec sd) = do
    putMutRec []
    let symRep = repOf $ Raw.symNameFromSD sd
    pushNested symRep
    (nVar, nVars, nExpr) <- mkNewBinding sd
    popNested
    let binding = TyNonRec (nVar, nVars, nExpr)
    putProgSymbol binding
    clearCurTyEnv
    resetCurConts
    return ()
inferBinding (Prep.RawRec bs) = do
    addMostGenNVars
    putMutRec mutRecSymbols
    resBs <- mapM inferAndApplySubsts bs
    genBs <- endRecInference resBs
    let binding = TyRec genBs
    putProgSymbol binding
    clearCurTyEnv
    resetCurConts
    return ()
    where
        inferAndApplySubsts sd = do
            let symRep = repOf $ Raw.symNameFromSD sd
            pushNested symRep
            (nVar, nVars, nExpr) <- mkNewBinding sd
            popNested
            return (nVar, nVars, nExpr)

        mutRecSymbols = map (repOf . Raw.symNameFromSD) bs

        addMostGenNVars = mapM (`addMostGenNVar` mutRecSymbols) bs

inferBindings :: Prep.SortedBindings -> TyInf ()
inferBindings = mapM_ inferBinding

build
    :: Fresh.FV ()
    -> TypesTable With.ProgState
    -> DataConsTable With.ProgState
    -> ConstraintsTable With.ProgState
    -> PropMethodsTable With.ProgState
    -> ImplTable With.ProgState
    -> Prep.SortedBindings
    -> Either TyInfErr
        ( TypedProgram With.ProgState
        , Fresh.FV ()
        , UniqueVarCounter
        , [BindingToRefine]
        )
build fv tt dct ct mhts it bs =
    case execStateT (inferBindings bs) tyInfState of
        Left err -> Left err
        Right st -> Right $ fetchTyInfRes st
    where
        tyInfState =
            S.initGenState fv tt dct ct noElems mhts it noElems tyInfInfo

        tyInfInfo =
            TIInfo empty [] [] Nothing NS.globalScope C.new C.new [] []

        fetchTyInfRes st =
            let tp = S.fetchProg st in
            let fv' = S.fetchFV st in
                case S.fetchData st of
                    TIInfo _ _ _ _ _ _ vc _ toRef -> (tp, fv', vc, toRef)
