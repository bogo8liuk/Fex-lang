{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{- NB: in general, equality tests of primitive tokens like variables and *-constructors are based on the comparison
with the atomic string part (see `AtomRep` type-class). -}

module Compiler.Ast.Typed
    ( EqPromote(..)
    , EqContext(..)
    , cxtEq'
    , TyVars(..)
    , tyVarsOfMany
    , endTyVarsOfSubst
    , Substitution
    , OrdContext(..)
    , HasKind(..)
    , ActualKind(..)
    , HasConstraints(..)
    , HasType(..)
    , ActualType(..)
    , UpdateType(..)
    , SpecType(..)
    , LangKind(..)
    , promoteKind
    , promoteVar
    , diffKinds
    , sameInfrdKindOf
    , LangType(..)
    , LangHigherType(..)
    , LangQualType(..)
    , unconstrainedTypeOf
    , SatisfiabilityError(..)
    , isSatisfied
    , isContMoreSpec
    , isContEqualSpec
    , normalize
    , LangTypeScheme(..)
    , generalize
    , generalize'
    , generalize''
    , generalizeExcluding
    , generalizeExcluding'
    , generalizeExcluding''
    , divideContsToNorm
    , generalizeAndTryImply
    , liftQualType
    , liftMonoType
    , instantiate
    , instantiateUnqualifying
    , removeOneConstraint
    , removeAllConstraints
    , showCont
    , showConts
    , showLHTy --TODO: rm this
    , showLQTy
    , showLPTy
    , showHeadsLHTy
    , showHeadsLHTyWith
    , showSubst
    , toLHTy
    , toLHTy'
    , ifFun
    , doOnType
    , unfoldType
    , unfoldQualType
    , unfoldQualType'
    , unfoldQualTypeOnce
    , unfoldTypeScheme
    , splitQualType
    , specVars
    , specLambda
    , UnificationError(..)
    , unify
    , isMoreSpecThan
    , isLessSpecThan
    , exactlyTheSame
    , exactlyTheSameConstraints
    , LangVarType
    , roleOf
    , Role(..)
    , occFirstTyVarsOfMany
    --, addContsToLVTy
    , addContsToLHTy
    , addContsToLQTy
    , addContsToLPTy
    --, removeTrueConstraintsFromLQTy
    --, removeTrueConstraintsFromLPTy
    , LangVarCompType
    , LangNewConstraint
    , LangSpecConstraint
    , LangNewType   --AbDT
    , LangSpecType  --AbDT
    -- program tokens
    , NotedVar
    , NotedLiteral(..)
    , NotedVal
    , OnlyConstraintScheme
    , onlyConstraintToScheme
    , newConstraintScheme
    , doOnVal
    , nValArgsNumber
    , DispatchTok(..)
    , newDispatchVal
    , newDispatchVal'
    , constraintFromVal
    --TODO: AbDt or keeping open like these???
    , NotedTok(..)
    , NotedMinimalExpr(..)
    , NotedMatchExpr(..)
    , nVarsOf
    , NotedExpr(..)
    , NotedApp(..)
    , NotedCase(..)
    , typeOfMatching
    , NotedPM(..)
    , getScrutinee
    , NotedBound(..)
    , NotedLam(..)
    , newVarLKTy
    , newLNTy
    , newLVTy
    , newLSpTy
    , newLSpTy'
    , newLVcTy
    , newLNCont
    , newLTy
    , newLTy'
    , newLTy''
    , newLowLHTy
    , newLowLHTy'
    , newLowLHTy''
    , newLHTyFromLVTy
    , newLHTyFromLNTy --TODO: rm this
    , newLQTyFromLVTy
    , newLQTyFromLNTy
    , newQualType
    , visitVarsInLNTy
    , visitVarsInLNCont
    , visitVarsInLHTy
    , visitVarsInLSpCont
    , visitVarsInLSpConts
    , newFunLHTy
    , newConcatLHTy
    , newLSpTyFromLSpTy
    , ContBuildError(..)
    , newLSpCont
    , newNaiveLSpCont
    , newNotedVar
    , newDispatchNotedVar
    , newNotedVal
    , newNotedIntLit
    , newNotedDoubleLit
    , newNotedCharLit
    , newNotedStringLit
    , newDefNotedMinExpr
    , newVarNotedMinExpr
    , newMinNotedMExpr
    , newVarNotedMExpr
    , newValNotedMExpr
    , newMinCompNotedMExpr
    , newCompNotedMExpr
    , newDefNotedMExpr
    , newVarNotedExpr
    , newVarsDispatchNotedExpr
    , newValNotedExpr
    , newBoundNotedExpr
    --, newAppNotedExpr
    , newAppNotedExpr'
    , newAppNotedExpr''
    , newPMNotedExpr
    , newNotedCase
    , newNotedPM
    , newNotedLam
    --, newNotedLams
    --, newNotedLams'
    , kindOfArg
    , rawSameBaseOf
    , sameBaseOf
    , notedVarOf
    , notedValOf
    , anyTyVars
    , lengthVisitExprsInExpr
    , widthVisitExprsInExpr
    , lengthVisitExprsInExpr'
    , widthVisitExprsInExpr'
    --, newLSTyFromLNTy
    --, newLSTyFromSpecLNTy
    , showPM
    , showExpr
) where

import Lib.Utils
import Data.List(foldl', nub, partition)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Maybe(isNothing)
import Data.Map.Strict as M hiding (map, foldl', foldl, take, drop, elemAt, findIndex, null, filter, splitAt, partition)
import Data.Semigroup
import qualified Lib.Counter as C
import Control.Monad.State.Lazy
import Compiler.Ast.Common
import Compiler.Config.Types as BI
import TyCon(Role(..))

{- Type-class to capture the notion of equality along with the promotion of a concept. A type is EqPromote iff
it offers a way to tell if two elements are equal or not and, if necessary, an action of promotion must be performed.
NB: it does not perform any action of promotion (whatever it means), it's up to the client to fetch the promotion
value and handle the situation. -}
class EqPromote tok prom where
    (==^) :: tok -> tok -> prom

{- It has the same semantics of EqPromote, but it adds a context in the equality test. -}
class EqContext tok prom cxt where
    cxtEq :: tok -> tok -> cxt -> prom

{- The reverse of cxtEq. Generally, cxtEq could be not symmetric and it could be troublesome to remember to reverse
the arguments every time. -}
cxtEq' :: EqContext tok prom cxt => tok -> tok -> cxt -> prom
cxtEq' token token' = cxtEq token' token

class TyVars tok where
    tyVarsOf :: tok a -> [LangVarType a]
    tyVarsOf token = bVarsOf token ++ fVarsOf token
    {- It returns the free variables of a token. -}
    fVarsOf :: tok a -> [LangVarType a]
    {- It returns the non-free (bound) variables of a token. -}
    bVarsOf :: tok a -> [LangVarType a]
    {- It returns the bound variables of a token which actually occur in the token. -}
    actualBVarsOf :: tok a -> [LangVarType a]

tyVarsOfMany :: TyVars tok => [tok a] -> [LangVarType a]
tyVarsOfMany = nub . concatMap tyVarsOf

endTyVarsOfSubst :: Substitution a -> [LangVarType a]
endTyVarsOfSubst = concatMap (tyVarsOf . snd)

type Substitution a = [(LangVarType a, LangHigherType a)]
type SubstitutionLambda a = LangVarType a -> Maybe (LangHigherType a)

{- An extension of EqContext. -}
class OrdContext tok prom cxt where
    cxtCompare :: tok -> tok -> cxt -> prom

{- Type-class for those tokens which have the notion of kind. This is useful to calculate the kind of a primitive
token, but it is context-independent and it does not take into consideration how a token is used. Look at `ActualKind`
if this is undesirable. -}
class HasKind tok where
    kindOf :: tok -> LangKind

{- Type-class for those tokens whose kind can change in different points of a program. For instance:
Suppose we have a type declaration with type `Foo` and arguments `a`, `b` and `c`. Now suppose
`Foo` has kind (* -> * -> * -> *) (so `a`, `b` and `c` must have kind *); now, `Foo` can be used
in different ways in a program, for example:
    1) `Foo Bar (Foo Bar Baz Bar) Baz`
    2) `Foo Baz`
    3) `Foo`
Supposing `Bar` and `Baz` have kind *, the first "type instance" has kind *, the second one has kind
* -> * -> * and the third one has * -> * -> * -> * (like `Foo`). -}
class ActualKind tok where
    {- The returned value is a Maybe to leave place to errors. -}
    infrdKindOf :: tok -> Maybe LangKind

{- Type-class for those tokens which have the notion of constraints. -}
class HasConstraints tok where
    contsOf :: tok a -> [LangSpecConstraint a]

{- Type-class for those tokens which have the notion of type. -}
class HasType tok where
    typeOf :: tok a -> LangTypeScheme a

{- Same reason at the base of ActualKind: it is necessary to have a tool which tests the correct
construction of tokens at the type level. -}
class ActualType tok where
    infrdTypeOf :: tok a -> Maybe (LangTypeScheme a)

class UpdateType tok where
    updateType :: tok a -> LangTypeScheme a -> tok a

class SpecType tok where
    {- NB: it is recommended not to implement this because it's necessary to apply a substitution to itself before
    using it. If you (client) want to implement this, you should really care about applying substitution to itself,
    before anything (look at `specSubst`). -}
    specType :: tok a -> Substitution a -> tok a
    specType token subst =
        let lam = specLambda subst in
            token `specTypeWith` lam
    {- A Substitution value can be easily turned into a lambda using specLambda, so this is just a shorthand for
    `specType` and using this may improve performances if `specType` implementation relies on turning the Substitution
    value into a lambda. -}
    specTypeWith :: tok a -> SubstitutionLambda a -> tok a

{- Kind notion of the language. Right now, it is not precise as Haskell's kind, because it does
not capture the notions of Constraint or Unboxing. -}
data LangKind =
      {- Kind-polymorphism: it represents a kind not yet discovered, namely a "kind variable". -}
      LKVar String
      {- Kind constant. It represents any type. -}
    | LKConst
      {- Complex kinds. It represents any type. -}
    | SubLK [LangKind]

{- The kind has an obvious notion of kind. -}
instance HasKind LangKind where
    kindOf = id

instance Show LangKind where
    show (LKVar v) = v
    show LKConst = "*"
    show (SubLK ks) = "(" ++ concat (lastmap (\lk -> show lk ++ " -> ") show ks) ++ ")"

{- `promoteKind var lk` promotes kind `var` to `lk` iff `var` is a kind variable. -}
promoteKind :: LangKind -> LangKind -> LangKind
{- Trying to promote a kind with a kind variable, so there's no promotion -}
promoteKind lk (LKVar _) = lk
promoteKind (LKVar _) lk = lk
promoteKind lk _ = lk

{- The first LangKind value is the one to replace, the second one is the replacing value. -}
promoteVar :: String -> LangKind -> LangKind -> LangKind
promoteVar s (LKVar s') lk' = if s == s' then lk' else LKVar s'
promoteVar s (SubLK l) lk' = SubLK $ map (\lk -> promoteVar s lk lk') l
promoteVar _ lk _ = lk

diffKinds :: LangKind -> LangKind -> Maybe LangKind
diffKinds (SubLK ks) lk =
    case head' ks of
        Nothing -> Nothing  --Malformed kind
        Just lk' ->
            if lk' /= lk
            then Nothing
            else case tail' ks of
                Nothing -> Nothing                  --This should never happen
                Just [] -> Nothing                  --Malformed kind: it is not of the form: vk -> *
                Just [LKConst] -> Just LKConst
                Just [_] -> Nothing                 --Malformed kind: the final kind of a composite kind should be *
                Just ks' -> Just $ SubLK ks'
diffKinds _ _ = Nothing

instance Diff LangKind Maybe where
    (-\) = diffKinds
    {- (*\) operator becomes just a concatenative difference among kinds. -}
    (*\) lk [] = Just lk
    (*\) lk (lk' : t) =
        case lk -\ lk' of
            Nothing -> Nothing
            Just lk'' -> lk'' *\ t

instance Semigroup LangKind where
    (<>) = promoteKind

{- Eq instance for LangKind. You (client) should think about using EqPromote instead of this one for
a more precise comparison. -}
instance Eq LangKind where
    (==) LKConst LKConst = True
    (==) (LKVar s) (LKVar s') = s == s'
    {- NB: this case is supposed not to occur. It evaluates True because of it must be a reccall. -}
    (==) (SubLK []) (SubLK []) = True
    (==) (SubLK []) (SubLK _) = False
    (==) (SubLK _) (SubLK []) = False
    (==) (SubLK (k : kt)) (SubLK (k' : kt')) = k == k' && SubLK kt == SubLK kt'
    (==) _ _ = False

{- EqPromote instance for LangKind: it is similar to Eq except for when the LangKind values are LKVar;
whenever a variable occurs, it must be promoted to the other LangKind value. -}
instance EqPromote LangKind (Maybe [(String, LangKind)]) where
    (==^) LKConst LKConst = Just []
    (==^) LKConst (SubLK _) = Nothing
    (==^) (SubLK _) LKConst = Nothing
    (==^) (SubLK []) (SubLK []) = Just []
    (==^) (SubLK []) (SubLK _) = Nothing
    (==^) (SubLK _) (SubLK []) = Nothing
    (==^) (SubLK (k : kt)) (SubLK (k' : kt')) = case k ==^ k' of
        Nothing -> Nothing
        Just toProm -> case SubLK kt ==^ SubLK kt' of
            Just toProm' -> Just (toProm ++ toProm')
            Nothing -> Nothing
    {- The promotion must be performed whatever the value of `lk'` is. If `lk'` is a variable, then
    only one variable will appear. -}
    (==^) (LKVar v) lk' = Just [(v, lk')]
    (==^) lk (LKVar v') = Just [(v', lk)]

instance Ord LangKind where
    compare (LKVar v) (LKVar v') = compare v v'
    compare LKConst LKConst = EQ
    compare (SubLK []) (SubLK []) = EQ
    compare (SubLK []) (SubLK _) = LT
    compare (SubLK _) (SubLK []) = GT
    compare (SubLK (k : kt)) (SubLK (k' : kt')) =
        case compare k k' of
            EQ -> SubLK kt `compare` SubLK kt'
            other -> other
    compare (LKVar _) _ = LT
    compare _ (LKVar _) = GT
    compare LKConst _ = LT
    compare _ LKConst = GT

{- The right equality test between inferred kinds. -}
sameInfrdKindOf :: ActualKind tok => tok -> tok -> Bool
sameInfrdKindOf token token' =
    let lk = infrdKindOf token in
    let lk' = infrdKindOf token' in
        lk /= Nothing &&
        lk' /= Nothing &&
        lk == lk'

data LangType a =
      LATyParam (LangVarCompType a)
    | LATyComp (LangSpecType a)
    deriving Show

instance TyVars LangType where
    tyVarsOf (LATyParam lvcty) = tyVarsOf lvcty
    tyVarsOf (LATyComp lspty) = tyVarsOf lspty

    fVarsOf = tyVarsOf

    bVarsOf _ = []

    actualBVarsOf _ = []

instance HasArgs (LangType a) (LangHigherType a) where
    argsOf (LATyParam lvcty) = argsOf lvcty
    argsOf (LATyComp lspty) = argsOf lspty

instance HasHead (LangType a) (LangHigherType a) where
    headOf (LATyParam lvcty) =
        LTy . LATyParam $ LVcTy
            { specVar = headOf lvcty
            , actualVArgs = []
            , lvctyState = lvctyState lvcty
            }
    headOf (LATyComp lspty) =
        LTy . LATyComp $ LSpTy
            { specBase = headOf lspty
            , actualArgs = []
            , lstyState = lstyState lspty
            }

instance AtomRep (LangType a) where
    repOf (LATyComp lspty) = repOf lspty
    repOf (LATyParam lvcty) = repOf lvcty

instance Eq (LangType a) where
    (==) (LATyParam ty) (LATyParam ty') = ty == ty'
    (==) (LATyComp ty) (LATyComp ty') = ty == ty'
    (==) _ _ = False

instance Ord (LangType a) where
    compare (LATyParam ty) (LATyParam ty') = compare ty ty'
    compare (LATyParam _) _ = LT
    compare _ (LATyParam _) = GT
    compare (LATyComp ty) (LATyComp ty') = compare ty ty'

instance Functor LangType where
    fmap f (LATyParam ty) = LATyParam $ fmap f ty
    fmap f (LATyComp ty) = LATyComp $ fmap f ty

instance HasState LangType where
    stateOf (LATyParam ty) = stateOf ty
    stateOf (LATyComp ty) = stateOf ty

instance HasKind (LangType a) where
    kindOf (LATyParam lvcty) = kindOf lvcty
    kindOf (LATyComp lspty) = kindOf lspty

instance ActualKind (LangType a) where
    infrdKindOf (LATyParam lvcty) = infrdKindOf lvcty
    infrdKindOf (LATyComp lspty) = infrdKindOf lspty

{- A wrapper of LangType. The reason this adt exists is a bit knotty: at the user-level of the language, the
type of functions (and maybe other) is a normal type at the same level of other ones, so with an algebraic
data type definition somewhere and usable as any other algebraic data types. At compiler-level, the issue
is a bit more complicated, because there are type inference and type checking and the function type needs a
special treatment. Including it as adt and nothing more like other adts (even if it is a built-in type) makes
the type inference more complicated and it would require some hacks among modules; nothing impossible, but
it would lead to a "visibility" break among some modules. LangHigherType offers a very clean way to handle
function type. Note that in this way, there would be two ways to have function type: one with a defined adt,
one with LangHigherType. This represents the notion of mono-type. -}
data LangHigherType a =
    {- The wrapper constructor for LangType. NB: do not use this to build a LangHigherType value from a
    LangType one. -}
      LTy (LangType a)
    {- Function type in all its glory. Note that it has a list of types as arguments. The first impression
    is that it's wrong and that it should have two arguments, but this is not the right way. Maybe the best
    solution would a maximum-2-size list of types, because the order is important and the function type has
    not more than 2 arguments. The latter is the crucial point: it must be possible to have function type
    objects with one or zero arguments, because, in some contexts, it's possible to have not-*-kind types. -}
    | HApp [LangHigherType a] a
    deriving Show

showCont :: LangSpecConstraint a -> String
showCont c = tokenRepToStr (repOf c) ++ concatMap (\lhty -> " " ++ showLHTy lhty) (argsOf c)

showConts :: [LangSpecConstraint a] -> String
showConts = concat . lastmap (\c -> showCont c ++ ", ") showCont

{- Pretty printing of a type. -}
showLHTy :: LangHigherType a -> String
showLHTy (HApp args _) = "(" ++ concat (lastmap (\ty -> showLHTy ty ++ " -> ") showLHTy args) ++ ")"
showLHTy (LTy (LATyComp lspty)) =
    let args = argsOf lspty in
        if null args
        then tokenRepToStr . repOf $ headOf lspty
        else "(" ++ tokenRepToStr (repOf $ headOf lspty) ++ concatMap (\ty -> " " ++ showLHTy ty) args ++ ")"
showLHTy (LTy (LATyParam lvcty)) =
    let args = argsOf lvcty in
        if null args
        then tokenRepToStr . repOf $ headOf lvcty
        else "(" ++ tokenRepToStr (repOf $ headOf lvcty) ++ concatMap (\ty -> " " ++ showLHTy ty) args ++ ")"

showLQTy :: LangQualType a -> String
showLQTy (Qual [] lhty) = showLHTy lhty
showLQTy (Qual cs lhty) = showConts cs ++ " => " ++ showLHTy lhty
showLQTy (OnlyConstraints cs _) = showConts cs

showTyVars :: [LangVarType a] -> String
showTyVars = concat . lastmap (\lvty -> tokenRepToStr (repOf lvty) ++ ", ") (\lvty -> tokenRepToStr (repOf lvty) ++ ". ")

showLPTy :: LangTypeScheme a -> String
showLPTy (Forall [] lqty) = showLQTy lqty
showLPTy (Forall tyVars lqty) = "forall " ++ showTyVars tyVars ++ showLQTy lqty

showHeadsLHTy :: LangHigherType a -> String
showHeadsLHTy (HApp args _) = tokenRepToStr BI.nameFunctionApp ++ concatMap showHeadsLHTy args
showHeadsLHTy (LTy lty) = tokenRepToStr (repOf lty) ++ concatMap showHeadsLHTy (argsOf lty)

showHeadsLHTyWith :: LangHigherType a -> (LangVarType a -> String) -> String
showHeadsLHTyWith (HApp args _) showLvty =
    tokenRepToStr BI.nameFunctionApp ++ concatMap (`showHeadsLHTyWith` showLvty) args
showHeadsLHTyWith (LTy (LATyComp lspty)) showLvty =
    tokenRepToStr (repOf lspty) ++ concatMap (`showHeadsLHTyWith` showLvty) (argsOf lspty)
showHeadsLHTyWith (LTy (LATyParam lvcty)) showLvty =
    showLvty (headOf lvcty) ++ concatMap (`showHeadsLHTyWith` showLvty) (argsOf lvcty)

showSubst :: Substitution a -> String
showSubst subst =
    "{ " ++ concatMap (\(lvty, lhty) -> "(" ++ tokenRepToStr (repOf lvty) ++ ", " ++ showLHTy lhty ++ ")") subst ++ " }"

transformArgs :: [LangHigherType a] -> Maybe [LangHigherType a]
transformArgs = maybemap toLHTy'

unsafeTransformArgs :: [LangHigherType a] -> [LangHigherType a]
unsafeTransformArgs = map unsafeToLHTy'

{- Function to convert a LangType value into LangHigherType value. It can fail in case the number of arguments
of LangType value, when it is a function type, is inconsistent. Use this instead of directly wrapping with
`LTy` to build a LangHigherType value from a LangType value. -}
toLHTy :: LangType a -> Maybe (LangHigherType a)
toLHTy lty @ (LATyComp lspty) =
    if repOf lspty == BI.nameFunctionApp
    then buildAppLHTy <| argsOf lspty <| stateOf lty
    else Just $ LTy lty
        where
            buildAppLHTy args st =
                if length args > 2
                then Nothing
                else case transformArgs args of
                    Nothing -> Nothing
                    Just args' -> Just $ HApp args' st
toLHTy lty = Just $ LTy lty

toLHTy' :: LangHigherType a -> Maybe (LangHigherType a)
toLHTy' (LTy lty) = toLHTy lty
toLHTy' (HApp args st) =
    case transformArgs args of
        Nothing -> Nothing
        Just args' -> Just $ HApp args' st

{- Same of `toLHTy`, but it does not handle the case the type has more than two arguments. Not exported. -}
unsafeToLHTy :: LangType a -> LangHigherType a
unsafeToLHTy lty @ (LATyComp lspty) =
    if repOf lspty == BI.nameFunctionApp
    then (HApp . unsafeTransformArgs $ argsOf lspty) $ stateOf lty
    else LTy lty
unsafeToLHTy lty = LTy lty

unsafeToLHTy' :: LangHigherType a -> LangHigherType a
unsafeToLHTy' (LTy lty) = unsafeToLHTy lty
unsafeToLHTy' lhty = lhty

unsafeToLQTy :: LangQualType a -> LangQualType a
unsafeToLQTy (Qual cs lhty) = Qual cs $ unsafeToLHTy' lhty
unsafeToLQTy qualTy @ (OnlyConstraints _ _) = qualTy

{- `ifFun ty f noFun err` examines `ty` and if it is represents the function type, it passes the arguments
of the function type to the callback `f`, else it returns the default value `noFun` if it is not the function
type, otherwise an the type `ty` has been built in a bad way and `err` is returned. -}
ifFun :: LangHigherType a -> ([LangHigherType a] -> res) -> res -> res -> res
ifFun (LTy lty) f noFun err =
    case toLHTy lty of
        Nothing -> err
        Just (LTy _) -> noFun
        Just (HApp lhts _) -> f lhts
ifFun (HApp lhts _) f _ _ = f lhts

doOnType
    :: LangHigherType a
    -> (LangSpecType a -> res)
    -> ([LangHigherType a] -> res)
    -> (LangVarCompType a -> res)
    -> res
doOnType (HApp args _) _ funF _ = funF args
doOnType (LTy (LATyParam lvcty)) _ _ varF = varF lvcty
doOnType lhty @ (LTy (LATyComp lspty)) spF funF _ =
    if baseOfFunTy $ unsafeToLHTy' lhty
    then funF $ argsOf lspty
    else spF lspty

instance HasKind (LangHigherType a) where
    kindOf (LTy lty) = kindOf lty
    kindOf (HApp _ _) = SubLK [LKConst, LKConst, LKConst]

instance ActualKind (LangHigherType a) where
    infrdKindOf (LTy lty) = infrdKindOf lty
    infrdKindOf (HApp [lhty, lhty'] _) =
        if infrdKindOf lhty == Just LKConst &&
           infrdKindOf lhty' == Just LKConst
        then Just LKConst
        else Nothing
    infrdKindOf (HApp [lhty] _) =
        if infrdKindOf lhty == Just LKConst
        then Just $ SubLK [LKConst, LKConst]
        else Nothing
    infrdKindOf (HApp [] _) = Just $ SubLK [LKConst, LKConst, LKConst]
    infrdKindOf _ = Nothing

instance TyVars LangHigherType where
    tyVarsOf (LTy lty) = tyVarsOf lty
    tyVarsOf (HApp lhts _) = nub $ concatMap tyVarsOf lhts

    fVarsOf = tyVarsOf

    bVarsOf _ = []

    actualBVarsOf _ = []

instance HasType LangHigherType where
    typeOf = Forall [] . Qual []

{- It returns the type variables of a set of mono-types, ordered by occurrence. -}
occFirstTyVarsOfMany :: [LangHigherType a] -> [LangVarType a]
occFirstTyVarsOfMany = tyVarsOfMany --tyVarsOfMany already grants the order by occurrence.

instance Eq (LangHigherType a) where
    (==) (LTy ty) (LTy ty') = ty == ty'
    (==) (HApp lhts _) (HApp lhts' _) = lhts == lhts'
    (==) _ _ = False

{- This looks just for concrete types or type variables. A concrete type is defined to be greater than
a type variable. -}
instance Ord (LangHigherType a) where
    compare (LTy ty) (LTy ty') = ty `compare` ty'
    compare (HApp lhts _) (HApp lhts' _) = lhts `compare` lhts'
    compare (LTy _) _ = LT
    compare _ (LTy _) = GT

instance Semigroup (LangHigherType a) where
    (<>) lhty lhty' = HApp [lhty, lhty'] $ stateOf lhty
    sconcat = newConcatLHTy

{- Useful to get all types applied to nested function types.
NB: it should be used in a context where only types with inferred kind * are allowed. -}
unfoldType :: LangHigherType a -> [LangHigherType a]
unfoldType (HApp [] _) = []
unfoldType (HApp [ty] _) = unfoldType ty
unfoldType (HApp (ty : t) st) = ty : unfoldType (HApp t st)
unfoldType lhty = [lhty]

unfoldQualType :: LangQualType a -> ([LangSpecConstraint a], [LangHigherType a])
unfoldQualType (Qual conts lhty) = (conts, unfoldType lhty)
unfoldQualType (OnlyConstraints conts _) = (conts, [])

unfoldQualType' :: LangQualType a -> [LangQualType a]
unfoldQualType' lqty =
    let (conts, lhts) = unfoldQualType lqty in
        case lhts of
            [] -> [lqty]
            _ -> map (addContsToLHTy conts) lhts

{- NB: it does not check if the type is malformed. -}
unfoldQualTypeOnce :: LangQualType a -> (LangQualType a, Maybe (LangQualType a))
unfoldQualTypeOnce lqty @ (OnlyConstraints _ _) = (lqty, Nothing)
unfoldQualTypeOnce lqty @ (Qual _ (LTy _)) = (lqty, Nothing)
{- Next two cases are quite strange because the types have not kind *. -}
unfoldQualTypeOnce lqty @ (Qual _ (HApp [] _)) = (lqty, Nothing)
unfoldQualTypeOnce lqty @ (Qual _ (HApp [_] _)) = (lqty, Nothing)
unfoldQualTypeOnce (Qual cs (HApp [ty, ty1] _)) = (addContsToLHTy cs ty, Just $ addContsToLHTy cs ty1)
{- This case should be an error. The type is malformed. -}
unfoldQualTypeOnce (Qual cs (HApp (ty : t) st)) = (addContsToLHTy cs ty, Just . addContsToLHTy cs $ HApp t st)

unfoldTypeScheme :: LangTypeScheme a -> [LangTypeScheme a]
unfoldTypeScheme (Forall bs lqty) =
    let lqts = unfoldQualType' lqty in
        map addBinders lqts
    where
        addBinders lqty' =
            let tyVars = tyVarsOf lqty' in
                Forall (filter (`elem` tyVars) bs) lqty'

splitQualType :: LangQualType a -> ([LangSpecConstraint a], Maybe (LangHigherType a))
splitQualType (Qual cs lhty) = (cs, Just lhty)
splitQualType (OnlyConstraints cs _) = (cs, Nothing)

instance Functor LangHigherType where
    fmap f (LTy ty) = LTy $ fmap f ty
    fmap f (HApp ts st) = HApp <| map (fmap f) ts <| f st

instance HasState LangHigherType where
    stateOf (LTy lty) = stateOf lty
    stateOf (HApp _ st) = st

instance HasArgs (LangHigherType a) (LangHigherType a) where
    argsOf (LTy lty) = argsOf lty
    argsOf (HApp lhts _) = lhts

instance HasHead (LangHigherType a) (LangHigherType a) where
    headOf (LTy lty) = headOf lty
    headOf (HApp _ st) = HApp [] st

instance SpecType LangHigherType where
    specTypeWith = specVars

{- This represents the notion of mono-type along with constraints (or predicates) in the language. -}
data LangQualType a =
      Qual [LangSpecConstraint a] (LangHigherType a)
    | OnlyConstraints [LangSpecConstraint a] a deriving Show

{- NB: it removes all the constraints. -}
unconstrainedTypeOf :: LangQualType a -> Maybe (LangHigherType a)
unconstrainedTypeOf (Qual _ lhty) = Just lhty
unconstrainedTypeOf (OnlyConstraints _ _) = Nothing

addContsToLHTy :: [LangSpecConstraint a] -> LangHigherType a -> LangQualType a
addContsToLHTy [] lhty = Qual [] lhty
addContsToLHTy (c : t) lhty =
    let contsTyVars = tyVarsOf c in
    let tyVars = tyVarsOf lhty in
    let lqty' = addContsToLHTy t lhty in
        if any (`elem` tyVars) contsTyVars
        then
            case lqty' of
                Qual cs oldLhty -> Qual (c : cs) oldLhty
        else lqty'

addContsToLQTy :: [LangSpecConstraint a] -> LangQualType a -> LangQualType a
addContsToLQTy cs (Qual conts lhty) =
    case addContsToLHTy cs lhty of
        Qual cs' lhty' -> Qual (cs' ++ conts) lhty'
addContsToLQTy cs (OnlyConstraints conts st) = OnlyConstraints (cs ++ conts) st

{- NB: constraints are injected without caring of eventual free variables which would be created. -}
addContsToLPTy :: [LangSpecConstraint a] -> LangTypeScheme a -> LangTypeScheme a
addContsToLPTy cs (Forall bs qual) = Forall bs $ addContsToLQTy cs qual

instance TyVars LangQualType where
    tyVarsOf (Qual _ lhty) = tyVarsOf lhty
    tyVarsOf (OnlyConstraints cs _) = nub $ concatMap tyVarsOf cs

    fVarsOf (Qual _ lhty) = fVarsOf lhty
    fVarsOf lqty @ (OnlyConstraints _ _) = tyVarsOf lqty

    {- A mono-type has no bound variables by definition. -}
    bVarsOf _ = []

    actualBVarsOf _ = []

instance Eq (LangQualType a) where
    (==) (Qual cs lhty) (Qual cs' lhty') =
        lhty == lhty' &&
        cs `eqs` cs'
    (==) (OnlyConstraints cs _) (OnlyConstraints cs' _) = cs `eqs` cs'
    (==) _ _ = False

instance Ord (LangQualType a) where
    compare lqty lqty' =
        case (lqty, lqty') of
            (Qual cs lhty, Qual cs' lhty') ->
                case compare lhty lhty' of
                    EQ -> cmpCs cs cs'
                    other -> other
            (OnlyConstraints cs _, OnlyConstraints cs' _) ->
                cmpCs cs cs'
            (OnlyConstraints _ _, Qual _ _) -> LT
            (Qual _ _, OnlyConstraints _ _) -> GT
        where
            cmpCs cs cs' =
                if cs `eqs` cs'
                then EQ
                else compare (length cs) (length cs')

instance HasArgs (LangQualType a) (LangQualType a) where
    argsOf (Qual cs lhty) =
        let lhts = argsOf lhty in
            map (addContsToLHTy cs) lhts
    argsOf (OnlyConstraints _ _) = []

instance Semigroup (LangQualType a) where
    (<>) (Qual cs lhty) (Qual cs' lhty') =
        Qual (cs ++ cs') $ lhty <> lhty'    --TODO: duplicating constraints
    (<>) (OnlyConstraints cs _) (Qual cs' lhty') =
        Qual (cs ++ cs') lhty'
    {- Ignoring second argument. It's not possible to create a type of the form:
        C1 => mono-type => C2 -}
    (<>) lqty @ (Qual _ _) (OnlyConstraints _ _) = lqty
    (<>) (OnlyConstraints cs st) (OnlyConstraints cs' _) = OnlyConstraints (cs ++ cs') st

instance HasState LangQualType where
    stateOf (Qual [] lhty) = stateOf lhty
    stateOf (Qual (c : _) _) = stateOf c
    stateOf (OnlyConstraints _ st) = st

instance Functor LangQualType where
    fmap f (Qual cs lhty) = Qual (map (fmap f) cs) $ fmap f lhty
    fmap f (OnlyConstraints cs st) = OnlyConstraints (map (fmap f) cs) $ f st

instance HasType LangQualType where
    typeOf = Forall []

instance SpecType LangQualType where
    specTypeWith (Qual cs lhty) lam =
        let cs' = map (`specTypeWith` lam) cs in
        let lhty' = lhty `specTypeWith` lam in
            Qual cs' lhty'
    specTypeWith (OnlyConstraints cs st) lam =
        OnlyConstraints (map (`specTypeWith` lam) cs) st

{- NB: it makes lose the binders from the type which will replace the qualified type. -}
instance UpdateType LangQualType where
    updateType _ (Forall _ lqty) = lqty

instance ActualKind (LangQualType a) where
    infrdKindOf (Qual _ lhty) = infrdKindOf lhty
    {- Using kind constant for constraints only type. -}
    infrdKindOf (OnlyConstraints _ _) = Just LKConst

instance HasConstraints LangQualType where
    contsOf (Qual conts _) = conts
    contsOf (OnlyConstraints conts _) = conts

data UnificationError a =
      UnmatchTypes (LangHigherType a) (LangHigherType a)
    {- Like `UnmatchTypes`, but not strict in the failure, namely it is necessary to try swapping before returning
    the error. -}
    | TrySwap (LangHigherType a) (LangHigherType a)
    | OccursCheck (LangHigherType a) (LangHigherType a)
    | UnmatchKinds (LangHigherType a) (LangHigherType a)

{- Data to track repetitions of a type variable in a type in specialization test. For example, in the type
`m a a`, there two are occurrences of `a` and this has to be tracked in specialization test because, for instance,
we can think `f x y` is more specialized than `m a a`, but this is an error, because `x` in general is not
the same variable of `y`. -}
type VisitedVars a = Map TyVarRep (LangVarType a, LangHigherType a)

{- Just a flag to make unification stricter, namely to make it a specialization test (type t0 is more specialized -
or less general - than type t1?) -}
type IsSpecTest = Bool

{- The raw version of unification. -}
rawUnify
    :: LangHigherType a
    -> LangHigherType a
    -> IsSpecTest
    -> Either (UnificationError a) (Substitution a)
rawUnify monoTy monoTy' isSpecTest =
    case unification monoTy monoTy' empty of
        Left err -> Left err
        Right vvars -> Right $ elems vvars
    where
        {- Each time a swap action has to be performed, if the unification is strict, then it returns an error
        because this is a test on which type is more specialized (swapping them would change the result). -}
        unification lhty lhty' vvars
            | not $ sameInfrdKindOf lhty lhty' =
                Left $ UnmatchKinds lhty lhty'
            | occursCheck lhty lhty' =
                Left $ OccursCheck lhty lhty'
            {- lhty' is a concrete type and lhty is a type variable so lhty is not more specialized than lhty'. -}
            | tyVarAndConcrete lhty lhty' =
                if isSpecTest
                then Left $ UnmatchTypes lhty lhty'
                {- Swap action. -}
                else unification lhty' lhty vvars
            | bothConcrete lhty lhty' =
                if headEq lhty lhty'
                then argsUnification lhty lhty' vvars
                else Left $ UnmatchTypes lhty lhty'
            | bothTyVar lhty lhty' && argsOf lhty' > argsOf lhty =
                if isSpecTest
                then Left $ UnmatchTypes lhty lhty'
                {- Swap action. -}
                else unification lhty' lhty vvars
            | otherwise =
                argsUnification lhty lhty' vvars

        {- If it is a strict test, then two chances unification cannot be performed, because it is the same as doing
        a swap action which is forbidden with the strict test. -}
        argsUnification =
            if isSpecTest
            then unifyOnArgs
            else twoChancesUnifyArgs

        {- It tries to unify arguments. If it fails, a swap action and a second try of unification are performed.
        Think about the following situation:

            unify (M a b) (M k k)

        The first step of unification is successful, because heads (concrete types) are equal, but when unifying
        arguments, on one side type variable `k` is mapped to `a` and on the other side type variable `k` is mapped to
        `b`, which makes the types not unify. But that would be wrong, because those types can be unified with the
        following substitution:

            {(a, k), (b, k)}

        By swapping the types, exactly that substitution is got.
        It makes unification algorithm much more inefficient. -}
        twoChancesUnifyArgs lhty lhty' vvars =
            case unifyOnArgs lhty lhty' vvars of
                Left err @ (TrySwap _ _) ->
                    {- Swap action, the second chance. -}
                    case unifyOnArgs lhty' lhty vvars of
                        {- If second chance fails, then the algorithm fails with the first error. -}
                        Left _ -> Left err
                        ok @ (Right _) -> ok
                {- Any other error means that unification definitively fails. -}
                err @ (Left _) -> err
                ok @ (Right _) -> ok

        unifyOnArgs lhty lhty' vvars =
            {- Look below to see the split made by truncArgs. -}
            case truncArgs lhty lhty' of
                Nothing -> Left $ UnmatchTypes lhty lhty'
                Just (hArgs, tArgs) ->
                    let newlhty = newLHTyWithArgs lhty hArgs in
                    let newlhty' = newLHTyWithArgs lhty' [] in    --Cuting off arguments
                        if not $ sameInfrdKindOf newlhty newlhty'
                        then Left $ UnmatchKinds newlhty newlhty'
                        else
                            {- It is important that `lhty'` is inserted as variable and not `lhty`, because
                            the eventual check is "lhty is more specialized than lhty'?", so in order to be
                            more specialized of lhty', lhty' is inserted as variable (look at VisitedVars type
                            alias for more information). -}
                            case insertVar newlhty' newlhty vvars of
                                {- No map back, so a type conflict occurred, then `lhty` is not more specialized
                                than `lhty'`. -}
                                Nothing -> Left $ TrySwap newlhty newlhty'
                                {- truncArgs is then useful to do an association like, for example, the following:

                                    f t1 t2 t3 t4        f t1 t2
                                    |  |  |  |  |        |  |  |
                                    +--+--+  |  |   or   |  |  +--+--+
                                    |        |  |        |  |  |  |  |
                                    V        V  V        V  V  V  V  V
                                    g       k1 k2        g k1 k2 k3 k4

                                g has been inserted (only if it is a type variable) in the map in association
                                with (f t1 t2) and now we have recursive calls of the algorithm on (t3, k1) and
                                (t4, k2). -}
                                Just vvars' -> unifyOnSplitArgs (zipUntilEmpty tArgs $ argsOf lhty') vvars'

        unifyOnSplitArgs args vvars =
            foldl' unifyOnRes (Right vvars) args

        unifyOnRes (Right vvars) (lhty, lhty') =
            unification lhty lhty' vvars
        unifyOnRes badRes _ = badRes

        occursCheck lhty lhty' =
            {- The occurs check has to be made on both types. -}
            occursCheck' lhty lhty' ||
            occursCheck' lhty' lhty

        occursCheck' lhty lhty' =
            case (singletonVarOf lhty, singletonVarOf lhty') of
                (Nothing, Nothing) -> False
                (Just _, Just _) -> False
                (Just tyVar, Nothing) -> tyVar `elem` tyVarsOf lhty'
                (Nothing, Just tyVar') -> tyVar' `elem` tyVarsOf lhty

        {- `insertVar ty ty1 vvars` tries to insert `ty` in `vvars` if it is a type variable. If the same
        variable to insert already exists in the map, the associated type is compared with ty1 with a special
        predicate (it cares of type variable name, but not of its constraints) and if it returns true, then
        variable from `ty` is inserted with `ty1` as associated type, otherwise it returns Nothing (type
        conflict). -}
        insertVar
            :: LangHigherType a
            -> LangHigherType a
            -> VisitedVars a
            -> Maybe (VisitedVars a)
        insertVar ty ty1 vvars =
            case varOf ty of
                {- The type is not a type variable, so it has not to be inserted in the map. If you follow the
                flow of the algorithm, this case can happen only if both `ty` and `ty1` have a non-type-variable
                in the head. -}
                Nothing -> Just vvars
                Just v ->
                    let varRep = repOf v in
                        {- `ty` is a singleton type variable, because:
                            1) `ty` is supposed to have no arguments, look at the call of insertVar in unifyOnArgs;
                            2) `varOf'` returned a type variable. -}
                        if ty == ty1
                        then Just vvars
                        else case M.lookup varRep vvars of
                            Nothing -> Just $ M.insert varRep (v, ty1) vvars
                            Just (_, ty2) ->
                                if unsafeToLHTy' ty2 == unsafeToLHTy' ty1
                                then Just vvars
                                else Nothing

        {- Given two type applications:
            f t1 ... tn
            g k1 ... km
        If n == 0, it returns Nothing, else it returns a tuple where:
            - In the first component there are t1, ..., t(n-m);
            - in the second component there are t(n-m+1), ..., tn
        If n < m, then the tuple will be:
            ([], [t1, ..., tn]) -}
        truncArgs :: LangHigherType a -> LangHigherType a -> Maybe ([LangHigherType a], [LangHigherType a])
        truncArgs lhty lhty' =
            let args = argsOf lhty in
            let args' = argsOf lhty' in
            let lgh = length args in
            let lgh' = length args' in
            let diffl = lgh - lgh' in
                if lgh == 0 && lgh' /= 0
                then Nothing
                else if diffl >= 0
                then Just $ splitAt diffl args
                else {- if diffl < 0 -} Just ([], args)

        {- It zips arguments of the first list with second one until the first list has just one element; then the
        last element is zipped with the remaining elements of second list in arrows (namely, function application).
        It's supposed that:
            1) length firstList <= length secondList
            2) (null firstList) iff (null secondList) -}
        zipUntilEmpty [ty] (arg : argt) = [(ty, sconcat $ arg :| argt)]
        zipUntilEmpty (ty : t) (arg : argt) = (ty, arg) : zipUntilEmpty t argt
        {- This should occur only when both lists are empty -}
        zipUntilEmpty [] _ = []
        {- This case should never occur. -}
        zipUntilEmpty _ [] = []

        {- `newLHTyWithArgs lhty args` builds a new LangHigherType value with the head of `lhty` and
        arguments `args`. The arguments of `lhty` are discarded. -}
        newLHTyWithArgs :: LangHigherType a -> [LangHigherType a] -> LangHigherType a
        newLHTyWithArgs (HApp _ st) args = HApp args st
        newLHTyWithArgs (LTy (LATyParam lvcty)) args =
            LTy . LATyParam $ LVcTy
                { specVar = specVar lvcty
                , actualVArgs = args
                , lvctyState = lvctyState lvcty
                }
        newLHTyWithArgs (LTy (LATyComp lspty)) args =
            LTy . LATyComp $ LSpTy
                { specBase = specBase lspty
                , actualArgs = args
                , lstyState = lstyState lspty
                }

        tyVarAndConcrete :: LangHigherType a -> LangHigherType a -> Bool
        tyVarAndConcrete lhty lhty' = isTyVar lhty && isConcrete lhty'

        bothConcrete :: LangHigherType a -> LangHigherType a -> Bool
        bothConcrete lhty lhty' = isConcrete lhty && isConcrete lhty'

        bothTyVar :: LangHigherType a -> LangHigherType a -> Bool
        bothTyVar lhty lhty' = isTyVar lhty && isTyVar lhty'

        headEq :: LangHigherType a -> LangHigherType a -> Bool
        headEq (LTy (LATyComp lspty)) (LTy (LATyComp lspty')) =
            headOf lspty == headOf lspty'
        headEq lhty @ (HApp _ _) (LTy lty') =
            case toLHTy lty' of
                Nothing -> False
                Just lhty' -> headEq' lhty lhty'
        headEq (LTy lty) lhty' @ (HApp _ _) =
            case toLHTy lty of
                Nothing -> False
                Just lhty -> headEq' lhty lhty'
        headEq (HApp _ _) (HApp _ _) = True
        headEq _ _ = False

        headEq' :: LangHigherType a -> LangHigherType a -> Bool
        headEq' (HApp _ _) (HApp _ _) = True
        {- case (LTy (LATyComp _)) (LTy (LATyComp _)) cannot be there, look at headEq. -}
        headEq' _ _ = False

unify
    :: LangHigherType a
    -> LangHigherType a
    -> Either (UnificationError a) (Substitution a)
{- In general, unification is never strict, otherwise it would not be unification. -}
unify lhty lhty' = rawUnify lhty lhty' False

isMoreSpecThan
    :: LangHigherType a
    -> LangHigherType a
    -> Either (UnificationError a) (Substitution a)
isMoreSpecThan lhty lhty' = rawUnify lhty lhty' True

isLessSpecThan
    :: LangHigherType a
    -> LangHigherType a
    -> Either (UnificationError a) (Substitution a)
isLessSpecThan = flip isMoreSpecThan

exactlyTheSame :: LangHigherType a -> LangHigherType a -> Either () () -> Either (UnificationError a) (Substitution a)
exactlyTheSame lhty lhty1 which =
    case isMoreSpecThan lhty lhty1 of
        Left err -> Left err
        Right leftSubst ->
            case isMoreSpecThan lhty1 lhty of
                Left err -> Left err
                Right rightSubst ->
                    case which of
                        Left () -> Right leftSubst
                        Right () -> Right rightSubst

exactlyTheSameConstraints
    :: LangSpecConstraint a
    -> LangSpecConstraint a
    -> Either () ()
    -> Maybe (Either (UnificationError a) [Substitution a])
exactlyTheSameConstraints lspc lspc1 which =
    if cont lspc /= cont lspc1
    then Nothing
    else theSame (argsOf lspc) (argsOf lspc1) []
    where
        theSame [] [] substs = Just $ Right substs
        theSame [] _ _ = Nothing
        theSame _ [] _ = Nothing
        theSame (lhty : t) (lhty1 : t1) substs =
            case exactlyTheSame lhty lhty1 which of
                Left err -> Just $ Left err
                Right subst ->
                    let lam = specLambda subst in
                        theSame t (map (`specTypeWith` lam) t1) $ substs ++ [subst]

{-FIXME
unificationConstraintsTest
    :: LangQualType a
    -> LangQualType a
    -> Substitution a
    -> Maybe (UnificationError a)
unificationConstraintsTest lqty @ (Qual cs _) lqty' @ (Qual cs' _) subst =
    let specialize = specLambda subst in
    let substCs' = map (`specTypeWith` specialize) cs' in
        {- If `lqty` is more specialized than `lqty'`, then the constraints of `lqty` have to be a superset of
        the constraints of `lqty'`, after on the latters the substitution coming from unification (specialization
        test) is applied. -}
        if substCs' `isSubsetOf` cs
        then Nothing
        else Just $ UnmatchTypes lqty lqty'
                -}

{- FIXME
{- Specialization test for LangHigherType values with the add of check on constraints. -}
moreSpecTestWithConts
    :: LangQualType a
    -> LangQualType a
    -> Either (UnificationError a) (Substitution a)
moreSpecTestWithConts lqty lqty' =
    rawUnify lqty lqty' True unificationConstraintsTest

lessSpecTestWithConts
    :: LangQualType a
    -> LangQualType a
    -> Either (UnificationError a) (Substitution a)
lessSpecTestWithConts = flip moreSpecTestWithConts
-}

data SatisfiabilityError a =
      CouldNotDeduce (LangSpecConstraint a)
    | UndecidableInst (LangSpecConstraint a) [LangSpecConstraint a]

{- Satisfiability test for specialized constraints. The algorithm works like this:

    We have an input:
        c  = C t1 ... tn
        c' = D k1 ... km

    if C is not equal to D, no doubt, c is not satisfied by c'.
    Otherwise:
        n must be equal to m (else c is not satisfied c') and
        one by one, the specialization test (the >= one) is performed between the arguments of c (t1, ..., tn)
        and the arguments of c' (k1, ..., kn) and the resulting substitutions (if all ok) are applied eagerly
        to k1, ..., kn:

            t1       ...       tn
            |                  |
            >= --- S1   Sn --- >=
            |      |    |      |
            |     apply to     |
            |      V    V      |
            +------+----+------+
            |                  |
            k1       ...       kn

    At the end, t1, ..., tn and (S1 o ... o Sn) k1, ..., (S1 o ... o Sn) kn are compared one by one (mono-type
    equality test), if all equal, then c is satisfied by c', else c is not satisfied by c'.
-}
isSatisfied :: LangSpecConstraint a -> LangSpecConstraint a -> Bool
isSatisfied c c' =
    let cArgs = argsOf c in
    let cArgs' = argsOf c' in
        {- First of all, the property must be the same. -}
        cont c == cont c' &&
        {- Secondly, the updated arguments of c' have to be equal to arguments of c. -}
        case moreSpec1by1 cArgs cArgs' [] of
            Nothing -> False
            Just updArgs' ->
                cArgs == updArgs'
        where
            moreSpec1by1 :: [LangHigherType a] -> [LangHigherType a] -> [LangHigherType a] -> Maybe [LangHigherType a]
            moreSpec1by1 [] [] resTs = Just resTs
            moreSpec1by1 [] _ _ = Nothing        --This should never be evaluated
            moreSpec1by1 _ [] _ = Nothing        --This should never be evaluated
            moreSpec1by1 (lhty : t) (lhty' : t') ts =
                case lhty `isMoreSpecThan` lhty' of
                    Left _ -> Nothing
                    Right subst ->
                        let lam = specLambda subst in
                        let updTs = map (`specTypeWith` lam) ts in
                        let updTail = map (`specTypeWith` lam) t' in
                        let updLhty' = lhty' `specTypeWith` lam in
                            moreSpec1by1 t updTail (updTs ++ [updLhty'])

{- c is more specific then c' if c is satisfied by c', but it does not hold true that c' is satisfied by c. -}
isContMoreSpec :: LangSpecConstraint a -> LangSpecConstraint a -> Bool
isContMoreSpec c c' =
    c `isSatisfied` c' &&
    not (c' `isSatisfied` c)

isContEqualSpec :: LangSpecConstraint a -> LangSpecConstraint a -> Bool
isContEqualSpec c c' =
    c `isSatisfied` c' &&
    c' `isSatisfied` c

{- NB: this does not check the mutual satisfiability, namely it supposes at least one constraint is satisfied by the
other one. -}
contOrdSpec :: LangSpecConstraint a -> LangSpecConstraint a -> Ordering
contOrdSpec c c'
    | not $ c `isSatisfied` c' = LT
    | c' `isSatisfied` c = EQ
    | otherwise = GT

{- Normalization of constraints under the existence of a group of constraints. The algorithm is quite simple: it finds
all constraints of the context (list of constraints parameter) which the input constraint is satisfied by, then, from them,
it selects the most specific. If there's no constraint, it is an error. If there's more than one most specific constraint,
it is an error as well. -}
normalize
    :: LangSpecConstraint a
    -> [LangSpecConstraint a]
    -> Either (SatisfiabilityError a) (LangSpecConstraint a)
normalize c cs =
    let satisfiers = filter (c `isSatisfied`) cs in
        raceOnSats satisfiers [] []
    where
        {- Literally a race on the most specific constraint (of the first list, the second list is just an accumulator
        of "winners" and the third one is just of discarded constraints). If there is more than one "winner", it is an
        error, because there is not the most specific constraint which satisfies a context. If there is no one, it means
        which there is no constraint satisfying the context and this is an error as well. -}
        raceOnSats [] [] _ = Left $ CouldNotDeduce c
        raceOnSats [] [c'] _ = Right c'
        raceOnSats [] cs' _ = Left $ UndecidableInst c cs'
        raceOnSats (satC : t) curs discarded =
            if any (`isContMoreSpec` satC) $ curs ++ discarded ++ t
            then raceOnSats t curs $ satC : discarded
            else raceOnSats t (satC : curs) discarded

{- The type schemes, also known as polytypes. -}
data LangTypeScheme a = Forall [LangVarType a] (LangQualType a) deriving Show

{- NB: it removes all the binders. -}
qualTypeOf :: LangTypeScheme a -> LangQualType a
qualTypeOf (Forall _ lqty) = lqty

generalize :: LangHigherType a -> LangTypeScheme a
generalize lhty = generalize' $ Qual [] lhty

generalize' :: LangQualType a -> LangTypeScheme a
generalize' lqty = Forall (tyVarsOf lqty) lqty

generalize'' :: LangTypeScheme a -> LangTypeScheme a
generalize'' (Forall _ lqty) = Forall (tyVarsOf lqty) lqty

generalizeExcluding :: LangHigherType a -> [LangVarType a] -> LangTypeScheme a
generalizeExcluding lhty excl = Qual [] lhty `generalizeExcluding'` excl

generalizeExcluding' :: LangQualType a -> [LangVarType a] -> LangTypeScheme a
generalizeExcluding' lqty excl = Forall (filter (`notElem` excl) $ tyVarsOf lqty) lqty

{- Generalization on type schemes. If the type scheme token has been already generalized, the call should cause
no effect on the final type. This is useful for type schemes which are just the lifted version of a qualified
type (look at liftQualType). -}
generalizeExcluding'' :: LangTypeScheme a -> [LangVarType a] -> LangTypeScheme a
generalizeExcluding'' lpty @ (Forall bVars lqty) excl =
    Forall (filter (`notElem` excl) (fVarsOf lpty) ++ bVars) lqty

divideContsToNorm
    :: [(LangSpecConstraint a, extra)]
    -> LangHigherType a
    -> [LangVarType a]
    -> ([(LangSpecConstraint a, extra)], [(LangSpecConstraint a, extra)])
divideContsToNorm cs lhty excl =
    let lvts = tyVarsOf lhty in
        partition (genCondition lvts) cs
    where
        {- The constraints to normalize are the ones which have no type variables or at least one variable that
        does not appear in the mono-type AND is not excluded. --TODO: verify the last condition -}
        genCondition lvts c =
            let cLvts = tyVarsOf $ fst c in
                null cLvts || any (\v -> v `notElem` lvts && v `notElem` excl) cLvts

{- It generalizes the constraints and the mono-type like they were a qualified type, but it firstly checks if the
implication is possible, namely it checks the type variables of the constraints are a subset of the type variables
of mono-type. If this does not hold true, it returns the type scheme (the generalized version of the mono-type)
separated from the constraints which need the normalization. The third parameter is the type variables to exclude
during generalization. Constraints has an extra attached information. -}
generalizeAndTryImply
    :: [(LangSpecConstraint a, extra)]
    -> LangHigherType a
    -> [LangVarType a]
    -> ([(LangSpecConstraint a, extra)], [(LangSpecConstraint a, extra)], LangTypeScheme a)
generalizeAndTryImply cs lhty excl =
    let (toNorm, notToNorm) = divideContsToNorm cs lhty excl in
         (toNorm, notToNorm, Qual (map fst notToNorm) lhty `generalizeExcluding'` excl)

liftMonoType :: LangHigherType a -> LangTypeScheme a
liftMonoType = Forall [] . Qual []

{- It returns the lifted version of a qualified type, namely a polytype with no bound variables. -}
liftQualType :: LangQualType a -> LangTypeScheme a
liftQualType = Forall []

instantiate :: LangTypeScheme a -> [(LangVarType a, LangVarType a)] -> LangQualType a
instantiate lpty subst =
    case lpty of
        Forall _ (Qual cs lhty) ->
            {- The unwrap is safe, because builtF is always successful (namely it never returns a Nothing value). -}
            let cs' = fst . unwrap $ visitVarsInLSpConts cs () builtF in
            let lhty' = fst . unwrap $ visitVarsInLHTy lhty () builtF in
                Qual cs' lhty'
        Forall _ (OnlyConstraints cs st) ->
            OnlyConstraints (fst . unwrap $ visitVarsInLSpConts cs () builtF) st
    where
        builtF lvty _ =
            case firstThat (\(lvty', _) -> repOf lvty' == repOf lvty) subst of
                Nothing -> Just (lvty, ())
                Just (_, replLvty) -> Just (replLvty, ())

        unwrap (Just x) = x

instantiateUnqualifying :: LangTypeScheme a -> [(LangVarType a, LangVarType a)] -> Maybe (LangHigherType a)
instantiateUnqualifying (Forall binders (Qual _ lhty)) subst =
    let lqty = Forall binders $ Qual [] lhty in
        case instantiate lqty subst of
            Qual _ lhty' -> Just lhty'
            OnlyConstraints _ _ -> Nothing
instantiateUnqualifying (Forall _ (OnlyConstraints _ _)) _ = Nothing

{- NB: this is terribly UNSAFE and should be used only when it's necessary to get the type resulting from the
application of a value with a constraints-only type. -}
removeOneConstraint :: LangTypeScheme a -> LangTypeScheme a
removeOneConstraint (Forall bs qual @ (Qual [] _)) = Forall bs qual
removeOneConstraint (Forall bs (Qual (_ : t) st)) = Forall bs $ Qual t st
removeOneConstraint (Forall bs qual @ (OnlyConstraints [] _)) = Forall bs qual
removeOneConstraint (Forall bs (OnlyConstraints (_ : t) st)) = Forall bs $ OnlyConstraints t st

removeAllConstraints :: LangTypeScheme a -> LangTypeScheme a
removeAllConstraints (Forall bs (Qual _ lhty)) = Forall bs $ Qual [] lhty
removeAllConstraints (Forall bs (OnlyConstraints _ st)) = Forall bs $ OnlyConstraints [] st

instance TyVars LangTypeScheme where
    fVarsOf (Forall lvts lqty) = filter (`notElem` lvts) $ tyVarsOf lqty

    bVarsOf (Forall lvts _) = nub lvts

    actualBVarsOf lpty @ (Forall _ lqty) =
        let boundVars = bVarsOf lpty in
        let actualVars = tyVarsOf lqty in
            filter (`elem` actualVars) boundVars

instance Semigroup (LangTypeScheme a) where
    {- Concatenating a type scheme is quite complex because it's necessary to care of free type variables and the
    bound variables with the same terms; so the algorithm works like this, given `lpty <> lpty1`:
        - the binders of `lpty` are changed (and all its bound variables); they are changed with new terms in order
          not to clash with terms of `lpty1` (anyone) and free variables of `lpty`;
        - now, we just updated `lpty` into `lpty'`;
        - the same thing is performed with `lpty1` binders (and its bound variables); this time the new created
          variables have not to clash with type variables of `lpty'` and free type variables of `lpty1`;
        - now, we have `lpty'` and `lpty1'`;
        - the binders of the new type scheme are just the binders of `lpty'` plus the binders of `lpty1'`;
        - we can safely concatenate the qualified types of `lpty'` and `lpty1'`. -}
    (<>) lpty lpty1 =
        let (lpty', c) = replacePolyType (fVarsOf lpty ++ tyVarsOf lpty1) lpty (C.new :: C.AlphabeticCounterObj) in
        {- Passing the previous counter in order to perform less recursive calls (look below at `changeTyVarsLam`).
        Using a new counter instead of the old one would make more recursive calls because, it's probably to create
        new variables equal to the type variables of lpty' (according to the type variables equality which compares
        the terms). -}
        let (lpty1', _) = replacePolyType (fVarsOf lpty1 ++ tyVarsOf lpty') lpty1 c in
            case (lpty', lpty1') of
                (Forall binders lqty, Forall binders1 lqty1) ->
                    Forall (binders ++ binders1) $ lqty <> lqty1
        where
            replacePolyType tyVars (Forall binders lqty) c =
                let (c', replMap) = changeTyVars binders tyVars c in
                let replace = replLam replMap in
                let binders' = visitVars'' binders replace in
                let lqty' = replaceQualType lqty replace in
                    (Forall binders' lqty', c')

            replaceQualType (Qual cs lhty) replace =
                let cs' = map (`visitVars'` replace) cs in
                let lhty' = visitVars lhty replace in
                    Qual cs' lhty'
            replaceQualType (OnlyConstraints cs st) replace =
                let cs' = map (`visitVars'` replace) cs in
                    OnlyConstraints cs' st

            replLam m lvty =
                case M.lookup (repOf lvty) m of
                    Nothing -> lvty
                    Just lvty' -> lvty'

            changeTyVars lvts tyVars c =
                foldl' (changeTyVar tyVars) (c, empty) lvts

            changeTyVar tyVars (c, m) lvty =
                let (lvty', c') = changeTyVarsLam tyVars c lvty in
                    if lvty == lvty'
                    {- No new type variable has been created if they are equal, so it's not necessary to perform
                    new insertion operations. -}
                    then (c', m)
                    else
                        let m' = M.insert (repOf lvty) lvty' m in
                            (c', m')

            changeTyVarsLam tyVars c lvty =
                if lvty `notElem` tyVars
                then (lvty, c)
                else
                    {- Creating new variables with the help of a counter. Try creating new variables until
                    no type variable of `tyVars` matches the just created one. -}
                    let (varRep, c') = C.next c in
                    let lvty' =
                         LVTy
                            { var = varRep
                            , kind = kind lvty
                            , lvtyRole = lvtyRole lvty
                            , lvtyState = lvtyState lvty
                            } in
                        changeTyVarsLam tyVars c' lvty'

{- NB: this is very UNSAFE! It loses the notion of bound variables. -}
instance HasConstraints LangTypeScheme where
    contsOf (Forall _ lqty) = contsOf lqty

instance HasState LangTypeScheme where
    {- Note that the state is given by the first token which occurs. -}
    stateOf (Forall _ lqty) = stateOf lqty

instance Functor LangTypeScheme where
    fmap f (Forall tyVars lqty) = Forall (map (fmap f) tyVars) $ fmap f lqty

instance HasType LangTypeScheme where
    typeOf = id

instance SpecType LangTypeScheme where
    specTypeWith lpty lam =
        case noClashesLpty of
            Forall lvts lqty ->
                let newLqty = lqty `specTypeWith` builtLam in
                    Forall lvts newLqty
        where
            {- Handling type variables of resulting mono-type which would be accidentally bound. So changing the
            binders when necessary. -}
            noClashesLpty =
                let bVars = bVarsOf lpty in
                let fVars = fVarsOf lpty in
                    changeLpty lpty bVars $ tyVarsToChange bVars fVars

            tyVarsToChange _ [] = []
            tyVarsToChange bVars (freeVar : t) =
                case builtLam freeVar of
                    Nothing -> tyVarsToChange bVars t
                    Just lhty -> filter (`elem` bVars) (tyVarsOf lhty) ++ tyVarsToChange bVars t

            changeLpty lpty' [] _ = lpty'
            changeLpty lpty' (lvty : t) substTyVars =
                let lvty' = whileClashing lvty substTyVars in
                let updLpty = visitVarsOfLPTy lpty' $ updateVar lvty lvty' in
                    changeLpty updLpty t substTyVars

            updateVar lvtyToChange newLvty =
                \lvty ->
                    if lvty == lvtyToChange
                    then newLvty
                    else lvty

            whileClashing lvty tyVars =
                if lvty `elem` tyVars
                then
                    let lvty' =
                         lvty
                            { var = var lvty ++ show (0 :: Integer)
                            } in
                        whileClashing lvty' tyVars
                else lvty

            builtLam lvty =
                let boundVars = bVarsOf lpty in
                    {- Bound variables have to remain untouched. -}
                    if lvty `elem` boundVars
                    then Nothing
                    else lam lvty

{- The reason why this newtype exists is that it's necessary to have a Show instance of Role data type, in order to
deriving Show instance automatically for LanVarType data type, but defining a Show instance for Role data type would
result in an orphan instance. -}
newtype Role_ = Role_ Role

convertRole_ :: Role_ -> Role
convertRole_ (Role_ r) = r

instance Show Role_ where
    show (Role_ Nominal) = "Nominal"
    show (Role_ Representational) = "Representational"
    show (Role_ Phantom) = "Phantom"

data LangVarType a =
    LVTy
        { var :: String
        , kind :: LangKind
        , lvtyRole :: Role_
        , lvtyState :: a
        } deriving Show

roleOf :: LangVarType a -> Role
roleOf = convertRole_ . lvtyRole

instance AtomRep (LangVarType a) where
    repOf = tokenRepFromStr . var

instance HasState LangVarType where
    stateOf = lvtyState

instance HasKind (LangVarType a) where
    kindOf = kind

instance ActualKind (LangVarType a) where
    infrdKindOf = Just . kindOf

instance Eq (LangVarType a) where
    (==) lvty lvty' = var lvty == var lvty'

instance Ord (LangVarType a) where
    compare lvty lvty' = kindOf lvty `compare` kindOf lvty'

instance Functor LangVarType where
    fmap f lvty =
        LVTy
            { var = var lvty
            , kind = kind lvty
            , lvtyRole = lvtyRole lvty
            , lvtyState = f $ lvtyState lvty
            }

{- Application of a substitution to itself. -}
specSubst :: Substitution a -> Substitution a
specSubst [] = []
specSubst ((lvty, lhty) : t) =
    (lvty, lhty) : specSubst (map specialize t)
    where
        specialize (origLvty, lhtyToSubst) =
            (origLvty, specVars lhtyToSubst builtLam)

        builtLam lvty' =
            if lvty == lvty'
            then Just lhty
            else Nothing

{- Just a way to convert a list of type variables to specialize into a lambda that does it. -}
specLambda :: Substitution a -> SubstitutionLambda a
specLambda toSpec tyVar =
    {- NB: before applying the substitution, the substitution is applied to itself, because it can happen a case where
    a type variable which has to be replaced is placed (in the list) in a single substitution which comes before
    single substitutions where that type variable appears. For example:

        [(a, Int), (b, a -> Int)]

    here, type variable `a` (which has to be replaced by type `Int`) appears later in the list.
    -}
    let toSpec' = specSubst toSpec in
        case firstThat (\(tyVar', _) -> repOf tyVar' == repOf tyVar) toSpec' of
            Just (_, ty) -> Just ty
            Nothing -> Nothing

{- `specVars lqty spec` specializes all the type variables which occur in `lqty` and which applied to `spec`
does return a LangQualType value (that is the specializer). -}
specVars
    :: LangHigherType a
    -> SubstitutionLambda a
    -> LangHigherType a
specVars monoTy spec =
    case monoTy of
        (HApp lhts st) ->
            let newArgs = map (`specVars` spec) lhts in
                HApp newArgs st
        (LTy lty) ->
            specInLTy lty
    where
        specInLTy (LATyParam lvcty) = specInLVcTy lvcty
        specInLTy (LATyComp lspty) = specInLSpTy lspty

        specInLVcTy lvcty =
            let newArgs = map (`specVars` spec) $ argsOf lvcty in
            let lvty = specVar lvcty in
                case spec lvty of
                    Nothing ->
                        LTy . LATyParam $ LVcTy
                            { specVar = lvty
                            , actualVArgs = newArgs
                            , lvctyState = lvctyState lvcty
                            }
                    Just newHead -> concatTs newHead newArgs

        specInLSpTy lspty =
            let newArgs = map (`specVars` spec) $ argsOf lspty in
                LTy . LATyComp $ LSpTy
                    { specBase = specBase lspty
                    , actualArgs = newArgs
                    , lstyState = lstyState lspty
                    }

        concatTs :: LangHigherType a -> [LangHigherType a] -> LangHigherType a
        concatTs (HApp lhts st) lhts' = HApp (lhts ++ lhts') st
        concatTs (LTy lty) lhts' = concatLty lty lhts'

        concatLty :: LangType a -> [LangHigherType a] -> LangHigherType a
        concatLty (LATyParam lvcty) lhts' =
            LTy . LATyParam $ LVcTy
                { specVar = specVar lvcty
                , actualVArgs = actualVArgs lvcty ++ lhts'
                , lvctyState = lvctyState lvcty
                }
        concatLty (LATyComp lspty) lhts' =
            LTy . LATyComp $ LSpTy
                { specBase = specBase lspty
                , actualArgs = actualArgs lspty ++ lhts'
                , lstyState = lstyState lspty
                }

specVarsComp :: LangHigherType a -> [SubstitutionLambda a] -> LangHigherType a
specVarsComp = foldl specVars

visitVars :: LangHigherType a -> (LangVarType a -> LangVarType a) -> LangHigherType a
visitVars (HApp lhts st) f = HApp <| map (`visitVars` f) lhts <| st
visitVars (LTy lty) f = LTy $ visitInLTy lty
    where
        visitInLTy (LATyParam lvcty) = LATyParam $ visitInLVcTy lvcty
        visitInLTy (LATyComp lspty) = LATyComp $ visitInLSpTy lspty

        visitInLVcTy lvcty =
            LVcTy
                { specVar = f $ specVar lvcty
                , actualVArgs = map (`visitVars` f) $ actualVArgs lvcty
                , lvctyState = lvctyState lvcty
                }

        visitInLSpTy lspty =
            LSpTy
                { specBase = specBase lspty
                , actualArgs = map (`visitVars` f) $ actualArgs lspty
                , lstyState = lstyState lspty
                }

visitVars' :: LangSpecConstraint a -> (LangVarType a -> LangVarType a) -> LangSpecConstraint a
visitVars' lspc f =
    let args = lscontArgs lspc in
    let updArgs = map (`visitVars` f) args in
        lspc
            { lscontArgs = updArgs
            }

visitVars'' :: [LangVarType a] -> (LangVarType a -> LangVarType a) -> [LangVarType a]
visitVars'' vars f = map f vars

visitVarsOfLPTy :: LangTypeScheme a -> (LangVarType a -> LangVarType a) -> LangTypeScheme a
visitVarsOfLPTy (Forall bs (Qual cs lhty)) f =
    Forall <| visitVars'' bs f <| Qual (map (`visitVars'` f) cs) (visitVars lhty f)
visitVarsOfLPTy (Forall bs (OnlyConstraints cs st)) f =
    Forall <| visitVars'' bs f <| OnlyConstraints (map (`visitVars'` f) cs) st

{- `addContsToLVTy lvty cs` updates type variable `lvty` with new constraints `cs`. It adds the passed
constraints `cs` iff they have a type variable which is the same of `lvty` (string comparison) and the
same constraint does not appear in `lvty`.
NB: this is UNSAFE, because it changes a token. -}
{-FIXME
addContsToLVTy :: LangVarType a -> [LangSpecConstraint a] -> LangVarType a
addContsToLVTy lvty [] = lvty
addContsToLVTy lvty (sc : t) = addContsToLVTy addCont t
    where
        addCont =
            let cs = constraints lvty in
                {- Not dup the constraints in the type variable. -}
                if sc `stcElem` cs ||
                   not occurVarInCont
                then lvty
                else LVTy
                    { var = var lvty
                    , kind = kind lvty
                    , lvtyRole = lvtyRole lvty
                    , lvtyState = lvtyState lvty
                    }

        stcElem :: EqStrict a => a -> [a] -> Bool
        stcElem x l = isJust $ find (==. x) l

        occurVarInCont = repOf lvty `elem` (map repOf . concatMap tyVarsOf $ argsOf sc)
                -}

{-FIXME
addContsToLHTy :: LangHigherType a -> [LangSpecConstraint a] -> LangHigherType a
addContsToLHTy lhty cs = visitVars lhty (`addContsToLVTy` cs)
-}

data LangVarCompType a =
    LVcTy
        { specVar :: LangVarType a
        , actualVArgs :: [LangHigherType a]
        , lvctyState :: a
        } deriving Show

instance TyVars LangVarCompType where
    tyVarsOf lvcty = nub $ headOf lvcty : concatMap tyVarsOf (argsOf lvcty)

    fVarsOf = tyVarsOf

    bVarsOf _ = []

    actualBVarsOf _ = []

instance HasArgs (LangVarCompType a) (LangHigherType a) where
    argsOf = actualVArgs

instance HasHead (LangVarCompType a) (LangVarType a) where
    headOf = specVar

instance AtomRep (LangVarCompType a) where
    repOf = repOf . specVar

instance HasState LangVarCompType where
    stateOf = lvctyState

instance Eq (LangVarCompType a) where
    (==) lvcty lvcty' =
        specVar lvcty == specVar lvcty' &&
        actualVArgs lvcty == actualVArgs lvcty'

instance Ord (LangVarCompType a) where
    compare lvcty lvcty' =
        case specVar lvcty `compare` specVar lvcty' of
            EQ -> actualVArgs lvcty `compare` actualVArgs lvcty'
            other -> other

instance Functor LangVarCompType where
    fmap f lvcty =
        LVcTy
            { specVar = f <$> specVar lvcty
            , actualVArgs = map (fmap f) $ actualVArgs lvcty
            , lvctyState = f $ lvctyState lvcty
            }

instance HasKind (LangVarCompType a) where
    kindOf = kindOf . specVar

{- It calculates the actual kind of a composite token (already split). NB: given the expression `kindOfComp tok args`,
it is supposed that `tok` and `args` were a unique token. -}
kindOfComp :: HasKind ty => ty -> [LangHigherType a] -> Maybe LangKind
kindOfComp tok args =
    forAll args tryInferKind `startingFrom` Just (kindOf tok)
        where
            tryInferKind Nothing _ = Nothing
            tryInferKind (Just lk) lhty =
                case infrdKindOf lhty of
                    Nothing -> Nothing
                    Just infrdlk -> diffKinds lk infrdlk

instance ActualKind (LangVarCompType a) where
    infrdKindOf
        LVcTy
            { specVar = lvty
            , actualVArgs = args
            , lvctyState = _
            } = kindOfComp lvty args

{- The "pure constraint" notion of the language (currently it is not polymorphic). -}
data LangNewConstraint a =
    LNCont
        { prop :: String
        {- The associated variables which should match a type. Why type vars?
        The information which is useful to build a constraint is just the kinds
        and the specialized constraints, so here `LangVarType` is used just as
        a shorthand to bring this type of data. -}
        , contVars :: [LangVarType a]
        , lcontState :: a
        } deriving Show

instance HasArgs (LangNewConstraint a) (LangVarType a) where
    argsOf = contVars

instance HasHead (LangNewConstraint a) String where
    headOf = prop

instance HasState LangNewConstraint where
    stateOf = lcontState

instance AtomRep (LangNewConstraint a) where
    repOf = tokenRepFromStr . prop

instance Eq (LangNewConstraint a) where
    (==) lnc lnc' = prop lnc == prop lnc'

instance Ord (LangNewConstraint a) where
    compare lnc lnc' = prop lnc `compare` prop lnc'

instance Functor LangNewConstraint where
    fmap f lnc =
        LNCont
            { prop = prop lnc
            , contVars = map (fmap f) $ contVars lnc
            , lcontState = f $ lcontState lnc
            }

{- Infinite constraints can be created, given a new one (as types). -}
data LangSpecConstraint a =
    LSCont
        { cont :: LangNewConstraint a
        , lscontArgs :: [LangHigherType a]
        , lscontState :: a
        } deriving Show

instance TyVars LangSpecConstraint where
    tyVarsOf c = nub . concatMap tyVarsOf $ lscontArgs c

    fVarsOf = tyVarsOf

    bVarsOf _ = []

    actualBVarsOf _ = []

instance SpecType LangSpecConstraint where
    specTypeWith lspc lam =
        let lhts = lscontArgs lspc in
            lspc
                { lscontArgs = map (`specTypeWith` lam) lhts
                }

instance Eq (LangSpecConstraint a) where
    (==) lspc lspc' =
        cont lspc == cont lspc' &&
        lscontArgs lspc == lscontArgs lspc'

isSubsetOf :: [LangSpecConstraint a] -> [LangSpecConstraint a] -> Bool
isSubsetOf cs cs' = all (`elem` cs') cs

eqs :: [LangSpecConstraint a] -> [LangSpecConstraint a] -> Bool
eqs cs cs' = cs `isSubsetOf` cs' && cs' `isSubsetOf` cs

instance Ord (LangSpecConstraint a) where
    compare lspc lspc' =
        case cont lspc `compare` cont lspc' of
            EQ -> lscontArgs lspc `compare` lscontArgs lspc'
            other -> other

instance AtomRep (LangSpecConstraint a) where
    repOf = repOf . cont

instance Functor LangSpecConstraint where
    fmap f lsc =
        LSCont
            { cont = f <$> cont lsc
            , lscontArgs = map (fmap f) $ lscontArgs lsc
            , lscontState = f $ lscontState lsc
            }

instance HasHead (LangSpecConstraint a) (LangNewConstraint a) where
    headOf = cont

instance HasArgs (LangSpecConstraint a) (LangHigherType a) where
    argsOf = lscontArgs

instance HasState LangSpecConstraint where
    stateOf = lscontState

instance HasConstraints LangSpecConstraint where
    contsOf c = [c]

data LangNewType a =
    LNTy
        { base :: String
        , kind' :: LangKind
        , params :: [LangVarType a]
        {- If `infinite` field is set to true, then the type can have an arbitrary number of
        arguments. Each argument should have the kind denoted by `kind'` field. If `params` is
        empty, then arguments must have the kind *. -}
        , infinite :: Bool
        , lntyState :: a
        } deriving Show

instance AtomRep (LangNewType a) where
    repOf = tokenRepFromStr . base

instance HasState LangNewType where
    stateOf = lntyState

instance HasHead (LangNewType a) String where
    headOf = base

instance HasArgs (LangNewType a) (LangVarType a) where
    argsOf = params

instance HasKind (LangNewType a) where
    kindOf = kind'

{- This should not be useful in several occasions, since the actual kind of a type with all its
arguments is always *, but it can be used to check the correctness of a LangNewType token. -}
instance ActualKind (LangNewType a) where
    {-infrdKindOf LNTy { base = _
                     , kind' = lk
                     , params = ps
                     , infinite = _
                     , lntyState = _
                     } =-}
    infrdKindOf lnty =
        case (kind' lnty, params lnty) of
            (SubLK ks, ps) ->
                if length ks + 1 == length ps
                then Just LKConst
                else Nothing
            (LKVar _, _) -> Nothing
            (LKConst, []) -> Just LKConst
            (LKConst, _) -> Nothing

{- This works on the hypothesis that there is only one type with a certain name. -}
instance Eq (LangNewType a) where
    (==) lnty lnty' = base lnty == base lnty'

instance Ord (LangNewType a) where
    compare lnty lnty' = base lnty `compare` base lnty'

instance Functor LangNewType where
    fmap f lnty =
        LNTy
            { base = base lnty
            , kind' = kind' lnty
            , params = map (fmap f) $ params lnty
            , infinite = infinite lnty
            , lntyState = f $ lntyState lnty
            }

data LangSpecType a =
    LSpTy
        { specBase :: LangNewType a
        , actualArgs :: [LangHigherType a]
        , lstyState :: a
        } deriving Show

instance HasKind (LangSpecType a) where
    {- The kind of a LangSpecType token is simply "inherited" from what is specialized. -}
    kindOf = kindOf . specBase

instance TyVars LangSpecType where
    tyVarsOf lspty = nub $ concatMap tyVarsOf $ argsOf lspty

    fVarsOf = tyVarsOf

    bVarsOf _ = []

    actualBVarsOf _ = []

instance ActualKind (LangSpecType a) where
    infrdKindOf
        LSpTy
            { specBase = lnty
            , actualArgs = args
            , lstyState = _
            } = kindOfComp lnty args

instance Eq (LangSpecType a) where
    (==) lspty lspty' =
        specBase lspty == specBase lspty' &&
        actualArgs lspty == actualArgs lspty'

instance Ord (LangSpecType a) where
    compare lspty lspty' =
        case specBase lspty `compare` specBase lspty' of
            EQ -> actualArgs lspty `compare` actualArgs lspty'
            other -> other

instance Functor LangSpecType where
    fmap f lspty =
        LSpTy
            { specBase = f <$> specBase lspty
            , actualArgs = map (fmap f) $ actualArgs lspty
            , lstyState = f $ lstyState lspty
            }

instance HasState LangSpecType where
    stateOf = lstyState

instance AtomRep (LangSpecType a) where
    repOf = repOf . specBase

instance HasArgs (LangSpecType a) (LangHigherType a) where
    argsOf = actualArgs

instance HasHead (LangSpecType a) (LangNewType a) where
    headOf = specBase

data NotedVar a =
    NotedVar
        { nVarName :: String
        , nVarType :: LangTypeScheme a
        , nVarState :: a
        } deriving Show

instance AtomRep (NotedVar a) where
    repOf = tokenRepFromStr . nVarName

instance Eq (NotedVar a) where
    (==) nVar nVar' = nVarName nVar == nVarName nVar'

instance Ord (NotedVar a) where
    compare nVar nVar' = nVarName nVar `compare` nVarName nVar'

instance HasState NotedVar where
    stateOf = nVarState

instance Functor NotedVar where
    fmap f nVar =
        NotedVar
            { nVarName = nVarName nVar
            , nVarType = f <$> nVarType nVar
            , nVarState = f $ nVarState nVar
            }

instance HasType NotedVar where
    typeOf = nVarType

instance ActualType NotedVar where
    infrdTypeOf = Just . typeOf

instance UpdateType NotedVar where
    updateType nVar lhty =
        NotedVar
            { nVarName = nVarName nVar
            , nVarType = lhty
            , nVarState = nVarState nVar
            }

instance SpecType NotedVar where
    specTypeWith nVar lam =
        let lpty = typeOf nVar in
            updateType nVar $ lpty `specTypeWith` lam

data NotedLiteral =
      --TODO: keep an Integer value instead of Int
      LitInt Int
    | LitDouble Double
    | LitChar Char
    | LitString String
    deriving Show

instance Eq NotedLiteral where
    (==) (LitInt i) (LitInt i') = i == i'
    (==) (LitDouble f) (LitDouble f') = f == f'
    (==) (LitChar c) (LitChar c') = c == c'
    (==) (LitString s) (LitString s') = s == s'
    (==) _ _ = False

instance AtomRep NotedLiteral where
    repOf (LitInt i) = tokenRepFromStr $ show i
    repOf (LitDouble f) = tokenRepFromStr $ show f
    repOf (LitChar c) = tokenRepFromStr $ show c
    repOf (LitString s) = tokenRepFromStr $ show s

data OnlyConstraintScheme a = ContsOnlyTy [LangVarType a] (LangSpecConstraint a) a deriving Show

instance Functor OnlyConstraintScheme where
    fmap f (ContsOnlyTy lvts c st) = ContsOnlyTy (map (fmap f) lvts) (fmap f c) $ f st

instance HasConstraints OnlyConstraintScheme where
    contsOf (ContsOnlyTy _ c _) = [c]

onlyConstraintToScheme :: OnlyConstraintScheme a -> LangTypeScheme a
onlyConstraintToScheme (ContsOnlyTy binders c st) = Forall binders $ OnlyConstraints [c] st

schemeToOnlyConstraint :: LangTypeScheme a -> Maybe (OnlyConstraintScheme a)
schemeToOnlyConstraint (Forall binders (OnlyConstraints [c] st)) = Just $ ContsOnlyTy binders c st
schemeToOnlyConstraint _ = Nothing

newConstraintScheme :: [LangVarType a] -> LangSpecConstraint a -> a -> OnlyConstraintScheme a
newConstraintScheme = ContsOnlyTy

{- The notion of identifiers which are not variables in a program along with a type. It includes constructors and
literals, removing that difference standing at syntax level. -}
data NotedVal a =
      NotedLit NotedLiteral (LangTypeScheme a) a
    | NotedVal String (LangTypeScheme a) a
    deriving Show

instance SpecType NotedVal where
    specTypeWith nVal lam =
        let lpty = typeOf nVal in
            updateType nVal $ lpty `specTypeWith` lam

instance Eq (NotedVal a) where
    (==) (NotedLit lit _ _) (NotedLit lit' _ _) = lit == lit'
    (==) (NotedVal rep _ _) (NotedVal rep' _ _) = rep == rep'
    (==) _ _ = False

instance Functor NotedVal where
    fmap f (NotedLit lit lhty st) = NotedLit lit (fmap f lhty) $ f st
    fmap f (NotedVal strName lhty st) = NotedVal strName (fmap f lhty) $ f st

instance HasState NotedVal where
    stateOf (NotedLit _ _ st) = st
    stateOf (NotedVal _ _ st) = st

instance AtomRep (NotedVal a) where
    repOf (NotedLit lit _ _) = repOf lit
    repOf (NotedVal strName _ _) = tokenRepFromStr strName

instance HasType NotedVal where
    typeOf (NotedLit _ lpty _) = lpty
    typeOf (NotedVal _ lpty _) = lpty

doOnVal
    :: NotedVal a
    -> (String -> LangTypeScheme a -> a -> res)
    -> (NotedLiteral -> LangTypeScheme a -> a -> res)
    -> res
doOnVal nVal withNonLit withLit =
    case nVal of
        (NotedLit lit lpty st) -> withLit lit lpty st
        (NotedVal valRep lpty st) -> withNonLit valRep lpty st

nValArgsNumber :: NotedVal a -> Int
nValArgsNumber nVal =
    let (_, lhts) = unfoldQualType . qualTypeOf $ typeOf nVal in
        length lhts - 1

instance ActualType NotedVal where
    infrdTypeOf = Just . typeOf

instance UpdateType NotedVal where
    updateType (NotedLit lit _ st) lpty = NotedLit lit lpty st
    updateType (NotedVal strName _ st) lpty = NotedVal strName lpty st

data DispatchTok a =
      DispatchVar (NotedVar a) a
    | DispatchVal (OnlyConstraintScheme a) a
    deriving Show

instance Eq (DispatchTok a) where
    (==) (DispatchVar nVar _) (DispatchVar nVar' _) = nVar == nVar'
    (==) (DispatchVal (ContsOnlyTy _ c _) _) (DispatchVal (ContsOnlyTy _ c' _) _) =
        case exactlyTheSameConstraints c c' $ Left () of
            Just (Right _) -> True
            _ -> False
    (==) _ _ = False

instance Functor DispatchTok where
    fmap f (DispatchVar nVar st) = DispatchVar (fmap f nVar) $ f st
    fmap f (DispatchVal scheme st) = DispatchVal (fmap f scheme) $ f st

instance HasState DispatchTok where
    stateOf (DispatchVar _ st) = st
    stateOf (DispatchVal _ st) = st

instance AtomRep (DispatchTok a) where
    repOf (DispatchVar nVar _) =
        tokenRepFromStr $ nVarName nVar
    repOf (DispatchVal (ContsOnlyTy _ c _) _) =
        tokenRepFromStr . prop $ headOf c    --TODO: check correctness

instance HasType DispatchTok where
    typeOf (DispatchVar nVar _) = typeOf nVar
    typeOf (DispatchVal scheme _) = onlyConstraintToScheme scheme

instance SpecType DispatchTok where
    specTypeWith dispTok lam =
        let lpty = typeOf dispTok in
            updateType dispTok $ lpty `specTypeWith` lam

instance UpdateType DispatchTok where
    {- NB: this should not be exploited. It could also be unaffectful, if the new type is not of the form:
        forall a1 ... an . C (t a1 ... an)
    where C is the head of a constraint and t is function on types. -}
    updateType dispTok lpty =
        case suitableType of
            Nothing -> dispTok
            Just cty ->
                case dispTok of
                    DispatchVar nVar st -> DispatchVar (updateType nVar lpty) st
                    DispatchVal _ st -> DispatchVal cty st
        where
            suitableType =
                case lpty of
                    (Forall bs (OnlyConstraints [c] dst)) -> Just (ContsOnlyTy bs c dst)
                    _ -> Nothing

newDispatchVal :: LangTypeScheme a -> a -> Maybe (DispatchTok a)
newDispatchVal lpty st =
    case schemeToOnlyConstraint lpty of
        Nothing -> Nothing
        Just ocs -> Just $ DispatchVal ocs st

{- It creates a new dispatch value from a constraint.
NB: it does not add any binder to the type scheme. -}
newDispatchVal' :: LangSpecConstraint a -> a -> DispatchTok a
newDispatchVal' lspc st = DispatchVal (ContsOnlyTy [] lspc st) st

constraintFromVal :: DispatchTok a -> Maybe (LangSpecConstraint a)
constraintFromVal (DispatchVal (ContsOnlyTy _ c _) _) = Just c
constraintFromVal (DispatchVar _ _) = Nothing

{- The union between variables and values. -}
data NotedTok a =
      NVarT (NotedVar a)
    | NValT (NotedVal a)
    deriving Show

instance Functor NotedTok where
    fmap f (NVarT nVar) = NVarT $ fmap f nVar
    fmap f (NValT nVal) = NValT $ fmap f nVal

instance AtomRep (NotedTok a) where
    repOf (NVarT nVar) = repOf nVar
    repOf (NValT nVal) = repOf nVal

instance HasType NotedTok where
    typeOf (NVarT v) = typeOf v
    typeOf (NValT v) = typeOf v

instance ActualType NotedTok where
    infrdTypeOf (NVarT v) = infrdTypeOf v
    infrdTypeOf (NValT v) = infrdTypeOf v

instance UpdateType NotedTok where
    updateType (NVarT nVar) lhty = NVarT $ updateType nVar lhty
    updateType (NValT nVal) lhty = NValT $ updateType nVal lhty

data NotedMinimalExpr a =
      MatchDefault (LangTypeScheme a) a
    | MatchVar (NotedVar a) a
    deriving Show

instance Functor NotedMinimalExpr where
    fmap f (MatchDefault ty st) = MatchDefault <| fmap f ty <| f st
    fmap f (MatchVar nVar st) = MatchVar <| fmap f nVar <| f st

instance HasState NotedMinimalExpr where
    stateOf (MatchDefault _ st) = st
    stateOf (MatchVar _ st) = st

instance HasType NotedMinimalExpr where
    typeOf (MatchDefault lpty _) = lpty
    typeOf (MatchVar nVar _) = typeOf nVar

instance ActualType NotedMinimalExpr where
    infrdTypeOf (MatchDefault lpty _) = Just lpty
    infrdTypeOf (MatchVar nVar _) = infrdTypeOf nVar

instance SpecType NotedMinimalExpr where
    specTypeWith minExpr lam =
        case minExpr of
            MatchDefault lpty st -> MatchDefault (lpty `specTypeWith` lam) st
            MatchVar nVar st ->
                let newNVar = nVar `specTypeWith` lam in
                    MatchVar newNVar st

instance UpdateType NotedMinimalExpr where
    updateType (MatchDefault _ st) lpty = MatchDefault lpty st
    updateType (MatchVar nVar st) lpty = MatchVar (updateType nVar lpty) st

{- Matching expressions with the notion of type. -}
data NotedMatchExpr a =
      MatchMinimal (NotedMinimalExpr a)
    {- Just a shorthand for deep pattern matching which is not really deep, namely the pattern matching stops
    with only variables and default cases. -}
    | MatchValMins (NotedVal a) [NotedMinimalExpr a] (LangTypeScheme a) a
    {- Deep pattern matching. -}
    | MatchValMs (NotedVal a) [NotedMatchExpr a] (LangTypeScheme a) a
    deriving Show

instance Functor NotedMatchExpr where
    fmap f (MatchMinimal minE) = MatchMinimal $ fmap f minE
    fmap f (MatchValMins nVal mins lpty st) =
        MatchValMins <| fmap f nVal <| map (fmap f) mins <| fmap f lpty <| f st
    fmap f (MatchValMs nVal ms lpty st) =
        MatchValMs <| fmap f nVal <| map (fmap f) ms <| fmap f lpty <| f st

instance HasState NotedMatchExpr where
    stateOf (MatchMinimal minE) = stateOf minE
    stateOf (MatchValMins _ _ _ st) = st
    stateOf (MatchValMs _ _ _ st) = st

instance HasType NotedMatchExpr where
    typeOf (MatchMinimal minE) = typeOf minE
    typeOf (MatchValMins _ _ lpty _) = lpty
    typeOf (MatchValMs _ _ lpty _) = lpty

instance ActualType NotedMatchExpr where
    infrdTypeOf = Just . typeOf

instance SpecType NotedMatchExpr where
    specTypeWith mExpr lam =
        case mExpr of
            MatchMinimal minExpr ->
                MatchMinimal $ minExpr `specTypeWith` lam
            MatchValMs nVal nms lpty st ->
                specMultiple nVal nms lpty st MatchValMs
            MatchValMins nVal nMins lpty st ->
                specMultiple nVal nMins lpty st MatchValMins
        where
            specMultiple nVal tokens lpty st build =
                let nVal' = nVal `specTypeWith` lam in
                    build nVal' (map (`specTypeWith` lam) tokens) (lpty `specTypeWith` lam) st

instance UpdateType NotedMatchExpr where
    updateType (MatchMinimal minExpr) lpty =
        MatchMinimal $ updateType minExpr lpty
    updateType (MatchValMs nVal nms _ st) lpty =
        MatchValMs nVal nms lpty st
    updateType (MatchValMins nVal mins _ st) lpty =
        MatchValMins nVal mins lpty st

nVarsOfMin :: NotedMinimalExpr a -> [NotedVar a]
nVarsOfMin (MatchDefault _ _) = []
nVarsOfMin (MatchVar nVar _) = [nVar]

nVarsOf :: NotedMatchExpr a -> [NotedVar a]
nVarsOf (MatchMinimal nmin) = nVarsOfMin nmin
nVarsOf (MatchValMs _ nms _ _) = concatMap nVarsOf nms
nVarsOf (MatchValMins _ nmins _ _) = concatMap nVarsOfMin nmins

{- Expressions with the notion of type. -}
data NotedExpr a =
      ExprVar (NotedVar a) a
    | ExprVal (NotedVal a) a
    --TODO: make a new type for dispatch variable
    | ExprDispatchVar (NotedVar a) [DispatchTok a] a
    | ExprPM (NotedPM a) a
    | ExprBound (NotedBound a) (NotedExpr a) a
    | ExprApp (NotedApp a) a
    | ExprLam (NotedLam a) a
    deriving Show

instance Functor NotedExpr where
    fmap f (ExprVar nVar st) = ExprVar <| fmap f nVar <| f st
    fmap f (ExprVal nVal st) = ExprVal <| fmap f nVal <| f st
    fmap f (ExprDispatchVar nVar nToks st) = ExprDispatchVar <| fmap f nVar <| map (fmap f) nToks <| f st
    fmap f (ExprPM pm st) = ExprPM <| fmap f pm <| f st
    fmap f (ExprApp app st) = ExprApp <| fmap f app <| f st
    fmap f (ExprBound be e st) = ExprBound <| fmap f be <| fmap f e <| f st
    fmap f (ExprLam nLam st) = ExprLam <| fmap f nLam <| f st

instance HasState NotedExpr where
    stateOf (ExprVar _ st) = st
    stateOf (ExprVal _ st) = st
    stateOf (ExprDispatchVar _ _ st) = st
    stateOf (ExprPM _ st) = st
    stateOf (ExprApp _ st) = st
    stateOf (ExprBound _ _ st) = st
    stateOf (ExprLam _ st) = st

instance HasType NotedExpr where
    typeOf (ExprVar nVar _) = typeOf nVar
    typeOf (ExprVal nVal _) = typeOf nVal
    typeOf (ExprDispatchVar nVar _ _) = typeOf nVar
    typeOf (ExprPM pm _) = typeOf pm
    typeOf (ExprApp app _) = typeOf app
    typeOf (ExprBound _ e _) = typeOf e
    typeOf (ExprLam lam _) = typeOf lam

instance ActualType NotedExpr where
    infrdTypeOf = Just . typeOf

instance SpecType NotedExpr where
    specTypeWith (ExprVar nVar st) lam = ExprVar (nVar `specTypeWith` lam) st
    specTypeWith (ExprVal nVal st) lam = ExprVal (nVal `specTypeWith` lam) st
    specTypeWith (ExprDispatchVar nVar dispToks st) lam =
        ExprDispatchVar (nVar `specTypeWith` lam) (map (`specTypeWith` lam) dispToks) st
    specTypeWith (ExprPM pm st) lam = ExprPM (pm `specTypeWith` lam) st
    specTypeWith (ExprApp app st) lam = ExprApp (app `specTypeWith` lam) st
    specTypeWith (ExprBound bound e st) lam = ExprBound (bound `specTypeWith` lam) (e `specTypeWith` lam) st
    specTypeWith (ExprLam nLam st) lam = ExprLam (nLam `specTypeWith` lam) st

instance UpdateType NotedExpr where
    updateType (ExprVar nVar st) lpty = ExprVar (updateType nVar lpty) st
    updateType (ExprVal nVal st) lpty = ExprVal (updateType nVal lpty) st
    {- Dispatch tokens remains unaltered. -}
    updateType (ExprDispatchVar nVar dispToks st) lpty =
        ExprDispatchVar (updateType nVar lpty) dispToks st
    updateType (ExprPM pm st) lpty = ExprPM (updateType pm lpty) st
    updateType (ExprApp app st) lpty = ExprApp (updateType app lpty) st
    updateType (ExprBound bound e st) lpty = ExprBound bound (updateType e lpty) st
    updateType (ExprLam lam st) lpty = ExprLam (updateType lam lpty) st

data NotedApp a = NotedApp (NotedExpr a) (NotedExpr a) (LangTypeScheme a) a deriving Show

instance Functor NotedApp where
    fmap f (NotedApp e e1 lpty st) = NotedApp <| fmap f e <| fmap f e1 <| fmap f lpty <| f st

instance HasType NotedApp where
    typeOf (NotedApp _ _ lpty _) = lpty

instance ActualType NotedApp where
    infrdTypeOf = Just . typeOf

instance SpecType NotedApp where
    specTypeWith (NotedApp e e' lpty st) lam =
        NotedApp (e `specTypeWith` lam) (e' `specTypeWith` lam) (lpty `specTypeWith` lam) st

instance UpdateType NotedApp where
    updateType (NotedApp e e' _ st) lpty = NotedApp e e' lpty st

data NotedCase a = NotedCase (NotedMatchExpr a) (NotedExpr a) a deriving Show

instance HasState NotedCase where
    stateOf (NotedCase _ _ st) = st

instance Functor NotedCase where
    fmap f (NotedCase m e st) = NotedCase (fmap f m) (fmap f e) (f st)

typeOfMatching :: NotedCase a -> Maybe (LangTypeScheme a)
typeOfMatching (NotedCase nme _ _) = infrdTypeOf nme

instance HasType NotedCase where
    typeOf (NotedCase _ e _) = typeOf e

instance ActualType NotedCase where
    infrdTypeOf = Just . typeOf

instance SpecType NotedCase where
    specTypeWith (NotedCase me e st) lam =
        NotedCase (me `specTypeWith` lam) (e `specTypeWith` lam) st

{- NB: the type of pattern matching should be the one of cases' right-hand expression. -}
data NotedPM a = NotedPM (NotedExpr a) [NotedCase a] (LangTypeScheme a) a deriving Show

instance HasState NotedPM where
    stateOf (NotedPM _ _ _ st) = st

instance Functor NotedPM where
    fmap f (NotedPM e cs lpty st) = NotedPM <| fmap f e <| map (fmap f) cs <| fmap f lpty <| f st

instance HasType NotedPM where
    typeOf (NotedPM _ _ lpty _) = lpty

instance ActualType NotedPM where
    infrdTypeOf = Just . typeOf

instance SpecType NotedPM where
    specTypeWith (NotedPM e cs lpty st) lam =
        NotedPM (e `specTypeWith` lam) (map (`specTypeWith` lam) cs) (lpty `specTypeWith` lam) st

instance UpdateType NotedPM where
    updateType (NotedPM e cs _ st) lpty = NotedPM e cs lpty st

{- The pattern matching construct has the following form (pseudo-code):
    match e with
        | m1 -> e1
        ...
        | mn -> en
This function tries to find the first `mk` which is of the form `v` where `v` is simply a variable. -}
getScrutinee :: NotedPM a -> Maybe (NotedVar a)
getScrutinee (NotedPM _ cs _ _) = findScrutinee cs
    where
        findScrutinee [] = Nothing
        findScrutinee ((NotedCase (MatchMinimal (MatchVar nVar _)) _ _) : _) = Just nVar
        findScrutinee (_ : t) = findScrutinee t

data NotedBound a = NotedBound (NotedVar a) [NotedVar a] (NotedExpr a) a deriving Show

instance Functor NotedBound where
    fmap f (NotedBound nVar nms nExpr st) =
        NotedBound <| fmap f nVar <| map (fmap f) nms <| fmap f nExpr <| f st

{-
instance HasType NotedBound where
    typeOf (NotedBound _ _ nExpr _) = typeOf nExpr
        -}

instance ActualType NotedBound where
    infrdTypeOf (NotedBound _ _ nExpr _) = infrdTypeOf nExpr

instance SpecType NotedBound where
    specTypeWith (NotedBound nVar nVars ne st) lam =
        NotedBound (nVar `specTypeWith` lam) (map (`specTypeWith` lam) nVars) (ne `specTypeWith` lam) st

data NotedLam a = NotedLam (NotedVar a) (NotedExpr a) (LangTypeScheme a) a deriving Show

instance Functor NotedLam where
    fmap f (NotedLam nVar nExpr lpty st) =
        NotedLam <| fmap f nVar <| fmap f nExpr <| fmap f lpty <| f st

instance HasState NotedLam where
    stateOf (NotedLam _ _ _ st) = st

instance HasType NotedLam where
    typeOf (NotedLam _ _ lpty _) = lpty

instance ActualType NotedLam where
    infrdTypeOf = Just . typeOf

instance SpecType NotedLam where
    specTypeWith (NotedLam nVar ne lpty st) lam =
        NotedLam (nVar `specTypeWith` lam) (ne `specTypeWith` lam) (lpty `specTypeWith` lam) st

instance UpdateType NotedLam where
    updateType (NotedLam nVar ne _ st) lpty = NotedLam nVar ne lpty st

{- It creates a new kind variable. -}
newVarLKTy :: String -> LangKind
newVarLKTy = LKVar

newLNTy :: TyConRep -> LangKind -> [(TyVarRep, Role, a)] -> a -> Maybe (LangNewType a)
newLNTy rep LKConst [] st =
    Just $ LNTy
        { base = tokenRepToStr rep
        , kind' = LKConst
        , params = []
        , infinite = False
        , lntyState = st
        }
newLNTy rep k @ (SubLK lks) ps st =
    {- The number of arguments must match exactly the number of sub-kinds - 1 because the arguments represent data
    for type variables and the sub-kinds are associated one by one to the arguments, but the last one has to "survive"
    because it is return kind. -}
    if length lks - 1 /= length ps
    then Nothing
    else
        let kps = zip ps lks in
        let tyVars = map (\((varRep, role, varSt), lk) -> newLVTy varRep lk role varSt) kps in
            Just $ LNTy
                { base = tokenRepToStr rep
                , kind' = k
                , params = tyVars
                , infinite = False
                , lntyState = st
                }
{- In any other case (e.g. kind is a variable, no arguments and multiple sub-kinds, etc.), it's not possible to build
the type. -}
newLNTy _ _ _ _ = Nothing

newLVTy :: TyVarRep -> LangKind -> Role -> a -> LangVarType a
newLVTy rep lk role st =
    LVTy
        { var = tokenRepToStr rep
        , kind = lk
        , lvtyRole = Role_ role
        , lvtyState = st
        }

{- It builds up a new LangSpecType from a LangNewType and a list of "promoters". If the
"promoters" are more than the args of the LangNewType, it fails, by returning Nothing. The
"promoters" promote from the first arguments of the LangNewType (so it is possible to build types
which have kind different from *). -}
newLSpTy :: LangNewType a -> [LangHigherType a] -> a -> Maybe (LangSpecType a)
newLSpTy lnty args st =
    if length (params lnty) < length args
    then Nothing
    else Just $ LSpTy
        { specBase = lnty
        , actualArgs = args
        , lstyState = st
        }

kindAt :: Int -> LangKind -> Maybe LangKind
kindAt pos (SubLK ks) =
    {- The following check is performed because the last kind of a composite kind must be LKConst and should not
    be indexed. If this behaviour is undesirable, use kindAt'. -}
    if pos > length ks - 2
    then Nothing
    else elemAt pos ks
kindAt _ _ = Nothing

{- Similar to newLSpTy, but it can only build up types with kind *. Furthermore, it takes a `prom` param
which tells which type variables of the LangNewType to promote. It can fail, e.g. the `prom` indexes something
over the range of arguments of the LangNewType. -}
newLSpTy' :: LangNewType a -> [(Int, LangHigherType a)] -> a -> Maybe (LangSpecType a)
newLSpTy' lnty prom st =
    let ts = map newLHTyFromLVTy $ params lnty in
        newLSpTyFromLSpTy LSpTy
            { specBase = lnty
            , actualArgs = ts
            , lstyState = st
            } prom st

{- Given a LangVarType `lvty`, it builds a LangVarCompType, by "specializing" `lvty` with a list of
LangHigherType `lts` and also checking the kinds of `lts` are consistent with the kind of `lvty`. -}
newLVcTy :: LangVarType a -> [LangHigherType a] -> a -> Maybe (LangVarCompType a)
newLVcTy lvty [] st =
    Just $ LVcTy
        { specVar = lvty
        , actualVArgs = []
        , lvctyState = st
        }
newLVcTy lvty lts @ (lhty : t) st =
    case kindOf lvty of
        SubLK [] -> Nothing
        SubLK [LKConst] -> Nothing
        SubLK lks @ (lk : lkt) ->
            if length lks - 1 < length lts   --The - 1 is for the final LKConst (it has not to be taken into consideration)
            then Nothing
            {- Associating a type with a kind (with zip) in order to check this type of pair in foldl'. -}
            else let args = foldl' rightKind (rightKind <| Just [] $ (lhty, lk)) $ zip t lkt in
                if isNothing args
                then Nothing
                else Just $ LVcTy
                    { specVar = lvty
                    , actualVArgs = reverse $ (\(Just l) -> l) args     --safe unwrap
                    , lvctyState = st
                    }
        _ -> Nothing
        where
            rightKind Nothing _ = Nothing
            rightKind (Just lts') (lhty', lk) =
                {- As said above, the kind of each LangHigherType value must be consistent. The notion of
                `ActualKind` is used instead of the one of `HasKind`, because the latter just calculates
                the kind of a "primitive" token (not a composite one). -}
                if infrdKindOf lhty' == Just lk
                then Just $ lhty' : lts'
                else Nothing

newLNCont :: PropConRep -> [LangVarType a] -> a -> LangNewConstraint a
newLNCont rep vars st =
    LNCont
        { prop = tokenRepToStr rep
        , contVars = vars
        , lcontState = st
        }

{- newLTy(')* functions just constructs a LangType from the constructors of different types. -}
newLTy :: LangNewType a -> [LangHigherType a] -> a -> Maybe (LangType a)
newLTy lnty lts st =
    case newLSpTy lnty lts st of
        Nothing -> Nothing
        Just lspty -> Just $ LATyComp lspty

newLTy' :: LangNewType a -> [(Int, LangHigherType a)] -> a -> Maybe (LangType a)
newLTy' lnty prom st =
    case newLSpTy' lnty prom st of
        Nothing -> Nothing
        Just lspty -> Just $ LATyComp lspty

newLTy'' :: LangVarType a -> [LangHigherType a] -> a -> Maybe (LangType a)
newLTy'' lvty lts st =
    case newLVcTy lvty lts st of
        Nothing -> Nothing
        Just lvcty -> Just $ LATyParam lvcty

{- newLowLHTy(')* functions are just wrappers for newLTy(')* functions, but they builds a LangHigherType value. -}
newLowLHTy :: LangNewType a -> [LangHigherType a] -> a -> Maybe (LangHigherType a)
newLowLHTy lnty lts st = maybe Nothing toLHTy $ newLTy lnty lts st

newLowLHTy' :: LangNewType a -> [(Int, LangHigherType a)] -> a -> Maybe (LangHigherType a)
newLowLHTy' lnty prom st = maybe Nothing toLHTy $ newLTy' lnty prom st

newLowLHTy'' :: LangVarType a -> [LangHigherType a] -> a -> Maybe (LangHigherType a)
newLowLHTy'' lvty lts st = maybe Nothing toLHTy $ newLTy'' lvty lts st

newLHTyFromLVTy :: LangVarType a -> LangHigherType a
newLHTyFromLVTy lvty =
    LTy . LATyParam $ LVcTy
        { specVar = lvty
        , actualVArgs = []
        , lvctyState = stateOf lvty
        }

newLHTyFromLNTy :: LangNewType a -> Maybe (LangHigherType a)
newLHTyFromLNTy lnty =
    let tyVars = argsOf lnty in
        Just . unsafeToLHTy . LATyComp $ LSpTy
            { specBase = lnty
            , actualArgs = map newLHTyFromLVTy tyVars
            , lstyState = stateOf lnty
            }

newLQTyFromLVTy :: LangVarType a -> LangQualType a
newLQTyFromLVTy = Qual [] . newLHTyFromLVTy

{- It builds a new LangHigherType value from a LangNewType value.
NB: Even though it returns a Maybe value, it should never fail or if it fails, the reason should be the
LangNewType value has been built in an inconsistent way. -}
newLQTyFromLNTy :: LangNewType a -> Maybe (LangQualType a)
newLQTyFromLNTy lnty = Qual [] <$> newLHTyFromLNTy lnty

newQualType :: [LangSpecConstraint a] -> LangHigherType a -> LangQualType a
newQualType = Qual

{- A function to visit type variables in a LangNewType token, this can be useful used with newLHTyFromLNTy
which does not care of bound type variables. The `x` is just ancillary data that can be passed to the function.
The callback is called on each argument of LNTy token (with the updated ancillary data from the previous
callback). -}
visitVarsInLNTy
    :: LangNewType a
    {- Ancillary data -}
    -> x
    {- Visitor callback -}
    -> (LangVarType a -> x -> (LangVarType a, x))
    -> (LangNewType a, x)
visitVarsInLNTy lnty x trans =
    let args = argsOf lnty in
    let (newRevArgs, x') = foldl' mkArg ([], x) args in
    --The reverse is necessary because elements has been inserted in the head.
    let newArgs = reverse newRevArgs in
        ( LNTy
            { base = base lnty
            , kind' = kind' lnty
            , params = newArgs
            , infinite = infinite lnty
            , lntyState = lntyState lnty
            }
        , x'
        )
    where
        mkArg (args, y) v =
            let (v', y') = trans v y in
                (v' : args, y')

{- Same of visitVarsInLNTy, but with LangNewConstraint tokens. Moreover, it acts directly on LangVarType tokens,
differently from what visitVarsInLNTy does. -}
visitVarsInLNCont
    :: LangNewConstraint a
    -> x
    -> (LangVarType a -> x -> (LangVarType a, x))
    -> (LangNewConstraint a, x)
visitVarsInLNCont lnc x trans =
    let vars = argsOf lnc in
    let (newRevVars, x') = foldl' mkArg ([], x) vars in
    --The reverse is necessary because elements has been inserted in the head.
    let newArgs = reverse newRevVars in
        ( LNCont
            { prop = prop lnc
            , contVars = newArgs
            , lcontState = lcontState lnc
            }
        , x'
        )
    where
        mkArg (args, y) lvty =
            let (lvty', y') = trans lvty y in
                (lvty' : args, y')

visitErr :: StateT x Maybe res
visitErr = lift Nothing

visitMaybe :: Maybe (LangVarType a, x) -> StateT x Maybe (LangVarType a)
visitMaybe res =
    case res of
        Nothing -> visitErr
        Just (lvty, s) -> do
            put s
            return lvty

visitLVTy :: (LangVarType a -> x -> Maybe (LangVarType a, x)) -> LangVarType a -> StateT x Maybe (LangVarType a)
visitLVTy f lvty = do
    s <- get
    visitMaybe $ f lvty s

visit :: (LangVarType a -> x -> Maybe (LangVarType a, x)) -> LangHigherType a -> StateT x Maybe (LangHigherType a)
visit f (HApp lhts st) = do
    lhts' <- mapM (visit f) lhts
    return $ HApp lhts' st
visit f (LTy (LATyParam lvcty)) = do
    lvty <- visitLVTy f $ specVar lvcty
    lhts <- mapM (visit f) $ actualVArgs lvcty
    return . LTy . LATyParam $ LVcTy
        { specVar = lvty
        , actualVArgs = lhts
        , lvctyState = lvctyState lvcty
        }
visit f (LTy (LATyComp lspty)) = do
    lhts <- mapM (visit f) $ actualArgs lspty
    return . LTy . LATyComp $ LSpTy
        { specBase = specBase lspty
        , actualArgs = lhts
        , lstyState = lstyState lspty
        }

visitVarsInLHTy
    :: LangHigherType a
    -> x
    -> (LangVarType a -> x -> Maybe (LangVarType a, x))
    -> Maybe (LangHigherType a, x)
visitVarsInLHTy lhty x f = runStateT (visit f lhty) x

visitVarsInLSpCont
    :: LangSpecConstraint a
    -> x
    -> (LangVarType a -> x -> Maybe (LangVarType a, x))
    -> Maybe (LangSpecConstraint a, x)
visitVarsInLSpCont lspc x f =
    let lhts = lscontArgs lspc in
        case runStateT (mapM (visit f) lhts) x of
            Nothing -> Nothing
            Just (lhts', x') ->
                Just
                    ( LSCont
                        { cont = cont lspc
                        , lscontArgs = lhts'
                        , lscontState = lscontState lspc
                        }
                    , x'
                    )

visitVarsInLSpConts
    :: [LangSpecConstraint a]
    -> x
    -> (LangVarType a -> x -> Maybe (LangVarType a, x))
    -> Maybe ([LangSpecConstraint a], x)
visitVarsInLSpConts cs x f =
    foldl' tryVisitCont (Just ([], x)) cs
    where
        tryVisitCont Nothing _ = Nothing
        tryVisitCont (Just (conts, y)) c =
            case visitVarsInLSpCont c y f of
                Nothing -> Nothing
                Just (c', y') -> Just (conts ++ [c'], y')

{- It builds a new LangHigherType value which represents the function type. Arguments cannot be more than 2. -}
newFunLHTy :: [LangHigherType a] -> a -> Maybe (LangHigherType a)
newFunLHTy args st =
    if length args > 2
    then Nothing
    else Just $ HApp args st

newConcatLHTy :: NonEmpty (LangHigherType a) -> LangHigherType a
newConcatLHTy (lhty :| []) = lhty
newConcatLHTy (lhty :| (lhty' : t)) = HApp [lhty, newConcatLHTy (lhty' :| t)] $ stateOf lhty

{- Given a LSpTy, it specializes it furthermore, but it can fail (if the `prom` param tries to specialize
something which is not a variable or it tries to access arguments which do not exist, namely overflow).
NB: this function makes a promotion, but the old type variables are lost forever. -}
newLSpTyFromLSpTy :: LangSpecType a -> [(Int, LangHigherType a)] -> a -> Maybe (LangSpecType a)
newLSpTyFromLSpTy lspty prom st =
    case updateLspty of
        Nothing -> Nothing
        {- Updating the state before returning -}
        Just lspty' ->
            Just $ lspty'
                { lstyState = st
                }
    where
        updateLspty = forAll prom tryReplace `startingFrom` Just lspty

        tryReplace Nothing _ = Nothing
        tryReplace (Just lspty') (pos, lhty) =
            case replaceAndGetAt pos lhty $ actualArgs lspty' of
                {- Only type variables can be replaced (namely `LATyParam` constructor). -}
                (Just replLhty @ (LTy (LATyParam _)), args) ->
                    {- To really perform the replacement, it is necessary that kinds of types are the same. -}
                    if infrdKindOf replLhty == infrdKindOf lhty
                    then
                        Just $ lspty'
                            { actualArgs = args
                            }
                    else Nothing
                {- All other cases are when the replaced type wasn't a type variable or when the replacement hasn't been
                performed because of an out-of-bound index. -}
                (_, _) -> Nothing

data ContBuildError a =
      ArgsNoErr (LangNewConstraint a) [LangHigherType a]
    | TypeBuildErr (LangHigherType a)
    | KindErr (LangVarType a, LangKind) (LangHigherType a, LangKind)
    deriving Show

newLSpCont
    :: LangNewConstraint a
    -> [LangHigherType a]
    -> a
    -> Either (ContBuildError a) (LangSpecConstraint a, Substitution a)
newLSpCont lnc lhts st =
    let tyVars = argsOf lnc in
    let ks = map (\v -> (v, kindOf v)) tyVars in
        case argsFit ks lhts of
            None ->
                Right
                    ( LSCont
                        { cont = lnc
                        , lscontArgs = lhts
                        , lscontState = st
                        }
                    , zip tyVars lhts
                    )
            This _ -> Left $ ArgsNoErr lnc lhts
            That err -> Left err
    where
        argsFit :: [(LangVarType a, LangKind)] -> [LangHigherType a] -> KnowledgeOneOf () (ContBuildError a)
        argsFit [] [] = None
        argsFit [] _ = This ()
        argsFit _ [] = This ()
        argsFit ((v, lk) : kt) (ty : t) =
            case infrdKindOf ty of
                Nothing -> That $ TypeBuildErr ty
                Just lk' ->
                    if lk /= lk'
                    then That $ KindErr (v, lk) (ty, lk')
                    else argsFit kt t

{- NB: this is unsafe, because it does not track eventual bound variables in the LangNewConstraint token. -}
newNaiveLSpCont :: LangNewConstraint a -> a -> LangSpecConstraint a
newNaiveLSpCont lnc st =
    LSCont
        { cont = lnc
        , lscontArgs = map newLHTyFromLVTy $ argsOf lnc
        , lscontState = st
        }

newNotedVar :: SymbolRep -> LangTypeScheme a -> a -> NotedVar a
newNotedVar rep ty st =
    NotedVar
        { nVarName = tokenRepToStr rep
        , nVarType = ty
        , nVarState = st
        }

newDispatchNotedVar :: String -> OnlyConstraintScheme a -> a -> NotedVar a
newDispatchNotedVar v scheme st =
    let lpty = onlyConstraintToScheme scheme in
        NotedVar
            { nVarName = v
            , nVarType = lpty
            , nVarState = st
            }

newNotedVal :: DataConRep -> LangTypeScheme a -> a -> NotedVal a
newNotedVal conRep lpty st = NotedVal (tokenRepToStr conRep) lpty st

newNotedIntLit :: Int -> LangTypeScheme a -> a -> NotedVal a
newNotedIntLit i = NotedLit (LitInt i)

newNotedDoubleLit :: Double -> LangTypeScheme a -> a -> NotedVal a
newNotedDoubleLit f = NotedLit (LitDouble f)

newNotedCharLit :: Char -> LangTypeScheme a -> a -> NotedVal a
newNotedCharLit c = NotedLit (LitChar c)

newNotedStringLit :: String -> LangTypeScheme a -> a -> NotedVal a
newNotedStringLit s = NotedLit (LitString s)

newDefNotedMinExpr :: LangTypeScheme a -> a -> NotedMinimalExpr a
newDefNotedMinExpr = MatchDefault

newVarNotedMinExpr :: NotedVar a -> a -> NotedMinimalExpr a
newVarNotedMinExpr = MatchVar

newMinNotedMExpr :: NotedMinimalExpr a -> NotedMatchExpr a
newMinNotedMExpr = MatchMinimal

{- Just three alias for NotedMatchExpr constructors. -}
newVarNotedMExpr :: NotedVar a -> a -> NotedMatchExpr a
newVarNotedMExpr nVar st = MatchMinimal $ MatchVar nVar st

newValNotedMExpr :: NotedVal a -> a -> NotedMatchExpr a
newValNotedMExpr nVal = MatchValMins nVal [] (typeOf nVal)

newMinCompNotedMExpr :: NotedVal a -> [NotedMinimalExpr a] -> LangTypeScheme a -> a -> NotedMatchExpr a
newMinCompNotedMExpr = MatchValMins

newCompNotedMExpr :: NotedVal a -> [NotedMatchExpr a] -> LangTypeScheme a -> a -> NotedMatchExpr a
newCompNotedMExpr = MatchValMs

newDefNotedMExpr :: LangTypeScheme a -> a -> NotedMatchExpr a
newDefNotedMExpr lpty st = MatchMinimal $ MatchDefault lpty st

newVarNotedExpr :: NotedVar a -> a -> NotedExpr a
newVarNotedExpr = ExprVar

newValNotedExpr :: NotedVal a -> a -> NotedExpr a
newValNotedExpr = ExprVal

newVarsDispatchNotedExpr :: NotedVar a -> [NotedVar a] -> a -> NotedExpr a
newVarsDispatchNotedExpr nVar args st =
    let args' = map (\arg -> DispatchVar arg $ stateOf arg) args in
        ExprDispatchVar nVar args' st

newBoundNotedExpr :: NotedBound a -> NotedExpr a -> a -> NotedExpr a
newBoundNotedExpr = ExprBound

{-
newAppNotedExpr :: NotedExpr a -> [NotedExpr a] -> a -> NotedExpr a
newAppNotedExpr e [] _ = e
newAppNotedExpr e (app : t) st =
    newAppNotedExpr (ExprApp (NotedApp e app $ stateOf e) st) t st
        -}

newAppNotedExpr' :: NotedExpr a -> NotedExpr a -> LangTypeScheme a -> a -> a -> NotedExpr a
newAppNotedExpr' ne ne1 lpty appSt = ExprApp (NotedApp ne ne1 lpty appSt)

newAppNotedExpr'' :: NotedApp a -> a -> NotedExpr a
newAppNotedExpr'' = ExprApp

newPMNotedExpr :: NotedPM a -> a -> NotedExpr a
newPMNotedExpr = ExprPM

newNotedCase :: NotedMatchExpr a -> NotedExpr a -> a -> NotedCase a
newNotedCase = NotedCase

newNotedPM :: NotedExpr a -> [NotedCase a] -> LangTypeScheme a -> a -> NotedPM a
newNotedPM = NotedPM

newNotedLam :: NotedLam a -> a -> NotedExpr a
newNotedLam = ExprLam

{-
newNotedLams :: NonEmpty (NotedVar a, a) -> NotedExpr a -> NotedExpr a
newNotedLams ((nVar, st) :| []) e = ExprLam (NotedLam nVar e st) st
newNotedLams ((nVar, st) :| h : t) e =
    let nLamExpr = newNotedLams (h :| t) e in
    let nLam = NotedLam nVar nLamExpr st in
        ExprLam nLam st

newNotedLams' :: (NotedVar a, a) -> [(NotedVar a, a)] -> NotedExpr a -> NotedExpr a
newNotedLams' h l = newNotedLams (h :| l)
-}

kindOfArg :: TyVarRep -> LangNewType a -> Maybe LangKind
kindOfArg arg lnty =
    case firstThat (\lvty -> repOf lvty == arg) $ argsOf lnty of
        Nothing -> Nothing
        Just lvty -> Just $ kindOf lvty

isTyVar :: LangHigherType a -> Bool
isTyVar (LTy (LATyParam _)) = True
isTyVar _ = False

isTyVar' :: LangQualType a -> Bool
isTyVar' (Qual _ lhty) = isTyVar lhty
isTyVar' (OnlyConstraints _ _) = False

isSingletonTyVar :: LangHigherType a -> Bool
isSingletonTyVar lhty = isTyVar lhty && null (argsOf lhty)

isSingletonTyVar' :: LangQualType a -> Bool
isSingletonTyVar' (Qual _ lhty) = isSingletonTyVar lhty
isSingletonTyVar' (OnlyConstraints _ _) = False

isConcrete :: LangHigherType a -> Bool
isConcrete = not . isTyVar

isConcrete' :: LangQualType a -> Bool
isConcrete' (Qual _ lhty) = isConcrete lhty
isConcrete' (OnlyConstraints _ _) = False

varOf :: LangHigherType a -> Maybe (LangVarType a)
varOf (LTy (LATyParam lvcty)) = Just $ specVar lvcty
varOf _ = Nothing

varOf' :: LangQualType a -> Maybe (LangVarType a)
varOf' (Qual _ lhty) = varOf lhty
varOf' (OnlyConstraints _ _) = Nothing

singletonVarOf :: LangHigherType a -> Maybe (LangVarType a)
singletonVarOf lhty =
    if null $ argsOf lhty
    then varOf lhty
    else Nothing

singletonVarOf' :: LangQualType a -> Maybe (LangVarType a)
singletonVarOf' (Qual _ lhty) = singletonVarOf lhty
singletonVarOf' (OnlyConstraints _ _) = Nothing

baseOfFunTy :: LangHigherType a -> Bool
baseOfFunTy (HApp _ _) = True
baseOfFunTy _ = False

{- Low-level head equality test between a String value and a LangHigherType value. Usually, a client should
prefer sameBaseOf. -}
rawSameBaseOf :: TypeRep -> LangHigherType a -> Bool
rawSameBaseOf tyRep (HApp _ _) = tyRep == BI.nameFunctionApp
rawSameBaseOf tyRep (LTy (LATyComp lspty)) = repOf (headOf lspty) == tyRep
rawSameBaseOf tyRep (LTy (LATyParam lvcty)) = repOf (headOf lvcty) == tyRep

{- Head equality test between LangHigherType values. -}
sameBaseOf :: LangHigherType a -> LangHigherType a -> Bool
sameBaseOf (HApp _ _) (HApp _ _) = True
sameBaseOf (LTy (LATyParam lvcty)) (LTy (LATyParam lvcty')) = headOf lvcty == headOf lvcty'
sameBaseOf (LTy (LATyComp lspty)) (LTy (LATyComp lspty')) = headOf lspty == headOf lspty'
sameBaseOf (HApp _ _) lhty' @ (LTy (LATyComp _)) = baseOfFunTy $ unsafeToLHTy' lhty'
sameBaseOf lhty @ (LTy (LATyComp _)) (HApp _ _) = baseOfFunTy $ unsafeToLHTy' lhty
sameBaseOf _ _ = False

notedVarOf :: NotedTok a -> Maybe (NotedVar a)
notedVarOf (NVarT nVar) = Just nVar
notedVarOf (NValT _) = Nothing

notedValOf :: NotedTok a -> Maybe (NotedVal a)
notedValOf (NValT nVal) = Just nVal
notedValOf (NVarT _) = Nothing

anyTyVars :: LangHigherType a -> Bool
anyTyVars (HApp lhts _) = any anyTyVars lhts
anyTyVars (LTy (LATyParam _)) = True
anyTyVars (LTy (LATyComp lspty)) = any anyTyVars $ argsOf lspty

lengthVisitExprsInExpr :: NotedExpr a -> (NotedExpr a -> NotedExpr a) -> NotedExpr a
lengthVisitExprsInExpr ne f =
    let builtF _ e = (f e, ()) in
        fst $ lengthVisitExprsInExpr' () ne builtF

widthVisitExprsInExpr :: NotedExpr a -> (NotedExpr a -> NotedExpr a) -> NotedExpr a
widthVisitExprsInExpr ne f =
    let builtF _ e = (f e, ()) in
        fst $ widthVisitExprsInExpr' () ne builtF

recVisitExprs
    :: x
    -> NotedExpr a
    -> (x -> NotedExpr a -> (NotedExpr a, x))
    -> (  x
       -> NotedExpr a
       -> (x -> NotedExpr a -> (NotedExpr a, x))
       -> (NotedExpr a, x)
       )
    -> (NotedExpr a, x)
recVisitExprs x ne @ (ExprVar _ _) _ _ = (ne, x)
recVisitExprs x ne @ (ExprVal _ _) _ _ = (ne, x)
recVisitExprs x ne @ ExprDispatchVar {} _ _ = (ne, x)
recVisitExprs x (ExprPM (NotedPM topNe ncs lpty pmSt) st) f visitExprs =
    let (topNe', x1) = visitExprs x topNe f in
    let (ncs', x2) = foldl' visitExprsInCase ([], x1) ncs in
        (ExprPM (NotedPM topNe' ncs' lpty pmSt) st, x2)
    where
        visitExprsInCase (accum, y) (NotedCase ncNme ncNe ncSt) =
            let (ncNe', y1) = visitExprs y ncNe f in
                (accum ++ [NotedCase ncNme ncNe' ncSt], y1)
recVisitExprs x (ExprBound (NotedBound topNVar nVars bne bst) finalNe st) f visitExprs =
    let (bne', x1) = visitExprs x bne f in
    let (finalNe', x2) = visitExprs x1 finalNe f in
        (ExprBound (NotedBound topNVar nVars bne' bst) finalNe' st, x2)
recVisitExprs x (ExprApp (NotedApp ne1 ne2 lpty appSt) st) f visitExprs =
    let (ne1', x1) = visitExprs x ne1 f in
    let (ne2', x2) = visitExprs x1 ne2 f in
        (ExprApp (NotedApp ne1' ne2' lpty appSt) st, x2)
recVisitExprs x (ExprLam (NotedLam nVar lamNe lpty lamSt) st) f visitExprs =
    let (lamNe', x1) = visitExprs x lamNe f in
        (ExprLam (NotedLam nVar lamNe' lpty lamSt) st, x1)

{- NB: it visits the deepest expressions first. In the case of pattern matching, it visits firstly the top expression,
then the cases. -}
lengthVisitExprsInExpr' :: x -> NotedExpr a -> (x -> NotedExpr a -> (NotedExpr a, x)) -> (NotedExpr a, x)
lengthVisitExprsInExpr' x ne f =
    let (ne', x1) = recVisitExprs x ne f lengthVisitExprsInExpr' in
        f x1 ne'

{- NB: this is the same of `visitExprsInExpr'`, but the visit is not in length (deepest expression first), but in
width (closest expression first). The policy on pattern matching construct is the same. -}
widthVisitExprsInExpr' :: x -> NotedExpr a -> (x -> NotedExpr a -> (NotedExpr a, x)) -> (NotedExpr a, x)
widthVisitExprsInExpr' x ne f =
    let (ne', x1) = f x ne in
        recVisitExprs x1 ne' f widthVisitExprsInExpr'

-- Dumping expressions

{-
      MatchMinimal (NotedMinimalExpr a)
    {- Just a shorthand for deep pattern matching which is not really deep, namely the pattern matching stops
    with only variables and default cases. -}
    | MatchValMins (NotedVal a) [NotedMinimalExpr a] (LangTypeScheme a) a
    {- Deep pattern matching. -}
    | MatchValMs (NotedVal a) [NotedMatchExpr a] (LangTypeScheme a) a
data NotedMinimalExpr a =
      MatchDefault (LangTypeScheme a) a
    | MatchVar (NotedVar a) a
    deriving Show
-}

type Padding = Int

showPadding :: Padding -> String
showPadding 0 = ""
showPadding p = ' ' : showPadding (p - 1)

showMinExpr :: NotedMinimalExpr a -> Padding -> String
showMinExpr (MatchDefault _ _) p = showPadding p ++ "_"
showMinExpr (MatchVar nVar _) p = showPadding p ++ tokenRepToStr (repOf nVar)

showMatchExpr :: NotedMatchExpr a -> Padding -> String
showMatchExpr (MatchMinimal m) p = showMinExpr m p
showMatchExpr (MatchValMins nVal nMins _ _) p =
    showPadding p ++ tokenRepToStr (repOf nVal) ++ concatMap (\m -> " " ++ showMinExpr m 0) nMins
showMatchExpr (MatchValMs nVal nms _ _) p =
    showPadding p ++ tokenRepToStr (repOf nVal) ++ concatMap (\nme -> " (" ++ showMatchExpr nme 0 ++ ")") nms

showCase :: NotedCase a -> Padding -> String
showCase (NotedCase nme ne _) p = showMatchExpr nme p ++ " ->\n" ++ showExpr' ne (p + 2) ++ "\n"

showPM' :: NotedPM a -> Padding -> String
showPM' (NotedPM ne ncs _ _) p =
    showPadding p ++ "case " ++ showExpr' ne 0 ++ " with\n" ++ concatMap (`showCase` (p + 2)) ncs

showPM :: NotedPM a -> String
showPM npm = showPM' npm 0

showExpr' :: NotedExpr a -> Padding -> String
showExpr' (ExprVar nVar _) p = showPadding p ++ tokenRepToStr (repOf nVar)
showExpr' (ExprVal nVal _) p = showPadding p ++ tokenRepToStr (repOf nVal)
showExpr' (ExprDispatchVar nVar toks _) p =
    showPadding p ++ tokenRepToStr (repOf nVar) ++ concatMap (\tok -> " " ++ tokenRepToStr (repOf tok)) toks
showExpr' (ExprLam (NotedLam nVar ne _ _) _) p =
    showPadding p ++ "\\ " ++ tokenRepToStr (repOf nVar) ++ " -> \n" ++ showExpr' ne (p + 2)
showExpr' (ExprApp (NotedApp ne1 ne2 _ _) _) p =
    showPadding p ++ "(" ++ showExpr' ne1 0 ++ " " ++ showExpr' ne2 0 ++ ")"
showExpr' (ExprBound (NotedBound bnVar bnVars bne _) ne _) p =
    showPadding p ++ "let " ++ tokenRepToStr (repOf bnVar) ++ concatMap (\v -> " " ++ tokenRepToStr (repOf v)) bnVars ++
    " =\n" ++ showExpr' bne (p + 2) ++ "\n" ++ showPadding p ++ "in " ++ showExpr' ne 0
showExpr' (ExprPM npm _) p = showPM' npm p

showExpr :: NotedExpr a -> String
showExpr ne = showExpr' ne 0