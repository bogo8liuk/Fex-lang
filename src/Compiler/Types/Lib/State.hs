{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Types.Lib.State
    ( EmbeddedState(..)
    , BoxedState(..)
    , putWith
    , GenericState
    , EitherHandle
    , fetchData
    , updateData
    , initGenState
    , initNoFreeVarsState
    , getData
    , putData
    , genErr
) where

import Control.Monad.State
import Compiler.State as With
import Compiler.Types.Tables
import qualified Compiler.Types.Lib.FreeVars as Fresh

class EmbeddedState x where
    fetchFV :: x -> Fresh.FV ()
    fetchTypes :: x -> TypesTable With.ProgState
    fetchDataCons :: x -> DataConsTable With.ProgState
    fetchConts :: x -> ConstraintsTable With.ProgState
    fetchInsts :: x -> InstsTable With.ProgState
    fetchMethods :: x -> PropMethodsTable With.ProgState
    fetchImpls :: x -> ImplTable With.ProgState
    fetchProg :: x -> TypedProgram With.ProgState

    updateFV :: x -> Fresh.FV () -> x
    updateTypes :: x -> TypesTable With.ProgState -> x
    updateDataCons :: x -> DataConsTable With.ProgState -> x
    updateConts :: x -> ConstraintsTable With.ProgState -> x
    updateInsts :: x -> InstsTable With.ProgState -> x
    updateMethods :: x -> PropMethodsTable With.ProgState -> x
    updateImpls :: x -> ImplTable With.ProgState -> x
    updateProg :: x -> TypedProgram With.ProgState -> x

putWith :: MonadState x m => (x -> o -> x) -> o -> m ()
putWith update new = do
    st <- get
    put $ update st new

class (MonadState x m, EmbeddedState x) => BoxedState x m where
    getFV :: m (Fresh.FV ())
    getFV = gets fetchFV

    getTypes :: m (TypesTable With.ProgState)
    getTypes = gets fetchTypes

    getDataCons :: m (DataConsTable With.ProgState)
    getDataCons = gets fetchDataCons

    getConts :: m (ConstraintsTable With.ProgState)
    getConts = gets fetchConts

    getInsts :: m (InstsTable With.ProgState)
    getInsts = gets fetchInsts

    getMethods :: m (PropMethodsTable With.ProgState)
    getMethods = gets fetchMethods

    getImpls :: m (ImplTable With.ProgState)
    getImpls = gets fetchImpls

    getProg :: m (TypedProgram With.ProgState)
    getProg = gets fetchProg

    putFV :: Fresh.FV () -> m ()
    putFV = putWith updateFV

    putTypes :: TypesTable With.ProgState -> m ()
    putTypes = putWith updateTypes

    putDataCons :: DataConsTable With.ProgState -> m ()
    putDataCons = putWith updateDataCons

    putConts :: ConstraintsTable With.ProgState -> m ()
    putConts = putWith updateConts

    putInsts :: InstsTable With.ProgState -> m ()
    putInsts = putWith updateInsts

    putMethods :: PropMethodsTable With.ProgState -> m ()
    putMethods = putWith updateMethods

    putImpls :: ImplTable With.ProgState -> m ()
    putImpls = putWith updateImpls

    putProg :: TypedProgram With.ProgState -> m ()
    putProg = putWith updateProg

data GenericState a =
    GenSt
        (Fresh.FV ())
        (TypesTable With.ProgState)
        (DataConsTable With.ProgState)
        (ConstraintsTable With.ProgState)
        (InstsTable With.ProgState)
        (PropMethodsTable With.ProgState)
        (ImplTable With.ProgState)
        (TypedProgram With.ProgState)
        a

{- Generic stateful context with symbol tables, parametric also in the error. -}
type EitherHandle a err = StateT (GenericState a) (Either err)

instance EmbeddedState (GenericState a) where
    fetchFV (GenSt fv _ _ _ _ _ _ _ _) = fv

    fetchTypes (GenSt _ tt _ _ _ _ _ _ _) = tt

    fetchDataCons (GenSt _ _ dct _ _ _ _ _ _) = dct

    fetchConts (GenSt _ _ _ ct _ _ _ _ _) = ct

    fetchInsts (GenSt _ _ _ _ insts _ _ _ _) = insts

    fetchMethods (GenSt _ _ _ _ _ mths _ _ _) = mths

    fetchImpls (GenSt _ _ _ _ _ _ it _ _) = it

    fetchProg (GenSt _ _ _ _ _ _ _ tp _) = tp

    updateFV (GenSt _ tt dct ct insts mhts it tp x) fv = GenSt fv tt dct ct insts mhts it tp x

    updateTypes (GenSt fv _ dct ct insts mhts it tp x) tt = GenSt fv tt dct ct insts mhts it tp x

    updateDataCons (GenSt fv tt _ ct insts mhts it tp x) dct = GenSt fv tt dct ct insts mhts it tp x

    updateConts (GenSt fv tt dct _ insts mhts it tp x) ct = GenSt fv tt dct ct insts mhts it tp x

    updateInsts (GenSt fv tt dct ct _ mhts it tp x) insts = GenSt fv tt dct ct insts mhts it tp x

    updateMethods (GenSt fv tt dct ct insts _ it tp x) mhts = GenSt fv tt dct ct insts mhts it tp x

    updateImpls (GenSt fv tt dct ct insts mhts _ tp x) it = GenSt fv tt dct ct insts mhts it tp x

    updateProg (GenSt fv tt dct ct insts mhts it _ x) tp = GenSt fv tt dct ct insts mhts it tp x

fetchData :: GenericState a -> a
fetchData (GenSt _ _ _ _ _ _ _ _ x) = x

updateData :: GenericState a -> a -> GenericState a
updateData (GenSt fv tt dct ct insts mhts it tp _) = GenSt fv tt dct ct insts mhts it tp

instance BoxedState (GenericState a) (StateT (GenericState a) (Either err))

initGenState
    :: Fresh.FV ()
    -> TypesTable With.ProgState
    -> DataConsTable With.ProgState
    -> ConstraintsTable With.ProgState
    -> InstsTable With.ProgState
    -> PropMethodsTable With.ProgState
    -> ImplTable With.ProgState
    -> TypedProgram With.ProgState
    -> a
    -> GenericState a
initGenState = GenSt

initNoFreeVarsState
    :: TypesTable With.ProgState
    -> DataConsTable With.ProgState
    -> ConstraintsTable With.ProgState
    -> InstsTable With.ProgState
    -> PropMethodsTable With.ProgState
    -> ImplTable With.ProgState
    -> TypedProgram With.ProgState
    -> a
    -> GenericState a
initNoFreeVarsState = GenSt Fresh.newFreeVarsContainer

getData :: EitherHandle a err a
getData = gets fetchData

putData :: a -> EitherHandle a err ()
putData = putWith updateData

genErr :: err -> EitherHandle a err res
genErr err = lift $ Left err
