module Compiler.Codegen.IO
    ( mkIOType
) where

--Look at `runMainIO` and `runMainIOName`

import Type
import TysPrim
import TysWiredIn
import BasicTypes

{- It builds the type:

    State# RealWorld -> (# State# RealWorld, resTy #)

where `resTy` is the argument of mkIOType, in order to make whatever type.
-}
mkIOType :: Type -> Type
mkIOType resTy = mkVisFunTy realWorldStatePrimTy $ mkTupleTy Unboxed [realWorldStatePrimTy, resTy]