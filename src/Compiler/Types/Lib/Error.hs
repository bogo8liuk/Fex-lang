{-# LANGUAGE FlexibleInstances #-}

module Compiler.Types.Lib.Error
    ( Ty.ContBuildError(..)
    , ContSpErr
) where

import Lib.Result(Description(..))
import Compiler.Ast.Common
import qualified Compiler.State as With
import qualified Compiler.Ast.Typed as Ty

type ContSpErr = Ty.ContBuildError With.ProgState

instance Description ContSpErr where
    descOf (Ty.ArgsNoErr lnc ts) =
        "Constraint at " ++ show (stateOf lnc) ++ " for " ++ tokenRepToStr (repOf lnc) ++
        " has a different number of arguments (" ++ show (length $ argsOf lnc) ++ "), but the the types applied are " ++
        show (length ts)
    descOf (Ty.TypeBuildErr ty) =
        "Type " ++ Ty.showLHTy ty ++ " at " ++ show (stateOf ty) ++ " has been built in an inconsistent way"
    descOf (Ty.KindErr (lvty, lk) (lhty, lk')) =
        "Expected kind " ++ show lk ++ " at " ++ show (stateOf lvty) ++ ", but " ++ Ty.showLHTy lhty ++ " at " ++
        show (stateOf lhty) ++ " has kind " ++ show lk'
