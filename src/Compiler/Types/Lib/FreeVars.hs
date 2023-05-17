module Compiler.Types.Lib.FreeVars
    ( FV
    , newFreeVarsContainer
    , freeVars
    , freeVars'
    , allocFreeVar
    , tryAllocFreeVar
    , getFreeVar
    , deallocFreeVar
    , deallocAllFreeVars
    , resetAllFreeVars
) where

import Data.Map.Strict as M hiding (map)
import qualified Utils.Data.Counter as C

data FV a =
    FV
        { counter :: C.AlphabeticCounterObj
        , vars :: Map String a
        }

instance Functor FV where
    fmap f fv =
        FV
            { counter = counter fv
            , vars = f <$> vars fv
            }

newFreeVarsContainer :: FV a
newFreeVarsContainer =
    FV
        { counter = C.new
        , vars = empty
        }

freeVars :: FV a -> [String]
freeVars = keys . vars

freeVars' :: FV a -> [(String, a)]
freeVars' = assocs . vars

allocFreeVar :: a -> FV a -> (String, FV a)
allocFreeVar info fv =
    let (var, c) = C.next $ counter fv in
        {- It tries allocating the next free variable until it hits the suitable one. -}
        case tryAllocFreeVar var info fv of
            Nothing -> allocFreeVar info $
                fv
                    { counter = c
                    }
            Just fv' ->
                ( var
                , fv'
                    { counter = c
                    }
                )

{- It tries allocating an arbitrary free variable. -}
tryAllocFreeVar :: String -> a -> FV a -> Maybe (FV a)
tryAllocFreeVar var info fv =
    let m = vars fv in
        case M.lookup var m of
            Nothing ->
                Just $ fv
                    { vars = insert var info m
                    }
            Just _ -> Nothing

getFreeVar :: String -> FV a -> Maybe a
getFreeVar var fv = M.lookup var $ vars fv

{- It deallocates a free variable. The deallocated variable will be no more instantiatable (namely, new allocated
free variables will be certainly different from the deallocated variable). -}
deallocFreeVar :: String -> FV a -> FV a
deallocFreeVar var fv =
    fv
        { vars = delete var $ vars fv
        }

{- It deallocates all stored free variables, but new allocated variables will be certainly different from deallocated
variables. -}
deallocAllFreeVars :: FV a -> FV a
deallocAllFreeVars fv =
    fv
        { vars = empty
        }

{- It deallocates all stored free variables and new allocated variables could be the same of deallocated ones. -}
resetAllFreeVars :: FV a -> FV a
resetAllFreeVars _ = newFreeVarsContainer