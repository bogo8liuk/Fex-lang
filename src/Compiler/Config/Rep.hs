module Compiler.Config.Rep
    ( TokenRep
    , SymbolRep
    , TyConRep
    , TyVarRep
    , TypeRep
    , KindVarRep
    , KindConRep
    , KindRep
    , PropConRep
    , DataConRep
    , tokenRepToStr
    , symbolRepToStr
    , tyConRepToStr
    , tyVarRepToStr
    , typeRepToStr
    , kindVarRepToStr
    , kindConRepToStr
    , kindRepToStr
    , propConRepToStr
    , dataConRepToStr
    , tokenRepFromStr
    , symbolRepFromStr
    , tyConRepFromStr
    , tyVarRepFromStr
    , typeRepFromStr
    , kindVarRepFromStr
    , kindConRepFromStr
    , kindRepFromStr
    , propConRepFromStr
    , dataConRepFromStr
) where

{- Atomic representation of tokens: the rule is that an atomic representation has to be ALWAYS convertible to a string.
It represents the minimal "non-ambigous" representation of a token. -}

{- Atomic representation of whatever token -}
type TokenRep = String
{- Atomic representation of a symbol (classic program variable) -}
type SymbolRep = String
{- Atomic representation of a concrete (not a variable) type, namely a type-constructor -}
type TyConRep = String
{- Atomic representation of a type variable (whatever kind it is) -}
type TyVarRep = String
{- Atomic representation of whatever type -}
type TypeRep = String
{- Atomic representation of a kind variable -}
type KindVarRep = String
{- Atomic representation of a concrete (not a variable) kind -}
type KindConRep = String
{- Atomic representation of whatever kind -}
type KindRep = String
{- Atomic representation of a concrete property -}
type PropConRep = String
{- Atomic representation of a data-constructor -}
type DataConRep = String

tokenRepToStr :: TokenRep -> String
tokenRepToStr = id

symbolRepToStr :: SymbolRep -> String
symbolRepToStr = id

tyConRepToStr :: TyConRep -> String
tyConRepToStr = id

tyVarRepToStr :: TyVarRep -> String
tyVarRepToStr = id

typeRepToStr :: TypeRep -> String
typeRepToStr = id

kindVarRepToStr :: KindVarRep -> String
kindVarRepToStr = id

kindConRepToStr :: KindConRep -> String
kindConRepToStr = id

kindRepToStr :: String -> KindRep
kindRepToStr = id

propConRepToStr :: String -> PropConRep
propConRepToStr = id

dataConRepToStr :: String -> DataConRep
dataConRepToStr = id

tokenRepFromStr :: String -> TokenRep
tokenRepFromStr = id

symbolRepFromStr :: String -> SymbolRep
symbolRepFromStr = id

tyConRepFromStr :: String -> TyConRep
tyConRepFromStr = id

tyVarRepFromStr :: String -> TyVarRep
tyVarRepFromStr = id

typeRepFromStr :: String -> TypeRep
typeRepFromStr = id

kindVarRepFromStr :: String -> KindVarRep
kindVarRepFromStr = id

kindConRepFromStr :: String -> KindConRep
kindConRepFromStr = id

kindRepFromStr :: String -> KindRep
kindRepFromStr = id

propConRepFromStr :: String -> PropConRep
propConRepFromStr = id

dataConRepFromStr :: String -> DataConRep
dataConRepFromStr = id
