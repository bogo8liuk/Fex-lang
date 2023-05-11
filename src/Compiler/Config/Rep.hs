module Compiler.Config.Rep
    ( TokenRep(..)
    , SymbolRep
    , TyConRep
    , TyVarRep
    , TypeRep
    , KindVarRep
    , KindConRep
    , KindRep
    , PropConRep
    , DataConRep
    , CompTokenRep
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
    , symbolRepFromStr
    , symbolRepFromStr'
    , tyConRepFromStr
    , tyConRepFromStr'
    , tyVarRepFromStr
    , tyVarRepFromStr'
    , typeRepFromStr
    , kindVarRepFromStr
    , kindVarRepFromStr'
    , kindConRepFromStr
    , kindConRepFromStr'
    , kindRepFromStr
    , propConRepFromStr
    , propConRepFromStr'
    , dataConRepFromStr
    , dataConRepFromStr'
    , compTokenRepFromStr
    , compTokenRepFromStr'
) where

{- Atomic representation of tokens: the rule is that an atomic representation has to be ALWAYS convertible to a string.
It represents the minimal "non-ambigous" representation of a token. -}

{- Atomic representation of whatever token -}
data TokenRep =
      SymRep SymbolRep
    | TyConRep TyConRep
    | TyVarRep TyVarRep
    | KindVarRep KindVarRep
    | KindConRep KindConRep
    | PropConRep PropConRep
    | DataConRep DataConRep
    | CompTokRep CompTokenRep
    deriving (Eq, Ord)

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
{- Atomic representation of something evaluated at compile-time. TODO: use a finer granularity -}
type CompTokenRep = String

tokenRepToStr :: TokenRep -> String
tokenRepToStr (SymRep r) = r
tokenRepToStr (TyConRep r) = r
tokenRepToStr (TyVarRep r) = r
tokenRepToStr (KindVarRep r) = r
tokenRepToStr (KindConRep r) = r
tokenRepToStr (PropConRep r) = r
tokenRepToStr (DataConRep r) = r
tokenRepToStr (CompTokRep r) = r

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

symbolRepFromStr :: String -> SymbolRep
symbolRepFromStr = id

symbolRepFromStr' :: String -> TokenRep
symbolRepFromStr' = SymRep

tyConRepFromStr :: String -> TyConRep
tyConRepFromStr = id

tyConRepFromStr' :: String -> TokenRep
tyConRepFromStr' = TyConRep

tyVarRepFromStr :: String -> TyVarRep
tyVarRepFromStr = id

tyVarRepFromStr' :: String -> TokenRep
tyVarRepFromStr' = TyVarRep

typeRepFromStr :: String -> TypeRep
typeRepFromStr = id

kindVarRepFromStr :: String -> KindVarRep
kindVarRepFromStr = id

kindVarRepFromStr' :: String -> TokenRep
kindVarRepFromStr' = KindVarRep

kindConRepFromStr :: String -> KindConRep
kindConRepFromStr = id

kindConRepFromStr' :: String -> TokenRep
kindConRepFromStr' = KindConRep

kindRepFromStr :: String -> KindRep
kindRepFromStr = id

propConRepFromStr :: String -> PropConRep
propConRepFromStr = id

propConRepFromStr' :: String -> TokenRep
propConRepFromStr' = PropConRep

dataConRepFromStr :: String -> DataConRep
dataConRepFromStr = id

dataConRepFromStr' :: String -> TokenRep
dataConRepFromStr' = DataConRep

compTokenRepFromStr :: String -> CompTokenRep
compTokenRepFromStr = id

compTokenRepFromStr' :: String -> TokenRep
compTokenRepFromStr' = CompTokRep
