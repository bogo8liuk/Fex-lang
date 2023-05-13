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
    , compTokenRepFromStr
) where

{- Atomic representation of tokens: the rule is that an atomic representation has to be ALWAYS convertible to a string.
It represents the minimal "non-ambigous" representation of a token. -}

{- Atomic representation of whatever token -}
newtype TokenRep = TR String deriving (Eq, Ord)

instance Show TokenRep where
    show = tokenRepToStr

instance Semigroup TokenRep where
    (<>) (TR r) (TR r') = TR (r ++ r')

instance Monoid TokenRep where
    mempty = TR ""

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
tokenRepToStr (TR r) = r

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

kindRepToStr :: KindRep -> String
kindRepToStr = id

propConRepToStr :: PropConRep -> String
propConRepToStr = id

dataConRepToStr :: DataConRep -> String
dataConRepToStr = id

{- Construction from strings. NB: this can be change in the future. -}

tokenRepFromStr :: String -> TokenRep
tokenRepFromStr = TR

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

compTokenRepFromStr :: String -> CompTokenRep
compTokenRepFromStr = id
