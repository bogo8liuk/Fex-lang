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
type SymbolRep = TokenRep
{- Atomic representation of a concrete (not a variable) type, namely a type-constructor -}
type TyConRep = TokenRep
{- Atomic representation of a type variable (whatever kind it is) -}
type TyVarRep = TokenRep
{- Atomic representation of whatever type -}
type TypeRep = TokenRep
{- Atomic representation of a kind variable -}
type KindVarRep = TokenRep
{- Atomic representation of a concrete (not a variable) kind -}
type KindConRep = TokenRep
{- Atomic representation of whatever kind -}
type KindRep = TokenRep
{- Atomic representation of a concrete property -}
type PropConRep = TokenRep
{- Atomic representation of a data-constructor -}
type DataConRep = TokenRep
{- Atomic representation of something evaluated at compile-time. TODO: use a finer granularity -}
type CompTokenRep = TokenRep

tokenRepToStr :: TokenRep -> String
tokenRepToStr (TR r) = r

symbolRepToStr :: SymbolRep -> String
symbolRepToStr = tokenRepToStr

tyConRepToStr :: TyConRep -> String
tyConRepToStr = tokenRepToStr

tyVarRepToStr :: TyVarRep -> String
tyVarRepToStr = tokenRepToStr

typeRepToStr :: TypeRep -> String
typeRepToStr = tokenRepToStr

kindVarRepToStr :: KindVarRep -> String
kindVarRepToStr = tokenRepToStr

kindConRepToStr :: KindConRep -> String
kindConRepToStr = tokenRepToStr

kindRepToStr :: KindRep -> String
kindRepToStr = tokenRepToStr

propConRepToStr :: PropConRep -> String
propConRepToStr = tokenRepToStr

dataConRepToStr :: DataConRep -> String
dataConRepToStr = tokenRepToStr

{- Construction from strings. NB: this can be change in the future. -}

tokenRepFromStr :: String -> TokenRep
tokenRepFromStr = TR

symbolRepFromStr :: String -> SymbolRep
symbolRepFromStr = tokenRepFromStr

tyConRepFromStr :: String -> TyConRep
tyConRepFromStr = tokenRepFromStr

tyVarRepFromStr :: String -> TyVarRep
tyVarRepFromStr = tokenRepFromStr

typeRepFromStr :: String -> TypeRep
typeRepFromStr = tokenRepFromStr

kindVarRepFromStr :: String -> KindVarRep
kindVarRepFromStr = tokenRepFromStr

kindConRepFromStr :: String -> KindConRep
kindConRepFromStr = tokenRepFromStr

kindRepFromStr :: String -> KindRep
kindRepFromStr = tokenRepFromStr

propConRepFromStr :: String -> PropConRep
propConRepFromStr = tokenRepFromStr

dataConRepFromStr :: String -> DataConRep
dataConRepFromStr = tokenRepFromStr

compTokenRepFromStr :: String -> CompTokenRep
compTokenRepFromStr = tokenRepFromStr
