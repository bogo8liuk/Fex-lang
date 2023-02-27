module Lib.Result
    ( DebugShow(..)
    , InfoShow(..)
    , unexpNoInfo
    , UnreachableState(..)
    , Description(..)
) where

class DebugShow a where
    dbgShow :: a -> String

{- The difference between DebugShow and InfoShow is semantic: in no-debug mode, the user should not know
the low-level reasons of a reached state (unless he specifies otherwise, but this is not handled here). -}
class InfoShow a where
    infoShow :: a -> String

{- Exporting a string which tells about an unexpected error, without giving information. -}
unexpNoInfo :: String
unexpNoInfo = "The compiler reached an unexpected state..."

class UnreachableState e where
    {- Given an error, if the error is due to an unreachable state of a computation, it returns the reason
    (in form of a string), else it returns Nothing. -}
    isUnreachable :: e -> Maybe String

{- For those types which have values that can be "descripted". -}
class Description a where
    descOf :: a -> String

instance (InfoShow err, InfoShow ok) => InfoShow (Either err ok) where
    infoShow (Left e) = infoShow e
    infoShow (Right s) = infoShow s

instance (DebugShow err, DebugShow ok) => DebugShow (Either err ok) where
    dbgShow (Left e) = dbgShow e
    dbgShow (Right s) = dbgShow s
