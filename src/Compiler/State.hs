module Compiler.State
    -- Re-exporting Text.Parsec position-tracking api
    ( SourceName
    , Line
    , Column
    , SourcePos
    , sourceName
    , sourceLine
    , sourceColumn
    -- module-defined
    --, CustomSourcePos     TODO: in this moment, no need to make it public
    , ProgState
    , ParamState
    , fetchTokenState
    , customState
    , paramState
    , startSrcLocFromState
    , endSrcLocFromState
) where

import Lib.Utils((<|))
import Text.Parsec
import SrcLoc
import FastString

{- Redefinition of SourcePos, in order to better handle values of this type. At the actual state of art
of Parsec library, api which handles SourcePos does not allow the arbitrary creation of a SourcePos
value and a SourcePos value can be fetched and handled only under the context of ParsecT monad. Moreover,
CustomSourcePos offers a way to define special cases of positioning. CustomSourcePos can be thought as:
Either something_exceptional SourcePos -}
data CustomSourcePos =
      Pos SourceName Line Column
    | Special String

showLineCol :: Line -> Column -> String
showLineCol line col = "line " ++ show line ++ ", column" ++ show col

instance Show CustomSourcePos where
    show (Pos "" line col) = showLineCol line col
    show (Pos sname line col) = "\"" ++ sname ++ "\", " ++ showLineCol line col
    show (Special desc) = desc

toCustom :: SourcePos -> CustomSourcePos
toCustom pos = Pos <| sourceName pos <| sourceLine pos <| sourceColumn pos

newtype ProgState = ProgState { pos :: CustomSourcePos
                              }

{- Like ProgState, but with ancillary data. -}
data ParamState a = ParamState { ppos :: CustomSourcePos
                               , ancillary :: a
                               }

instance Show ProgState where
    show ProgState { pos = sp } = show sp

instance Show a => Show (ParamState a) where
    show ParamState { ppos = sp
                    , ancillary = x
                    } =
        show x ++ ", " ++ show sp

fetchTokenState :: Monad m => ParsecT s u m ProgState
fetchTokenState = do
    p <- getPosition
    return (ProgState { pos = toCustom p })

{- It allows to define a custom state. -}
customState :: String -> ProgState
customState desc = ProgState { pos = Special desc
                             }

paramState :: a -> ProgState -> ParamState a
paramState x ps =
    ParamState { ppos = pos ps
               , ancillary = x
               }

-- Conversion to and creation of source location (SrcLoc type)

srcPosToSrcLoc :: CustomSourcePos -> SrcLoc
srcPosToSrcLoc (Pos src line col) =
    let fsSrc = mkFastString src in
        mkSrcLoc fsSrc line col
srcPosToSrcLoc (Special src) =
    let fsSrc = mkFastString src in
        mkGeneralSrcLoc fsSrc

startSrcLocFromState :: ProgState -> SrcLoc
startSrcLocFromState = srcPosToSrcLoc . pos

--TODO: refactor ProgState type and keep two different positions
endSrcLocFromState :: ProgState -> SrcLoc
endSrcLocFromState = startSrcLocFromState
