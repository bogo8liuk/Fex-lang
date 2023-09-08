{- |
Module : Compiler.Lib.Pos
Description : Source position tracking
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Types for position tracking in source code.
-}

module Compiler.Lib.Pos
    (
    -- * Positions

    -- $position
      InitPos
    , EndPos
    , PhysicalPos(..)
    , ProgramPos(..)
    -- * Notion of position
    , initPos
    , initPos'
    , endPos
    , endPos'
    , HasPosition(..)
    , physicalPositionOf
    , hasPhysicalPosition
    , specialPositionOf
    , hasSpecialPosition
) where

import Utils.TypeAlias (Typing (..))
import Text.Parsec.Pos (SourceName, Line, Column)
import Utils.Data.Text.TypeAlias (Description)
import Data.Text (unpack)
import Data.Maybe (isJust)

-- $position
-- A position in a source consists of three pieces of information:
--
--     1) a source name;
--     2) a starting point;
--     3) an ending point.
--
-- Both the starting point and ending point consist of a pair made by a line
-- number and a column number. Usually, the line number of the starting point
-- should be lesser than or, at least, equal to the line number of the ending
-- point.
--
-- A position can also be \"special\", which means that the position is not
-- part of an existing source.

type InitPos = (Line, Column)
type EndPos = (Line, Column)

{- |
A (physical) position in a source.
-}
data PhysicalPos = PhysicalPos !SourceName !InitPos !EndPos

showPoint :: (Line, Column) -> String
showPoint (line, column) = "line " ++ show line ++ " and column " ++ show column

instance Show PhysicalPos where
    show (PhysicalPos srcName begin end) =
        srcName ++ ", from " ++ showPoint begin ++ " to " ++ showPoint end

instance Eq PhysicalPos where
    (==) (PhysicalPos srcName begin end) (PhysicalPos srcName' begin' end') =
        srcName == srcName' &&
        begin == begin' &&
        end == end'

{- |
A position in a program.
-}
data ProgramPos
    -- | A physical position in a source.
    = Pos !PhysicalPos
    -- | A special case of position, it should be used for tokens defined
    -- \"natively\", so the tokens which doesn't have a real source.
    | Special !Description

instance Show ProgramPos where
    show (Pos srcPos) = show srcPos
    show (Special desc) = unpack $ unTyping desc

instance Eq ProgramPos where
    (==) (Pos srcPos) (Pos srcPos') = srcPos == srcPos'
    (==) (Special desc) (Special desc') = desc == desc'
    (==) _ _ = False

initPos :: PhysicalPos -> InitPos
initPos (PhysicalPos _ begin _) = begin

initPos' :: PhysicalPos -> (SourceName, Line, Column)
initPos' (PhysicalPos srcName begin _) =
    (srcName, fst begin, snd begin)

endPos :: PhysicalPos -> EndPos
endPos (PhysicalPos _ _ end) = end

endPos' :: PhysicalPos -> (SourceName, Line, Column)
endPos' (PhysicalPos srcName _ end) =
    (srcName, fst end, snd end)

{- |
For items which can tell their position in a program.
-}
class HasPosition a where
    positionOf :: a -> ProgramPos

instance HasPosition ProgramPos where
    positionOf = id

instance HasPosition PhysicalPos where
    positionOf = Pos

physicalPositionOf :: HasPosition a => a -> Maybe PhysicalPos
physicalPositionOf token =
    case positionOf token of
        Pos srcPos -> Just srcPos
        Special _ -> Nothing

hasPhysicalPosition :: HasPosition a => a -> Bool
hasPhysicalPosition = isJust . physicalPositionOf

specialPositionOf :: HasPosition a => a -> Maybe Description
specialPositionOf token =
    case positionOf token of
        Pos _ -> Nothing
        Special desc -> Just desc

hasSpecialPosition :: HasPosition a => a -> Bool
hasSpecialPosition = isJust . specialPositionOf
