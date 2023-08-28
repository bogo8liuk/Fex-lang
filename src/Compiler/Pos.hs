{- |
Module : Compiler.Pos
Description : Source position tracking
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Types for position tracking in source code.
-}

module Compiler.Pos
    (
    -- * Positions

    -- $position
      InitPos
    , EndPos
    , SourcePos(..)
    , ProgramPos(..)
    -- * Notion of position
    , isPhysical
    , isSpecial
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

data Init
type InitPos = Typing Init (Line, Column)

data End
type EndPos = Typing End (Line, Column)

{- |
A position in a source.
-}
data SourcePos = SourcePos !SourceName !InitPos !EndPos

showPoint :: (Line, Column) -> String
showPoint (line, column) = "line " ++ show line ++ " and column " ++ show column

instance Show SourcePos where
    show (SourcePos srcName begin end) =
        srcName ++ ", from " ++ showPoint begin' ++ " to " ++ showPoint end'
        where
            begin' = unTyping begin
            end' = unTyping end

instance Eq SourcePos where
    (==) (SourcePos srcName begin end) (SourcePos srcName' begin' end') =
        srcName == srcName' &&
        begin == begin' &&
        end == end'

{- |
A position in a program.
-}
data ProgramPos
    -- | A physical position in a source.
    = Pos !SourcePos
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

isPhysical :: ProgramPos -> Bool
isPhysical (Pos _) = True
isPhysical (Special _) = False

isSpecial :: ProgramPos -> Bool
isSpecial (Pos _) = False
isSpecial (Special _) = True

initPos :: SourcePos -> InitPos
initPos (SourcePos _ begin _) = begin

initPos' :: SourcePos -> (SourceName, Line, Column)
initPos' (SourcePos srcName begin _) =
    (srcName, fst begin', snd begin')
    where
        begin' = unTyping begin

endPos :: SourcePos -> EndPos
endPos (SourcePos _ _ end) = end

endPos' :: SourcePos -> (SourceName, Line, Column)
endPos' (SourcePos srcName _ end) =
    (srcName, fst end', snd end')
    where
        end' = unTyping end

{- |
For items which can tell their position.
-}
class HasPosition a where
    positionOf :: a -> ProgramPos

instance HasPosition SourcePos where
    positionOf = Pos

physicalPositionOf :: HasPosition a => a -> Maybe SourcePos
physicalPositionOf token =
    case positionOf token of
        Pos srcPos -> Just srcPos
        Special _ -> Nothing

hasPhysicalPosition :: HasPosition a => a -> Bool
hasPhysicalPosition = isPhysical . positionOf

specialPositionOf :: HasPosition a => a -> Maybe Description
specialPositionOf token =
    case positionOf token of
        Pos _ -> Nothing
        Special desc -> Just desc

hasSpecialPosition :: HasPosition a => a -> Bool
hasSpecialPosition = isSpecial . positionOf
