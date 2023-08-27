{- |
Module : Compiler.Syntax.Refactoring.Language
Description : Text-like literals parsers
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

This piece of code is taken directly from Parsec library
https://hackage.haskell.org/package/parsec-3.1.16.1/docs/src/Text.Parsec.Token.html#makeTokenParser.
The aim is to have a generic way to parse text-like literals (namely char
literals and string literals) enclosing. Actually, Parsec exposes only two
functions (`charLiteral` and `stringLiteral`) which parses literals according
to Haskell standard and it doesn't parameterize the enclosing of literals. This
module exposes functions to parse just what is enclosed in a literal and not
the enclosing of the literal.
-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.Syntax.Refactoring.TextLiterals
    ( validCharLiteral
    , validStringLiteral
) where
import Text.Parsec (ParsecT, char, choice, many1, digit, upper, (<|>),
    octDigit, hexDigit, string, try, (<?>), satisfy, Stream, many, space)
import Data.Char (digitToInt)
import Data.Foldable (foldl')

{- |
It parses a single character valid to be enclosed in a character literal.
-}
validCharLiteral :: Stream s m Char => ParsecT s u m Char
validCharLiteral = charLetter <|> charEscape
    where
        charEscape = do
            char '\\'
            escapeCode

        charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

{- |
It parses a sequence valid to be enclosed in a string literal.
-}
validStringLiteral :: Stream s m Char => ParsecT s u m String
validStringLiteral = do
    chars <- many stringChar
    return (foldr (maybe id (:)) "" chars)

charControl :: Stream s m Char => ParsecT s u m Char
charControl = do
    char '^'
    code <- upper
    return (toEnum (fromEnum code - fromEnum 'A' + 1))

number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

decimal :: Stream s m Char => ParsecT s u m Integer
decimal = number 10 digit

charNum :: Stream s m Char => ParsecT s u m Char
charNum = do
    code <-
        decimal
            <|> do { char 'o'; number 8 octDigit }
            <|> do { char 'x'; number 16 hexDigit }
    if code > 0x10FFFF
    then fail "invalid escape sequence"
    else return (toEnum (fromInteger code))

charEsc :: Stream s m Char => ParsecT s u m Char
charEsc =
    choice (map parseEsc escMap)
    where
        parseEsc (c, code) = do
            char c
            return code

        escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

charAscii :: Stream s m Char => ParsecT s u m Char
charAscii =
    choice (map parseAscii asciiMap)
    where
        parseAscii (asc, code) = try $ do
            string asc
            return code

        asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

        ascii2codes =
            [ "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS", "GS"
            , "RS", "US", "SP"
            ]
        ascii3codes =
            [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "DLE"
            , "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "SUB"
            , "ESC", "DEL"
            ]

        ascii2 =
            [ '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\EM'
            , '\FS', '\GS', '\RS', '\US', '\SP'
            ]
        ascii3 =
            [ '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK', '\BEL'
            , '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB'
            , '\CAN', '\SUB', '\ESC', '\DEL'
            ]

escapeCode :: Stream s m Char => ParsecT s u m Char
escapeCode =
    charEsc <|> charNum <|> charAscii <|> charControl <?> "escape code"

stringChar :: Stream s m Char => ParsecT s u m (Maybe Char)
stringChar =
    justStringLetter <|> stringEscape <?> "string character"
    where
        justStringLetter = do
            c <- stringLetter
            return (Just c)

stringLetter :: Stream s m Char => ParsecT s u m Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape :: Stream s m Char => ParsecT s u m (Maybe Char)
stringEscape = do
    { char '\\'
    ;   passEscapeGap
    <|> passEscapeEmpty
    <|> justEscapeCode
    }
    where
        passEscapeGap = do
            escapeGap
            return Nothing

        passEscapeEmpty = do
            escapeEmpty
            return Nothing

        justEscapeCode = do
            esc <- escapeCode
            return $ Just esc

escapeEmpty :: Stream s m Char => ParsecT s u m Char
escapeEmpty = char '&'

escapeGap :: Stream s m Char => ParsecT s u m Char
escapeGap = do
    many1 space
    char '\\' <?> "end of string gap"
