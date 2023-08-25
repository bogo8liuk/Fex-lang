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
to Haskell standard and it doesn't parameterize the enclosing of literals.
-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.Syntax.Refactoring.TextLiterals
    (
) where
import Text.Parsec (ParsecT, char, choice, many1, digit, upper, (<|>), octDigit, hexDigit, string, try, (<?>), satisfy, Stream)
import Data.Char (digitToInt)

charControl :: Stream s m Char => ParsecT s u m Char
charControl = do
    char '^'
    code <- upper
    return (toEnum (fromEnum code - fromEnum 'A' + 1))

number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

decimal = number 10 digit

charNum :: ParsecT s u m Char
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

charLiteralInternal :: Stream s m Char => ParsecT s u m Char
charLiteralInternal = charLetter <|> charEscape
    where
        charEscape = do
            char '\\'
            escapeCode

        charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))
