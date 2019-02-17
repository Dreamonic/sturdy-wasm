module LexerRegex(
    splitString
    , tokenize
    , tokenizeS
) where

import Tokens
import Text.Regex.PCRE

-- |    Split a string, containing multiple possible tokens, 
--      into a list based upon certain delimiters.
splitString:: String -> [String]
splitString str = getAllTextMatches $ str =~ "\\(|\\)|(\\\".*?\\\")|([^\\s\\(\\)])+"

-- |    Tokenize a string, which might contain multiple tokens.
tokenize :: String -> [Token]
tokenize str = tokenizeList $ splitString str

-- |    Tokenize a list of strings, 
--      of which each element represents a single token
tokenizeList :: [String] -> [Token]
tokenizeList list = case list of
    h:tail -> tokenizeS h : tokenizeList tail
    _ -> []

-- |    Tokenize a single element.
--
--      The following elements are possible
--          - "(": Left Parenthesis
--          - ")": Right Parenthesis
--          - "$id" : An ID, which always starts with a '$'
--          - ""string"": A string, which is surrounded with '"'
--          - "keyword": A keyword, which always starts with a lower case symbol
--          - "42": An unsigned number, every number without sign is parsed as such
--          - "-42": A signed number, every number with sign is parsed as such
--          - "42.0": A float, every number containing a '.' is parsed as such
--          - otherwise: It is a reserved symbol, which is not really known otherwise
tokenizeS :: String -> Token
tokenizeS token 
    | token == "(" = LP
    | token == ")" = RP
    | token =~ ("^\\$(" ++ idcharRegex ++ ")+$") = ID $ tail token
    | token =~ "^\\\"(.|\n)*?\\\"$" = Str $ tail $ init token
    | token =~ ("^[a-z](" ++ idcharRegex ++ ")*$") = Keyword token
    | token =~ "^[0-9]+$" = UnsignedN (read token :: Integer)
    | token =~ "^-[0-9]+$" = SignedN (read token :: Integer)
    | token =~ "^-?[0-9.]+$" = FloatN (read token :: Double)
    | otherwise = Reserved token
