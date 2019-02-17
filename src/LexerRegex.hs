module LexerRegex(
    splitString
    , tokenize
    , tokenizeS
) where

import Tokens
import Text.Regex.PCRE


splitString:: String -> [String]
splitString str = getAllTextMatches $ str =~ "\\(|\\)|(\\\".*?\\\")|([^\\s\\(\\)])+"

tokenize :: String -> [Token]
tokenize str = tokenizeList $ splitString str

tokenizeList :: [String] -> [Token]
tokenizeList list = case list of
    h:tail -> tokenizeS h : tokenizeList tail
    _ -> []

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
