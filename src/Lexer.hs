module Lexer(
    Token (..)
    , tokenizeString
) where

import Data.Char (isDigit)

data Token
    = Keyword String
    | UnsignedN Integer
    | SignedN Integer
    | FloatN Double
    | Str String
    | ID String
    | LP
    | RP
    | Reserved String
    deriving (Show, Eq)

tokenizeString :: String -> Token
tokenizeString str = case str of
    "(" -> LP
    ")" -> RP
    _ -> tokenizeStringHelper str


tokenizeStringHelper :: String -> Token
tokenizeStringHelper str
    | head str == '$' = ID $ drop 1 str
    | head str == '\"' && last str == '\"' = Str $ drop 1 $ init str
    | checkKeyword str = Keyword str
    | otherwise = tokenizeNumber str

tokenizeNumber :: String -> Token
tokenizeNumber str = case dropWhile isDigit str of
    "" -> UnsignedN (read str :: Integer)
    '-':decimals -> case dropWhile isDigit decimals of
        "" -> SignedN (read str :: Integer)
    '.':decimals -> case dropWhile isDigit decimals of
        "" -> FloatN (read str :: Double)
    _ -> Reserved str

checkKeyword :: String -> Bool
checkKeyword str = head str >= 'a' && head str <= 'z' && all isIDChar str

isIDChar:: Char -> Bool
isIDChar c
    | c >= '0' && c <='9' = True
    | c >= 'A' && c <='Z' = True
    | c >= 'a' && c <='z' = True
    | elem c "!#$%&*+-./" = True
    | elem c ":<=>?@\\~_|" = True
    | otherwise = False
