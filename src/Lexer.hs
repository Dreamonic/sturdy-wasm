module Lexer( 
    Token (..)
    , tokenizeString
    , tokenizeAll
) where

import Data.Char (isDigit)

data Token 
    = Keyword String
    | UnsignedN Integer
    | SignedN Integer
    | FloatN Float
    | Str String
    | ID String
    | LP
    | RP
    | Reserved String
    deriving (Show, Eq)

data Tokenizable a 
    = CompleteToken a
    | IncompleteToken a
    deriving (Show, Eq)

tokenizeAll :: String -> [Token]
tokenizeAll str = reverse $ snd $ tokenizeStream str ("", [])

tokenizeStream :: String -> (String, [Token]) -> (String, [Token])
tokenizeStream str (mem, tokens) 
    | str == [] = case mem of
        [] -> ("", tokens)
        _ -> ("", tokenizeString (trim mem) : tokens)
    | head str == '(' || head str == ')' = case trim mem of
        [] -> tokenizeStream (tail str) ("", tokenizeString [head str] : tokens)
        _ -> tokenizeStream (tail str) ("",  tokenizeString [head str] : tokenize (findToken (head str) mem): tokens)
    | isCompleteToken (findToken (head str) mem) = tokenizeStream (tail str) ("", tokenize (findToken (head str) mem) : tokens)
    | otherwise = tokenizeStream (tail str) (tokenizableToString (findToken (head str) mem), tokens)

tokenizableToString :: Tokenizable String -> String
tokenizableToString (CompleteToken str) = str
tokenizableToString (IncompleteToken str) = str

isCompleteToken :: Tokenizable a -> Bool
isCompleteToken (CompleteToken _) = True
isCompleteToken (IncompleteToken _) = False

findToken :: Char -> String -> Tokenizable String
findToken char str 
    | char == '(' || char == ')' = CompleteToken str
    | char == ' ' && str /= "" = CompleteToken str
    | otherwise = IncompleteToken $ str ++ [char]

trim :: String -> String
trim str 
    | str == "" = ""
    | last str  == ' ' = trim $ init str
    | head str  == ' ' = trim $ tail str
    | otherwise = str

tokenizeCharStream :: String -> [Token] -> [Token]
tokenizeCharStream str tokens = tokenizeString str : tokens

tokenize :: Tokenizable String -> Token
tokenize (CompleteToken str) = case str of
    "(" -> LP
    ")" -> RP
    _ -> tokenizeStringHelper str

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
        "" -> FloatN (read str :: Float)
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