-- | This module takes a text input and transforms it into a few different tokens.
module Lexer(
    tokenizeS
    , tokenize
) where

import Data.Char (isDigit)
import Text.Regex.PCRE
import Tokens


data Tokenizable a
    = CompleteToken a
    | IncompleteToken a
    deriving (Show, Eq)

-- | Create a list of tokens, from a string containing multiple tokens.
tokenize :: String -> [Token]
tokenize str = reverse $ snd $ tokenizeStream str ("", [])

-- | tokenizeSyma single token, contained within a string.
tokenizeS :: String -> Token
tokenizeS str = case str of
    "(" -> LP
    ")" -> RP
    _ -> tokenizeStringHelper str

-- Helper functions --

-- | tokenizeSyma stream of tokens [Note: Can be improved].
tokenizeStream :: String -> (String, [Token]) -> (String, [Token])
tokenizeStream str (mem, tokens)
    | str == [] = case mem of
        [] -> ("", tokens)
        _ -> ("", tokenizeS (trim mem) : tokens)
    | head str == '(' || head str == ')' = case trim mem of
        [] -> tokenizeStream (tail str) ("", tokenizeS [head str] : tokens)
        _ -> tokenizeStream (tail str) ("",  tokenizeS [head str] : tokenizeSym(findToken (head str) mem): tokens)
    | isCompleteToken (findToken (head str) mem) = tokenizeStream (tail str) ("", tokenizeSym(findToken (head str) mem) : tokens)
    | otherwise = tokenizeStream (tail str) (tokenizableToString (findToken (head str) mem), tokens)

-- | Casts a token to a string.
tokenizableToString :: Tokenizable String -> String
tokenizableToString (CompleteToken str) = str
tokenizableToString (IncompleteToken str) = str

-- | Check if a tokenizable is complete, or is still missing some information.
isCompleteToken :: Tokenizable a -> Bool
isCompleteToken (CompleteToken _) = True
isCompleteToken (IncompleteToken _) = False


-- | Find out if the character will complete the token.
findToken :: Char -> String -> Tokenizable String
findToken char str
    | char == '(' || char == ')' = CompleteToken str
    | [char] =~ "\\s" && str /= "" = CompleteToken str
    | [char] =~ "\\s" && str == "" = IncompleteToken str
    | otherwise = IncompleteToken $ str ++ [char]

-- | Trim whitespaces from the strings that will be tokenized
trim :: String -> String
trim str
    | str == "" = ""
    | [last str]  =~ "\\s" = trim $ init str
    | [head str]  =~ "\\s" = trim $ tail str
    | otherwise = str

-- | tokenize a stream of characters.
tokenizeCharStream :: String -> [Token] -> [Token]
tokenizeCharStream str tokens = tokenizeS str : tokens

tokenizeSym:: Tokenizable String -> Token
tokenizeSym(CompleteToken str) = case str of
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
