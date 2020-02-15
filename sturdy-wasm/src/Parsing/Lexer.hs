module Parsing.Lexer(
    identifier
    , anyKeyword
    , keyword
    , charLiteral
    , stringLiteral
    , natural
    , integer
    , float
    , naturalOrFloat
    , decimal
    , hexadecimal
    , symbol
    , lexeme
    , whiteSpace
    , parens
    , dot
) where

import Data.Char

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import qualified Text.ParserCombinators.Parsec.Token as T

import           Control.Monad

-- Currently the parser sometimes consumes a keyword regardless of which one, in order for that to work
-- there shouldn't be any explicitly reserved operators
-- types = ["i32", "i64", "f32", "f64"]
-- instructions = ["add", "sub", "mul", "get_local"]
-- keywords = types ++ instructions ++ ["param", "result", "func", "module"]
keywords = []

langDef = LanguageDef {

    T.commentStart = "(;",

    T.commentEnd = ";)",

    T.commentLine =";",

    T.nestedComments = False,

    -- The start of an identifier
    T.identStart = char '$',

    -- Can end with any idchar
    T.identLetter = alphaNum <|> oneOf "!#$%&′∗+−./:<=>?@∖^_`|~",

    -- Used for keywords
    T.opStart = satisfy isAsciiLower,

    -- Can end with any idchar (w/o '.' to simplify parsing)
    T.opLetter = alphaNum <|> oneOf "!#$%&′∗+−/:<=>?@∖^_`|~",

    -- Wasm doesn't have reserved identifiers (starting with a $)
    T.reservedNames = [],

    -- Keywords are reserved operator names
    T.reservedOpNames = keywords,

    T.caseSensitive = True

}

lexer :: T.TokenParser st
lexer = T.makeTokenParser langDef

identifier = T.identifier lexer
-- reserved   = T.reserved   lexer
anyKeyword = T.operator lexer -- Renamed to anyKeyword for consitency with webasm terminology and previous parser
keyword = T.reservedOp lexer -- Renamed to keyword for consitency with webasm terminology and previous parser
charLiteral = T.charLiteral lexer
stringLiteral = T.stringLiteral lexer
natural = T.natural lexer
integer = T.integer lexer
float = T.float lexer
naturalOrFloat = T.naturalOrFloat lexer
decimal = T.decimal lexer
hexadecimal = T.hexadecimal lexer
symbol = T.symbol lexer
lexeme = T.lexeme lexer
whiteSpace = T.whiteSpace lexer
parens = T.parens lexer
dot = T.dot lexer
