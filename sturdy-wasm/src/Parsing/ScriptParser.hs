module Parsing.ScriptParser where

import Text.ParserCombinators.Parsec

import Data.Char
import Data.Text
import qualified Data.Text as T

import Syntax
import Types hiding ((<|>))
import Parsing.Parser
import Parsing.Lexer

parens' = try . parens

type Name = Maybe String
type Failure = String

type Script = [Command]

data Command
    = Module Module
    | Register String Name
    | Action Action
    | Assertion Assert
    | Meta Meta
    deriving (Show, Eq)

data Module
    = Base WasmModule
    | BinaryModule Name [String]
    | QuoteModule Name [String]
    deriving (Show, Eq)

-- The first argument is the optional module name
-- If no module name is given, use the global module
data Action
    = Invoke Name String [Instr]
    | Get Name String
    deriving (Show, Eq)

data Assert
    = AssertReturn Action [Instr]
    | AssertReturnCanonicalNaN Action
    | AssertReturnArithmicNaN Action
    | AssertTrap Action Failure
    | AssertExhaustion Action Failure
    | AssertMalformed Module Failure
    | AssertInvalid Module Failure
    | AssertUnlinkable Module Failure
    | AssertModuleTrap Module Failure
    deriving (Show, Eq)

data Meta
    = MScript Name Script
    | Input Name String
    | Output Name (Maybe String)
    deriving (Show, Eq)

maybeName :: Parser (Maybe String)
maybeName = optionMaybe identifier

binaryModule :: Parser Module
binaryModule = parens' $ do
    keyword "module"
    name <- maybeName
    keyword "binary"
    modules <- many1 stringLiteral
    return $ BinaryModule name modules

quoteModule :: Parser Module
quoteModule = parens' $ do
    keyword "module"
    name <- maybeName
    keyword "quote"
    modules <- many1 stringLiteral
    return $ QuoteModule name modules

scriptModule :: Parser Module
scriptModule = binaryModule <|> quoteModule <|> (Base <$> wasmModule)

invoke :: Parser Action
invoke = parens' $ do
    keyword "invoke"
    moduleName <- maybeName
    functionName <- anyKeyword
    result <- many1 parseInstruction
    return $ Invoke moduleName functionName result

get :: Parser Action
get = parens' $ do
    keyword "get"
    moduleName <- maybeName
    name <- anyKeyword
    return $ Get moduleName name

action :: Parser Action
action = invoke <|> get

assertModule :: Parser Assert
assertModule = parens' $ do
    assertType <- anyKeyword
    module' <- scriptModule
    failureMessage <- stringLiteral
    case assertType of
        "assert_malformed" -> return $ AssertMalformed module' failureMessage
        "assert_invalid" -> return $ AssertInvalid module' failureMessage
        "assert_unlinkable" -> return $ AssertUnlinkable module' failureMessage
        "assert_trap" -> return $ AssertModuleTrap module' failureMessage
        _ -> fail "Invalid assert type"

assertAction :: Parser Assert
assertAction = parens' $ do
    assertType <- anyKeyword
    action' <- action
    case assertType of
        "assert_return" -> do
            args <- many1 parseInstruction
            return $ AssertReturn action' args
        "assert_return_canonical_nan" -> return $ AssertReturnCanonicalNaN action'
        "assert_return_arithmetic_nan" -> return $ AssertReturnArithmicNaN action'
        "assert_trap" -> do
            failureMessage <- stringLiteral
            return $ AssertTrap action' failureMessage
        "assert_exhaustion" -> do
            failureMessage <- stringLiteral
            return $ AssertExhaustion action' failureMessage
        _ -> fail "Invalid assert type"

assert :: Parser Assert
assert = assertAction <|> assertModule

register :: Parser Command
register = parens' $ do
    keyword "register"
    name <- stringLiteral
    moduleName <- maybeName
    return $ Register name moduleName

command :: Parser Command
command 
    = Module <$> try scriptModule
    <|> try register
    <|> Action <$> try action 
    <|> Assertion <$> try assert 
    <|> Meta <$> try meta

script :: Parser Script
script = many1 command

metaScript :: Parser Meta
metaScript = parens' $ do
    keyword "script"
    name <- maybeName
    script' <- script
    return $ MScript name script'

input :: Parser Meta
input = parens' $ do
    keyword "input"
    name <- maybeName
    filename <- stringLiteral
    return $ Input name filename

-- if no filename  is given, output to stdout
output :: Parser Meta
output = parens' $ do
    keyword "output"
    name <- maybeName
    filename <- optionMaybe stringLiteral
    return $ Output name filename

meta :: Parser Meta
meta = metaScript <|> input <|> output
