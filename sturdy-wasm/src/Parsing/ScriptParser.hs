module Parsing.ScriptParser where

import Text.ParserCombinators.Parsec

import Data.Char
import Data.Text
import qualified Data.Text as T

import Syntax
import Types hiding ((<|>))
import Parsing.Parser
import Parsing.Lexer

-- Module to parse WebAssembly Script syntax

type Name = Maybe String
type Failure = String

data Command
    = Module Module
    | Register String Name
    | Action Action
    | Assert Assert
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

newtype Script = Script [Command] deriving (Show, Eq)

binaryModule :: Parser Module
binaryModule = parens $ do
    keyword "module"
    name <- optionMaybe identifier
    keyword "binary"
    modules <- many stringLiteral
    return $ BinaryModule name modules

quoteModule :: Parser Module
quoteModule = parens $ do
    keyword "module"
    name <- optionMaybe identifier
    keyword "quote"
    modules <- many stringLiteral
    return $ QuoteModule name modules

scriptModule :: Parser Module
scriptModule = binaryModule <|> quoteModule <|> (try wasmModule >>= pure . Base)

invoke :: Parser Action
invoke = parens $ do
    keyword "invoke"
    moduleName <- optionMaybe identifier
    functionName <- anyKeyword
    instr <- many parseInstruction
    return $ Invoke moduleName functionName instr
