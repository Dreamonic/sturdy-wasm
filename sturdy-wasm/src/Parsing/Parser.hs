module Parsing.Parser
  ( parse
  , function
  , parseWasm
  , wasmModule
  ) where

import Data.List

import Text.ParserCombinators.Parsec

import Control.Monad
import Parsing.Lexer
import Types(WasmType(..), WasmVal(..), isInt, isFloat)
import Syntax

-- Parse a function body consisting of a mix of plain and foldedinstructions
parseBody :: Parser [Instr]
parseBody = do
  instructions <- parseFolded <|> many parseInstruction
  if instructions == mzero then return mzero else do
    rest <- parseBody <|> return []
    return $ instructions ++ rest

parseInstruction
  = parseBlock
  <|> parseLoop
  <|> parseBranch
  <|> parseIf
  <|> parseGetLocal
  <|> parseSetLocal
  <|> parseTeeLocal
  <|> parseCall
  <|> parseConst
  <|> parseBinaryInstr
  <|> parseUnaryInstr
  <|> parseCompareInstr
  <|> parens parseInstruction

-- Parse instructions that are folded into an S-expression
parseFolded :: Parser [Instr]
parseFolded = parens $ do
  instruction <- parseInstruction
  operands <- many parseFolded
  return $ concat operands ++ [instruction]

parseBlock :: Parser Instr
parseBlock = do
  keyword "block"
  t <- many $ parseResultType
  instr <- parseBody
  keyword "end"
  return $ Block t instr

parseLoop :: Parser Instr
parseLoop = do
  keyword "loop"
  instr <- parseBody
  keyword "end"
  return $ Loop [] instr

parseBranch :: Parser Instr
parseBranch =
  (keyword "br" >> return (Br . fromIntegral) <*> integer)
  <|> (keyword "br_if" >> return (BrIf . fromIntegral) <*> integer)

parseIf :: Parser Instr
parseIf = do
  keyword "if"
  t <- many $ parseResultType
  instrT <- many $ parseInstruction
  keyword "else"
  instrF <- many $ parseInstruction
  keyword "end"
  return $ If t instrT instrF

parseGetLocal :: Parser Instr
parseGetLocal = do
  keyword "get_local"
  return LocalGet <*> identifier

parseSetLocal :: Parser Instr
parseSetLocal = do
  keyword "set_local"
  return LocalSet <*> identifier

parseTeeLocal :: Parser Instr
parseTeeLocal = do
  keyword "tee_local"
  return LocalTee <*> identifier

parseConst :: Parser Instr
parseConst = try $ do
  t <- parseType
  _ <- dot
  _ <- keyword "const"
  case t of
        F32 -> Syntax.Const . F32Val <$> float
        F64 -> Syntax.Const . F64Val <$> float
        I32 -> Syntax.Const . I32Val <$> integer
        I64 -> Syntax.Const . I64Val <$> integer

parseCall :: Parser Instr
parseCall = do
    keyword "call"
    return Call <*> identifier

parseBinaryInstr :: Parser Instr
parseBinaryInstr = try $ do
  t <- parseType
  _ <- dot
  i <- anyKeyword
  typedInstr <- case t of
    t | isInt t   -> makeBinII i
    t | isFloat t -> makeBinFI i
  return (Binary t typedInstr)

parseUnaryInstr :: Parser Instr
parseUnaryInstr = try $ do
  t <- parseType
  _ <- dot
  i <- anyKeyword
  typedInstr <- case t of
    t | isInt t   -> makeUnII i
    t | isFloat t -> makeUnFI i
  return (Unary t typedInstr)

parseCompareInstr :: Parser Instr
parseCompareInstr = try $ do
  t <- parseType
  _ <- dot
  i <- keyword "eq"
  return (Compare t Eql)


-- make Integer Binary Instr
makeBinII :: String -> Parser BinOpInstr
makeBinII "add"   = return Add
makeBinII "sub"   = return Sub
makeBinII "mul"   = return Mul
makeBinII "div_s" = return $ Div Signed
makeBinII "div_u" = return $ Div Unsigned
makeBinII "and"   = return And
makeBinII "or"    = return Or
makeBinII "xor"   = return Xor
makeBinII str     = fail $ "Not a valid binary integer instruction: " ++ str

-- make Float Binary Instr
makeBinFI :: String -> Parser BinOpInstr
makeBinFI "add" = return Add
makeBinFI "sub" = return Sub
makeBinFI "mul" = return Mul
makeBinFI "div" = return $ Div Signed
makeBinFI str   = fail $ "Not a valid binary float instruction: " ++ str

makeUnII :: String -> Parser UnOpInstr
makeUnII str = fail $ "Not a valid unary integer instruction: " ++ str

makeUnFI :: String -> Parser UnOpInstr
makeUnFI "neg" = return Neg
makeUnFI "abs" = return Abs
makeUnFI str   = fail $ "Not a valid unary float instruction: " ++ str

parseType :: Parser WasmType
parseType =  (keyword "i32" >> return I32)
          <|> (keyword "i64" >> return I64)
          <|> (keyword "f32" >> return F32)
          <|> (keyword "f64" >> return F64)
          <|> mzero

param :: Parser Param
param = parens $ do
  keyword "param"
  idstr <- identifier
  typ   <- parseType
  return (Param idstr typ)

parseResultType :: Parser WasmType
parseResultType = parens $ do
  keyword "result"
  typ   <- parseType
  return typ


function :: Parser Func
function = parens $ do
  keyword "func"
  idstr <- identifier
  params <- many $ try param
  resultTypes <- many $ try parseResultType
  instr  <- parseBody
  return $ Func idstr params resultTypes instr


-- TODO: currently only handles function definitions, not exports etc
wasmModule :: Parser WasmModule
wasmModule = parens $ do
  keyword "module"
  functions <- many function
  return (WasmModule functions)

watfunc = "(func $add (param $lhs i32) (param $rhs i32) (result i32) get_local $lhs get_local $rhs i32.add)"

parseWasm :: Parser a -> String -> a
parseWasm func str = case parse func "wasm lang" str of
  Left err  -> error $ "No match: " ++ show err
  Right val -> val
