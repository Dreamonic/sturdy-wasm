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
manyInstr :: Parser [Instr]
manyInstr = do
  instructions <- foldedInstr <|> many instr
  if instructions == mzero then return mzero else do
    rest <- manyInstr <|> return []
    return $ instructions ++ rest

instr :: Parser Instr
instr
  = plainInstr
  <|> blockInstr

blockInstr :: Parser Instr
blockInstr 
  = parseBlockInstr
  <|> parseLoopInstr
  <|> parseIf

plainInstr :: Parser Instr
plainInstr  
  = parseBranch
  <|> parseGetLocal
  <|> parseSetLocal
  <|> parseTeeLocal
  <|> parseCall
  <|> parseConst
  <|> parseBinaryInstr
  <|> parseUnaryInstr
  <|> parseCompareInstr
  <|> parseNopInstr

foldedInstr :: Parser [Instr]
foldedInstr 
  = try parseFoldedIf
  <|> parseFoldedBlockInstr
  <|> parseFoldedLoopInstr
  <|> parseFoldedInstr

-- Parse instructions that are folded into an S-expression
parseFoldedInstr :: Parser [Instr]
parseFoldedInstr = parens $ do
  instruction <- plainInstr
  operands <- many foldedInstr
  return $ concat operands ++ [instruction]

parseFoldedBlockInstr :: Parser [Instr]
parseFoldedBlockInstr = try $ do
  instr' <- parens parseBlockInstr
  return [instr']

parseFoldedLoopInstr :: Parser [Instr]
parseFoldedLoopInstr = try $ do
  instr' <- parens parseLoopInstr
  return [instr']

parseBlockInstr :: Parser Instr
parseBlockInstr = do
  keyword "block"
  t <- many $ parseResultType
  instr' <- manyInstr
  keyword "end"
  return $ Block t instr'

parseLoopInstr :: Parser Instr
parseLoopInstr = do
  keyword "loop"
  instr' <- manyInstr
  keyword "end"
  return $ Loop [] instr'

parseBranch :: Parser Instr
parseBranch =
  (keyword "br" >> return (Br . fromIntegral) <*> integer)
  <|> (keyword "br_if" >> return (BrIf . fromIntegral) <*> integer)

parseIf :: Parser Instr
parseIf = do
  keyword "if"
  _ <- option "" identifier -- ignore label
  t <- many $ parseResultType
  instrT <- manyInstr
  instrF <- option [] $ do 
    keyword "else"
    manyInstr
  keyword "end"
  return $ If t instrT instrF

parseFoldedIf :: Parser [Instr]
parseFoldedIf = parens $ do
  keyword "if"
  _ <- option "" identifier -- ignore label
  t <- many $ try parseResultType
  instrs <- foldedInstr
  instrT <- parens $ do
    keyword "then"
    manyInstr
  instrF <- option [] $ parens $ do
    keyword "else"
    manyInstr
  return $ instrs ++ [If t instrT instrF]

-- Allow both spec and WASM studio style variable instructions
parseGetLocal :: Parser Instr
parseGetLocal = do
  (keyword "get_local") <|> (keyword "local.get")
  return LocalGet <*> identifier

parseSetLocal :: Parser Instr
parseSetLocal = do
  (keyword "set_local") <|> (keyword "get_local")
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

parseNopInstr :: Parser Instr
parseNopInstr = try $ do
  keyword "nop"
  return Nop

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
  instr'  <- manyInstr
  return $ Func idstr params resultTypes instr'

-- TODO: currently only handles function definitions, not exports etc
wasmModule :: Parser WasmModule
wasmModule = parens $ do
  keyword "module"
  functions <- many function
  return (WasmModule functions)

parseWasm :: Parser a -> String -> a
parseWasm func str = case parse func "wasm lang" str of
  Left err  -> error $ "No match: " ++ show err
  Right val -> val
