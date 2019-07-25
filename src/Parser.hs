module Parser(
  WasmType(..)
  , WasmModule(..)
  , Func(..)
  , Instr(..)
  , BinOpInstr(..)
  , UnOpInstr(..)
  , RelOpInstr(..)
  , WasmVal(..)
  , Param(..)
  , Result(..)
  , getResult
  , SignedNess(..)
  , parse
  , function
  , parseWasm
  , wasmModule
) where

import Text.ParserCombinators.Parsec

import           Control.Monad
import           Lexer

import Data.List
import WasmTypes(WasmType(..), WasmVal(..))

data Instr
  = Const WasmVal
  | Block [WasmType] [Instr]
  | Br Integer
  | BrIf Integer
  | If [WasmType] [Instr] [Instr]
  | Loop [WasmType] [Instr]
  | Call String
  | LocalGet String
  | LocalSet String
  | LocalTee String
  | Binary WasmType BinOpInstr
  | Unary WasmType UnOpInstr
  | Compare WasmType RelOpInstr
  | Nop
  | Unreachable
  deriving (Show, Eq)

data BinOpInstr
  = Add
  | And
  | Sub
  | Mul
  | Div SignedNess
  | Or
  | Xor deriving (Show, Eq)

data UnOpInstr
  = Neg
  | Abs deriving (Show, Eq)

data RelOpInstr
  = Eql deriving (Show, Eq)

data Param = Param {getName :: String, getValue :: WasmType} deriving (Show, Eq)
data Result = Result WasmType deriving (Show, Eq)
data SignedNess = Signed | Unsigned deriving (Show, Eq)
data Func = Func String [Param] [Result] [Instr] deriving (Show, Eq)
data WasmModule = WasmModule [Func] deriving (Show, Eq)

getResult :: Result -> WasmType
getResult (Result t) = t

isInt :: WasmType -> Bool
isInt I32 = True
isInt I64 = True
isInt _   = False

isFloat :: WasmType -> Bool
isFloat F32 = True
isFloat F64 = True
isFloat _   = False

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
  instr <- parseBody
  keyword "end"
  return $ Block [] instr

parseLoop :: Parser Instr
parseLoop = do
  keyword "loop"
  instr <- parseBody
  keyword "end"
  return $ Loop [] instr

parseBranch :: Parser Instr
parseBranch =
  (keyword "br" >> return Br <*> integer)
  <|> (keyword "br_if" >> return BrIf <*> integer)

parseIf :: Parser Instr
parseIf = do
  keyword "if"
  t <- many $ parseResultType
  instrT <- many $ parseInstruction
  keyword "else"
  instrF <- many $ parseInstruction
  keyword "end"
  return $ If (fmap (\(Result x) -> x) t) instrT instrF

parseGetLocal :: Parser Instr
parseGetLocal = do
  keyword "get_local"
  return LocalGet <*> identifier

parseSetLocal :: Parser Instr
parseSetLocal = do
  keyword "set_local"
  return LocalSet <*> identifier

parseConst :: Parser Instr
parseConst = try $ do
  t <- parseType
  _ <- dot
  _ <- keyword "const"
  case t of
        F32 -> Parser.Const . F32Val <$> float
        F64 -> Parser.Const . F64Val <$> float
        I32 -> Parser.Const . I32Val <$> integer
        I64 -> Parser.Const . I64Val <$> integer

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

parseResultType :: Parser Result
parseResultType = parens $ do
  keyword "result"
  typ   <- parseType
  return (Result typ)


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
