module Parser(
  WasmType(..)
  , WasmModule(..)
  , Func(..)
  , Instr(..)
  , TypedInstr(..)
  , WasmVal(..)
  , Block(..)
  , Param(..)
  , Result(..)
  , SignedNess(..)
  , ofType
  , getType
  , parse
  , toWasmI
  , toWasmF
  , function
  , parseFunc
) where

import Text.ParserCombinators.Parsec

import           Control.Monad
import           Lexer

import Data.List


data WasmType
  = I32
  | I64
  | F32
  | F64
  deriving (Show, Eq)

data Instr
  = EnterBlock Block
  | Branch Integer
  | If Instr
  | Loop Instr
  | Call String
  | LocalGet String
  | LocalSet String
  | LocalTee String
  | Numeric TypedInstr
  | Nop
  deriving (Show, Eq)

data TypedInstr
  = Const WasmVal
  | Add WasmType
  | Sub WasmType
  | Mul WasmType
  | Div WasmType SignedNess   -- TODO: keep it like this, or introduce seperate Div instructions (one for floats, and signed and unsigned)
  | Rem WasmType SignedNess
  | And WasmType
  | Or WasmType
  | Xor WasmType
  | Abs WasmType
  | Neg WasmType
  | Popcnt WasmType
  deriving (Show, Eq)

data WasmVal
  = I32Val Integer
  | I64Val Integer
  | F32Val Double
  | F64Val Double
  deriving (Show, Eq)

getType :: WasmVal -> WasmType
getType typ = case typ of
  (I32Val _) -> I32
  (I64Val _) -> I64
  (F32Val _) -> F32
  (F64Val _) -> F64

ofType :: WasmVal -> WasmType -> Bool
ofType val typ = (getType val) == typ

toWasmI :: WasmType -> Integer -> WasmVal
toWasmI typ x = case typ of
  I32 -> I32Val x
  I64 -> I64Val x
  _   -> toWasmF typ (fromIntegral x)

toWasmF :: WasmType -> Double -> WasmVal
toWasmF typ x = case typ of
  F32 -> F32Val x
  F64 -> F64Val x
  _   -> toWasmI typ (round x)

data Param = Param String WasmType deriving (Show, Eq)
data Result = Result WasmType deriving (Show, Eq)
data SignedNess = Signed | Unsigned deriving (Show, Eq)
data Block = Block [Result] [Instr] deriving (Show, Eq)
data Func = Func String [Param] Block deriving (Show, Eq)
data WasmModule = WasmModule [Func] deriving (Show, Eq)

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

parseInstruction = parseGetLocal <|> parseNumericInstr <|> parens parseInstruction

-- Parse instructions that are folded into an S-expression
parseFolded :: Parser [Instr]
parseFolded = parens $ do
  instruction <- parseInstruction
  operands <- many parseFolded
  return $ concat operands ++ [instruction]


parseGetLocal :: Parser Instr
parseGetLocal = do
  keyword "get_local"
  return LocalGet <*> identifier

parseConst :: Parser Instr
parseConst = do
  t <- parseType
  _ <- dot
  _ <- keyword "const"
  case t of
        F32 -> Numeric . Parser.Const . F32Val <$> float
        F64 -> Numeric . Parser.Const . F64Val <$> float
        I32 -> Numeric . Parser.Const . I32Val <$> integer
        I64 -> Numeric . Parser.Const . I64Val <$> integer

parseNumericInstr :: Parser Instr
parseNumericInstr = try parseConst <|> do
  t <- parseType
  _ <- dot
  i <- anyKeyword
  typedInstr <- case t of
    t | isInt t   -> makeII i t
    t | isFloat t -> makeFI i t
  return (Numeric typedInstr)

-- makeIntegerInstruction
makeII :: String -> WasmType -> Parser TypedInstr
makeII "add" t   = return $ Add t
makeII "sub" t   = return $ Sub t
makeII "mul" t   = return $ Mul t
makeII "div_s" t = return $ Div t Signed
makeII "div_u" t = return $ Div t Unsigned
makeII "rem_s" t = return $ Rem t Signed
makeII "rem_u" t = return $ Rem t Unsigned
makeII "and" t   = return $ And t
makeII "or"  t   = return $ Or t
makeII "xor" t   = return $ Xor t
makeII "popcnt" t = return $ Popcnt t

makeII str   t   = error $ "Not a valid instruction: " ++ str ++ " with type: " ++ show t

-- makeFloatInstruction
makeFI :: String -> WasmType -> Parser TypedInstr
makeFI "add" t = return $ Add t
makeFI "sub" t = return $ Sub t
makeFI "mul" t = return $ Mul t
makeFI "div" t = return $ Div t Signed
makeFI "abs" t = return $ Abs t
makeFI "neg" t = return $ Neg t
makeFI str   t = error $ "Not a valid instruction: " ++ str ++ " with type: " ++ show t

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
  return $ Func idstr params $ Block resultTypes instr


-- TODO: currently only handles function definitions, not exports etc
parseModule :: Parser WasmModule
parseModule = parens $ do
  keyword "module"
  functions <- many function
  return (WasmModule functions)

watfunc = "(func $add (param $lhs i32) (param $rhs i32) (result i32) get_local $lhs get_local $rhs i32.add)"

parseFunc :: Parser a -> String -> a
parseFunc func str = case parse func "wasm lang" str of
  Left err  -> error $ "No match: " ++ show err
  Right val -> val

parseFuncStr2show str = case parse function "wasm lang" str of
  Left err  -> "No match: " ++ show err
  Right val -> show val