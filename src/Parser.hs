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
) where

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
  | Div WasmType SignedNess
  | Rem WasmType SignedNess
  | And WasmType
  | Or WasmType
  | Xor WasmType
  | Abs WasmType
  | Neg WasmType
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
